module Coalmine.PtrKit.Encoding
  ( Encoding,
    toByteString,
    toByteStringList,
    toLazyByteString,
    streamThruBuffer,
    varLengthUnsignedInteger,
  )
where

import Coalmine.InternalPrelude
import Coalmine.PtrKit.Streamer qualified as Streamer
import Coalmine.PtrKit.Writer qualified as Writer

-- |
-- Universal poker, which can be used to both effeciently construct
-- immediate applications and streaming.
--
-- Implemented as a product of 'Writer.Writer'
-- and 'Streamer.Streamer'.
data Encoding = Encoding
  { streaming :: ~Streamer.Streamer,
    immediate :: ~Writer.Writer
  }

instance Semigroup Encoding where
  Encoding streamingL immediateL <> Encoding streamingR immediateR =
    Encoding (streamingL <> streamingR) (immediateL <> immediateR)

instance Monoid Encoding where
  mempty = Encoding mempty mempty

toByteString :: Encoding -> ByteString
toByteString =
  Writer.toByteString . (.immediate)

toByteStringList :: Encoding -> [ByteString]
toByteStringList =
  error "TODO"

toLazyByteString :: Encoding -> LazyByteString
toLazyByteString =
  Streamer.toLazyByteStringOfDefaultChunkSize . (.streaming)

-- | Evaluate the poker by filling up a reusable buffer and repeatedly calling
-- a continuation on it. Buffer allocation is encapsulated.
--
-- This is what you should use for integrating with sockets or file system.
streamThruBuffer ::
  Encoding ->
  -- | Reused buffer size.
  Int ->
  -- | Action to be repeatedly executed when the buffer is filled.
  -- The params are the pointer to read from and the length of data in it.
  (Ptr Word8 -> Int -> IO ()) ->
  -- | Action producing error details if there is one.
  IO (Maybe Text)
streamThruBuffer poker =
  Streamer.streamThruBuffer poker.streaming

-- * Constructors

varLengthUnsignedInteger :: (Integral a, Bits a, Show a) => a -> Encoding
varLengthUnsignedInteger value =
  Encoding
    (Streamer.varLengthUnsignedInteger value)
    (Writer.varLengthUnsignedInteger value)
