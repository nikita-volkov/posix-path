module Coalmine.PtrKit.Encoding
  ( Encoding,
    toByteString,
    toByteStringList,
    toLazyByteString,
    streamThruBuffer,

    -- * Errors
    ValidatingWriter.Err (..),
  )
where

import Coalmine.InternalPrelude
import Coalmine.PtrKit.StreamingPoker qualified as StreamingPoker
import Coalmine.PtrKit.ValidatingWriter qualified as ValidatingWriter

-- |
-- Universal poker, which can be used to both effeciently construct
-- immediate applications and streaming.
--
-- Implemented as a product of 'ValidatingWriter.ValidatingWriter'
-- and 'StreamingPoker.StreamingPoker'.
data Encoding = Encoding
  { streaming :: ~StreamingPoker.StreamingPoker,
    immediate :: ~ValidatingWriter.ValidatingWriter
  }

instance Semigroup Encoding where
  Encoding streamingL immediateL <> Encoding streamingR immediateR =
    Encoding (streamingL <> streamingR) (immediateL <> immediateR)

instance Monoid Encoding where
  mempty = Encoding mempty mempty

toByteString :: Encoding -> Either ValidatingWriter.Err ByteString
toByteString =
  ValidatingWriter.toByteString . (.immediate)

toByteStringList :: Encoding -> [ByteString]
toByteStringList =
  error "TODO"

toLazyByteString :: Encoding -> LazyByteString
toLazyByteString =
  StreamingPoker.toLazyByteStringOfDefaultChunkSize . (.streaming)

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
  StreamingPoker.streamThruBuffer poker.streaming

-- * Constructors

varLengthInteger :: Integer -> Encoding
varLengthInteger =
  error "TODO"
