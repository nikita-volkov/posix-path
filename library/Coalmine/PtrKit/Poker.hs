module Coalmine.PtrKit.Poker
  ( Poker,
    toByteString,
    toByteStringList,
    toLazyByteString,
    streamThruBuffer,

    -- * Errors
    ImmediatePoker.ByteStringErr (..),
  )
where

import Coalmine.InternalPrelude
import Coalmine.PtrKit.ImmediatePoker qualified as ImmediatePoker
import Coalmine.PtrKit.StreamingPoker qualified as StreamingPoker

-- |
-- Universal poker, which can be used to both effeciently construct
-- immediate applications and streaming.
--
-- Implemented as a product of 'ImmediatePoker.ImmediatePoker'
-- and 'StreamingPoker.StreamingPoker'.
data Poker = Poker
  { streaming :: ~StreamingPoker.StreamingPoker,
    immediate :: ~ImmediatePoker.ImmediatePoker
  }

instance Semigroup Poker where
  Poker streamingL immediateL <> Poker streamingR immediateR =
    Poker (streamingL <> streamingR) (immediateL <> immediateR)

instance Monoid Poker where
  mempty = Poker mempty mempty

toByteString :: Poker -> Either ImmediatePoker.ByteStringErr ByteString
toByteString =
  ImmediatePoker.toByteString . (.immediate)

toByteStringList :: Poker -> [ByteString]
toByteStringList =
  error "TODO"

toLazyByteString :: Poker -> LazyByteString
toLazyByteString =
  StreamingPoker.toLazyByteStringOfDefaultChunkSize . (.streaming)

-- | Evaluate the poker by filling up a reusable buffer and repeatedly calling
-- a continuation on it. Buffer allocation is encapsulated.
--
-- This is what you should use for integrating with sockets or file system.
streamThruBuffer ::
  Poker ->
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

varLengthInteger :: Integer -> Poker
varLengthInteger =
  error "TODO"
