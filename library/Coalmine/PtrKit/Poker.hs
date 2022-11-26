module Coalmine.PtrKit.Poker
  ( Poker,
    toByteString,
    toByteStringList,
    toLazyByteString,
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

toByteString :: Poker -> Either Text ByteString
toByteString =
  ImmediatePoker.toByteString . (.immediate)

toByteStringList :: Poker -> [ByteString]
toByteStringList =
  error "TODO"

toLazyByteString :: Poker -> LazyByteString
toLazyByteString =
  StreamingPoker.toLazyByteStringOfDefaultChunkSize . (.streaming)
