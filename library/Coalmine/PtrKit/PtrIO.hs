-- |
-- IO actions on a pointer conforming to Handle pattern.
module Coalmine.PtrKit.PtrIO where

import Coalmine.InternalPrelude

-- * Backward poking

-- |
-- Poke a list of bytes in backward direction.
backPokeByteRevListWithHead ::
  -- | Ptr to write the last byte to.
  -- The remainder byte list will be written in backward direction
  -- to preceding pointers.
  Ptr Word8 ->
  Word8 ->
  [Word8] ->
  IO ()
backPokeByteRevListWithHead ptr value tail = do
  poke ptr value
  case tail of
    nextValue : nextTail -> backPokeByteRevListWithHead (plusPtr ptr (-1)) nextValue nextTail
    [] -> return ()
