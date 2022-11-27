module Coalmine.Comms.IntegerMath where

import Coalmine.InternalPrelude

{-# INLINE byteSize #-}
byteSize :: (Integral a, Bits a) => a -> Int
byteSize =
  processValue 0
  where
    processValue !i = \case
      0 -> i
      val -> processValue (succ i) (unsafeShiftR val 8)
