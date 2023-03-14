module Coalmine.BaseExtras.Integer where

import Coalmine.InternalPrelude

countDigits :: (Integral a) => a -> Int
countDigits = go 1 . abs
  where
    go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

{-# INLINE byteSize #-}
byteSize :: (Integral a, Bits a) => a -> Int
byteSize =
  processValue 0
  where
    processValue !i = \case
      0 -> i
      val -> processValue (succ i) (unsafeShiftR val 8)

{-# INLINE maxBitOffset #-}
maxBitOffset :: (Integral a, Bits a) => a -> Int
maxBitOffset =
  processValue 0
  where
    processValue !i val =
      case val of
        0 -> i
        _ -> processValue (succ i) (unsafeShiftR val 1)

{-# INLINE bytesNeededForBits #-}
bytesNeededForBits :: (Integral a) => a -> a
bytesNeededForBits bits =
  succ (div (pred bits) 8)
