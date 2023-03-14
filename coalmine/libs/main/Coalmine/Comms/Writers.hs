{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Coalmine.Comms.Writers where

import Coalmine.InternalPrelude hiding (Writer)
import Coalmine.PtrKit.PtrIO qualified as PtrIO
import Coalmine.PtrKit.Writer

-- |
-- Variable length representation of unsigned integers.
--
-- Uses the 8th bit of each octet to specify, whether another octet is needed.
--
-- __Warning:__
-- It is your responsibility to ensure that the value is non-negative,
-- otherwise the encoder will fall into an infinite loop.
varLengthUnsignedInteger :: (Integral a, Bits a) => a -> Writer
varLengthUnsignedInteger =
  -- A two-phase implementation:
  -- 1. Aggregate the size and metadata required for poking.
  -- 2. Use the metadata to optimize the poking action.
  processValue 0 []
  where
    processValue !offset !byteRevList value =
      case nextValue of
        0 ->
          processMetadata offset (fromIntegral value) byteRevList
        _ ->
          processValue (succ offset) (byte : byteRevList) nextValue
          where
            !byte = setBit (fromIntegral value) 7
      where
        nextValue = unsafeShiftR value 7

    processMetadata lastOffset head tail =
      Writer size poke
      where
        size = succ lastOffset
        poke ptr =
          PtrIO.backPokeByteRevListWithHead lastPtr head tail
            $> plusPtr ptr size
          where
            lastPtr = plusPtr ptr lastOffset

varLengthSignedInteger :: (Integral a, Bits a) => a -> Writer
varLengthSignedInteger =
  error "TODO"

constLengthInteger :: (Integral a, Bits a) => Int -> a -> Writer
constLengthInteger size =
  error "TODO"
