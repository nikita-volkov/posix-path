{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Coalmine.Comms.Streamers where

import Coalmine.InternalPrelude
import Coalmine.PtrKit.Streamer

-- |
-- Variable length representation of unsigned integers.
--
-- Uses the 8th bit of each octet to specify, whether another octet is needed.
--
-- __Warning:__
-- It is your responsibility to ensure that the value is non-negative,
-- otherwise the encoder will fall into an infinite loop.
varLengthUnsignedInteger :: (Integral a, Bits a) => a -> Streamer
varLengthUnsignedInteger =
  error "TODO"

varLengthSignedInteger :: (Integral a, Bits a) => a -> Streamer
varLengthSignedInteger =
  error "TODO"

constLengthInteger :: (Integral a, Bits a) => Int -> a -> Streamer
constLengthInteger size =
  error "TODO"
