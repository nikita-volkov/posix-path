module Coalmine.ScientificExtras where

import Coalmine.InternalPrelude
import Data.Scientific

scaleToDecimalsClipping :: Int -> Scientific -> Integer
scaleToDecimalsClipping decimals =
  either id id . scaleToDecimals decimals

scaleToDecimalsIfFits :: Int -> Scientific -> Maybe Integer
scaleToDecimalsIfFits decimals =
  either (const Nothing) Just . scaleToDecimals decimals

-- | Fails when the resolution is not enough and
-- produces a clipped output in Left.
scaleToDecimals :: Int -> Scientific -> Either Integer Integer
scaleToDecimals decimals a =
  if decimalsDiff >= 0
    then Right $ coefficient a * 10 ^ decimalsDiff
    else Left $ div (coefficient a) (10 ^ negate decimalsDiff)
  where
    decimalsDiff = decimals + base10Exponent a
