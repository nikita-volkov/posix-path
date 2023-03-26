module Coalmine.ScientificExtras where

import Coalmine.InternalPrelude
import Data.Scientific

scaleToDecimalsClipping :: Int -> Scientific -> Integer
scaleToDecimalsClipping decimals a =
  if decimalsDiff >= 0
    then coefficient a * 10 ^ decimalsDiff
    else div (coefficient a) (10 ^ negate decimalsDiff)
  where
    decimalsDiff = decimals + base10Exponent a

-- | Fails when the resolution is not enough.
scaleToDecimals :: Int -> Scientific -> Maybe Integer
scaleToDecimals decimals a =
  if decimalsDiff >= 0
    then Just $ coefficient a * 10 ^ decimalsDiff
    else Nothing
  where
    decimalsDiff = decimals + base10Exponent a
