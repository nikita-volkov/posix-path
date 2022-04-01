module Coalmine.ScientificExtras where

import Coalmine.InternalPrelude
import Data.Scientific

scaleToDecimals :: Scientific -> Int -> Integer
scaleToDecimals a decimals =
  if decimalsDiff >= 0
    then coefficient a * fromIntegral (10 ^ decimalsDiff)
    else div (coefficient a) (fromIntegral (10 ^ negate decimalsDiff))
  where
    decimalsDiff = decimals + base10Exponent a
