module Coalmine.DecimalExtras where

import Coalmine.DecimalExtras.Instances ()
import Coalmine.InternalPrelude
import Data.Decimal

getMantissaOfDecimals :: Word8 -> Decimal -> Maybe Integer
getMantissaOfDecimals expectedDecimals (Decimal places mantissa) =
  if places == expectedDecimals
    then Just mantissa
    else Nothing

toFixed :: HasResolution res => Decimal -> Maybe (Fixed res)
toFixed (Decimal places mantissa) =
  let fixed = MkFixed mantissa
   in if resolution fixed == fromIntegral places
        then Just fixed
        else Nothing

toFixedE8 :: Decimal -> Maybe (Fixed E8)
toFixedE8 = coerce . getMantissaOfDecimals 8

toFixed8 :: Decimal -> Maybe (Fixed 8)
toFixed8 = coerce . getMantissaOfDecimals 8
