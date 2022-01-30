module Coalmine.AttoparsecExtras.Text where

import qualified Coalmine.BaseExtras.Integer as IntegerExtras
import Coalmine.Prelude
import Data.Attoparsec.Text

invulnerableUnsignedDecimal :: (Integral a, Show a) => a -> a -> Parser a
invulnerableUnsignedDecimal min max =
  runScanner (0, 0) step >>= postCheck . snd
  where
    step (i, acc) char =
      if i < maxLen && isDigit char
        then case acc * 10 + fromIntegral (ord char - 48) of
          acc -> Just (succ i, acc)
        else Nothing
    postCheck (i, acc) =
      if i >= minLen
        then
          if acc <= max
            then
              if acc >= min
                then return acc
                else fail $ "Decimal is smaller than the expected minimum of " <> show min <> ": " <> show acc
            else fail $ "Decimal is larger than the expected maximum of " <> show max <> ": " <> show acc
        else
          fail $
            mconcat
              [ "Decimal is shorter than the expected minimum length of ",
                show minLen,
                ". It is ",
                show i,
                " characters long"
              ]
    maxLen =
      IntegerExtras.countDigits max
    minLen =
      IntegerExtras.countDigits min
