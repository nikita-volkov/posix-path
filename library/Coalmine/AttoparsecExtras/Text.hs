module Coalmine.AttoparsecExtras.Text where

import qualified Coalmine.BaseExtras.Integer as IntegerExtras
import Coalmine.Prelude
import Data.Attoparsec.Text

validated :: (a -> Maybe String) -> Parser a -> Parser a
validated validator parser =
  parser >>= \a -> maybe (return a) fail (validator a)

nonGreedyUnsignedDecimal :: (Integral a, Show a) => a -> a -> Parser a
nonGreedyUnsignedDecimal min max =
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
                else
                  fail $
                    mconcat
                      [ "Decimal is smaller than the expected minimum of ",
                        show min,
                        ": ",
                        show acc
                      ]
            else
              fail $
                mconcat
                  [ "Decimal is larger than the expected maximum of ",
                    show max,
                    ": ",
                    show acc
                  ]
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

greedyUnsignedDecimal :: (Integral a, Show a) => a -> a -> Parser a
greedyUnsignedDecimal min max =
  runScanner (0, 0) step >>= postCheck . snd
  where
    step (i, acc) char =
      if i <= maxLen && isDigit char
        then case acc * 10 + fromIntegral (ord char - 48) of
          acc -> Just (succ i, acc)
        else Nothing
    postCheck (i, acc) =
      if i <= maxLen
        then
          if i >= minLen
            then
              if acc <= max
                then
                  if acc >= min
                    then return acc
                    else
                      fail $
                        mconcat
                          [ "Decimal is smaller than the expected minimum of ",
                            show min,
                            ": ",
                            show acc
                          ]
                else
                  fail $
                    mconcat
                      [ "Decimal is larger than the expected maximum of ",
                        show max,
                        ": ",
                        show acc
                      ]
            else
              fail $
                mconcat
                  [ "Decimal is shorter than the expected minimum length of ",
                    show minLen,
                    ". It is ",
                    show i,
                    " characters long"
                  ]
        else
          fail $
            mconcat
              [ "Decimal is longer than the expected maximum length of ",
                show maxLen,
                ". It is at least ",
                show i,
                " characters long"
              ]
    maxLen =
      IntegerExtras.countDigits max
    minLen =
      IntegerExtras.countDigits max

-- |
-- >>> parseOnly (unboundedFixedLengthUnsignedDecimal 4) "2003"
-- Right 2003
--
-- >>> parseOnly (unboundedFixedLengthUnsignedDecimal 4) "20030"
-- Right 2003
--
-- >>> parseOnly (unboundedFixedLengthUnsignedDecimal 4) "0034"
-- Right 34
--
-- >>> parseOnly (unboundedFixedLengthUnsignedDecimal 4) "003"
-- Left "Failed reading: Decimal is shorter than the expected length of 4. It is 3 characters long"
--
-- >>> parseOnly (unboundedFixedLengthUnsignedDecimal 4) "OO34"
-- Left "Failed reading: Decimal is shorter than the expected length of 4. It is 0 characters long"
unboundedFixedLengthUnsignedDecimal :: Integral a => Int -> Parser a
unboundedFixedLengthUnsignedDecimal length =
  runScanner (0, 0) step >>= postCheck . snd
  where
    step (i, acc) char =
      if i < length && isDigit char
        then case acc * 10 + fromIntegral (ord char - 48) of
          acc -> Just (succ i, acc)
        else Nothing
    postCheck (i, acc) =
      if i == length
        then return acc
        else
          fail $
            mconcat
              [ "Decimal is shorter than the expected length of ",
                show length,
                ". It is ",
                show i,
                " characters long"
              ]

boundedFixedLengthUnsignedDecimal :: (Integral a, Show a) => Int -> a -> a -> Parser a
boundedFixedLengthUnsignedDecimal length min max =
  unboundedFixedLengthUnsignedDecimal length >>= \a ->
    if a < min
      then
        fail $
          mconcat
            [ "Decimal is smaller than the expected minimum of ",
              show min,
              ": ",
              show a
            ]
      else
        if a > max
          then
            fail $
              mconcat
                [ "Decimal is larger than the expected maximum of ",
                  show max,
                  ": ",
                  show a
                ]
          else return a
