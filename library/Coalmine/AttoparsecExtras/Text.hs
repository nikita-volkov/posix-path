module Coalmine.AttoparsecExtras.Text
  ( -- *
    variableLengthUnsignedDecimal,
    unboundedFixedLengthUnsignedDecimal,
    boundedFixedLengthUnsignedDecimal,
  )
where

import qualified Coalmine.BaseExtras.Integer as IntegerExtras
import Coalmine.Prelude
import Data.Attoparsec.Text

-- *

variableLengthUnsignedDecimal :: (Integral a, Show a) => a -> a -> Parser a
variableLengthUnsignedDecimal min max =
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
-- >>> parseOnly (unboundedFixedLengthUnsignedDecimal 4 0) "2003"
-- Right 2003
--
-- >>> parseOnly (unboundedFixedLengthUnsignedDecimal 4 0) "20030"
-- Right 2003
--
-- >>> parseOnly (unboundedFixedLengthUnsignedDecimal 4 0) "0034"
-- Right 34
--
-- >>> parseOnly (unboundedFixedLengthUnsignedDecimal 4 0) "003"
-- Left "Failed reading: Decimal is shorter than the expected length of 4. It is 3 characters long"
--
-- >>> parseOnly (unboundedFixedLengthUnsignedDecimal 4 0) "OO34"
-- Left "Failed reading: Decimal is shorter than the expected length of 4. It is 0 characters long"
unboundedFixedLengthUnsignedDecimal :: (Integral a, Show a) => Int -> a -> Parser a
unboundedFixedLengthUnsignedDecimal length min =
  runScanner (0, 0) step >>= postCheck . snd
  where
    step (i, acc) char =
      if i < length && isDigit char
        then case acc * 10 + fromIntegral (ord char - 48) of
          acc -> Just (succ i, acc)
        else Nothing
    postCheck (i, acc) =
      if i == length
        then
          if acc < min
            then
              fail $
                mconcat
                  [ "Decimal is smaller than the expected minimum of ",
                    show min,
                    ": ",
                    show acc
                  ]
            else return acc
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
  unboundedFixedLengthUnsignedDecimal length min >>= \a ->
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

-- *

validated :: Validator a -> Parser a -> Parser a
validated (Validator validator) parser =
  parser >>= \a -> maybe (return a) fail (validator a)

-- *

newtype Validator a = Validator (a -> Maybe String)

instance Semigroup (Validator a) where
  Validator l <> Validator r =
    Validator $ \input -> case l input of
      Just err -> Just err
      Nothing -> r input

instance Monoid (Validator a) where
  mempty =
    Validator (const Nothing)

notSmallerThanValidator :: (Show a, Ord a) => a -> Validator a
notSmallerThanValidator min =
  Validator $ \a ->
    if a < min
      then
        Just . mconcat $
          [ "Decimal is smaller than the expected minimum of ",
            show min,
            ": ",
            show a
          ]
      else Nothing

notLargerThanValidator :: (Show a, Ord a) => a -> Validator a
notLargerThanValidator max =
  Validator $ \a ->
    if a > max
      then
        Just . mconcat $
          [ "Decimal is larger than the expected maximum of ",
            show max,
            ": ",
            show a
          ]
      else Nothing
