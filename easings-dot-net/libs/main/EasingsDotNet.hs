-- |
-- Collection of easing functions as per the definition at the <easings.net> website
-- presented as a simple composable eDSL for multiple use-cases.
module EasingsDotNet where

import Prelude

newtype Easing = Easing {ease :: Double -> Double}

instance Semigroup Easing where
  (<>) a b = Easing (a.ease . b.ease)

instance Monoid Easing where
  mempty = Easing id

applyToIntInRange :: Easing -> Int -> Int -> Int -> Int
applyToIntInRange easing min max val =
  let space = fromIntegral (max - min)
      normalizedVal = fromIntegral (val - min)
      progress = normalizedVal / space
      scaled = easing.ease progress
   in min + floor (space * scaled)

applyToProperFraction :: Easing -> Double -> Double
applyToProperFraction easing = easing.ease

-- * Definitions

inTunable :: Double -> Easing
inTunable factor =
  Easing $ \x -> x ** factor

inQuad :: Easing
inQuad =
  Easing $ \x -> x * x

inOutCubic :: Easing
inOutCubic =
  Easing $ \x ->
    if x < 0.5
      then 4 * x * x * x
      else 1 - (((-2) * x + 2) ** 3) / 2
