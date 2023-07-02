module Coalmine.Distribution where

import Coalmine.InternalPrelude

newtype Distribution = Distribution {scale :: Double -> Double}

applyToIntInRange :: Distribution -> Int -> Int -> Int -> Int
applyToIntInRange distribution min max val =
  let space = fromIntegral (max - min)
      normalizedVal = fromIntegral (val - min)
      progress = normalizedVal / space
      scaled = distribution.scale progress
   in min + round (space * scaled)

-- * Definitions

exponential :: Distribution
exponential =
  Distribution $ \x -> x * x
