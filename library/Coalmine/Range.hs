module Coalmine.Range
  ( -- *
    Range (..),

    -- **
    isEmpty,
    contains,
  )
where

import Coalmine.Prelude

-- *

data Range a
  = Range
      !a
      -- ^ Equal or larger than.
      !a
      -- ^ Smaller than.

instance Ord a => Semigroup (Range a) where
  Range lMin lMax <> Range rMin rMax =
    Range (max lMin rMin) (min lMax rMax)

instance (Bounded a, Ord a) => Monoid (Range a) where
  mempty =
    Range minBound maxBound

-- *

-- |
-- Checks whether the range includes any elements at all.
isEmpty :: Ord a => Range a -> Bool
isEmpty (Range min max) =
  min >= max

contains :: Ord a => a -> Range a -> Bool
contains a (Range min max) =
  a >= min && a < max
