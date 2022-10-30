module Coalmine.QuickCheckExtras.Gens where

import Coalmine.InternalPrelude
import Test.QuickCheck.Gen

filter :: (a -> Bool) -> Gen a -> Gen a
filter = flip suchThat

enumBounded :: (Enum a, Bounded a) => Gen a
enumBounded =
  chooseEnum (minBound, maxBound)
