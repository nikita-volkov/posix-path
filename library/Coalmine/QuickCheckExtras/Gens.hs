module Coalmine.QuickCheckExtras.Gens where

import Coalmine.InternalPrelude
import Test.QuickCheck.Gen

filter :: (a -> Bool) -> Gen a -> Gen a
filter pred gen = go
  where
    go = do
      a <- gen
      if pred a
        then return a
        else go
