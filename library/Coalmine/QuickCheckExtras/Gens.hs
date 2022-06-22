module Coalmine.QuickCheckExtras.Gens where

import Coalmine.InternalPrelude
import Test.QuickCheck.Gen

filter :: (a -> Bool) -> Gen a -> Gen a
filter = flip suchThat
