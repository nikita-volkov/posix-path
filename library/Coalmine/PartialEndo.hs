module Coalmine.PartialEndo where

import Coalmine.Prelude

-- *

newtype PartialEndo a
  = PartialEndo (a -> Either (PartialEndoErr a) a)
  deriving (Semigroup)

-- **

data family PartialEndoErr a
