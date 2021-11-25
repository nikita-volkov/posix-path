module Coalmine.HashSet where

import Data.HashSet
import Prelude hiding (empty, insert)

fromFoldable :: (Foldable f, Eq a, Hashable a) => f a -> HashSet a
fromFoldable =
  Prelude.foldl' (flip insert) empty
