module Coalmine.HashSet where

import Prelude hiding (insert, empty)
import Data.HashSet


fromFoldable :: (Foldable f, Eq a, Hashable a) => f a -> HashSet a
fromFoldable =
  Prelude.foldl' (flip insert) empty
