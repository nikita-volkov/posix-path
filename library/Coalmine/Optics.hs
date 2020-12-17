module Coalmine.Optics where

import Coalmine.InternalPrelude
import qualified Coalmine.HashSet as HashSet
import qualified MooreMachines as Mm


feedingMooreOf :: Fold a b -> Moore b c -> a -> Moore b c
feedingMooreOf optic =
  foldlOf' optic step
  where
    step moore b =
      Mm.feeding b moore

{-|

>>> toListOf uniqueFolded [1,2,1,3,1,4,2]
[1,2,3,4]

-}
uniqueFolded :: (Eq a, Hashable a, Foldable f) => Fold (f a) a
uniqueFolded =
  folding HashSet.fromFoldable
