module Coalmine.Optics where

import Coalmine.InternalPrelude
import qualified MooreMachines as Mm
import qualified Data.HashSet as HashSet


feedingMooreOf :: Fold a b -> Moore b c -> a -> Moore b c
feedingMooreOf optic =
  foldlOf' optic step
  where
    step moore b =
      Mm.feeding b moore

hashableUnique :: (Eq a, Hashable a) => Fold a a
hashableUnique =
  folding HashSet.singleton
