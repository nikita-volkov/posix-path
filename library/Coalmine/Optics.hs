module Coalmine.Optics where

import Coalmine.InternalPrelude
import qualified MooreMachines as Mm


feedingMooreOf :: Fold a b -> Moore b c -> a -> Moore b c
feedingMooreOf optic =
  foldlOf' optic step
  where
    step moore b =
      Mm.feeding b moore
