module Coalmine.EvenSimplerPathsClasses where

import Coalmine.EvenSimplerPaths
import Coalmine.InternalPrelude

class GeneralizesToPath a where
  generalize :: a -> Path
  specialize :: Path -> Maybe a
