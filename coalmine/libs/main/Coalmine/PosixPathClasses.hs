module Coalmine.PosixPathClasses where

import Coalmine.InternalPrelude
import Coalmine.PosixPath.NormalizedPath

class ModelsSomePaths a where
  demodel :: a -> NormalizedPath
  model :: NormalizedPath -> Maybe a
