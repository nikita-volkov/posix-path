module PosixPath.Classes where

import Coalmine.Prelude hiding (Path)
import PosixPath

class ModelsSomePaths a where
  demodel :: a -> Path
  model :: Path -> Maybe a
