module PosixPath.Classes where

import PosixPath
import Prelude

class ModelsSomePaths a where
  demodel :: a -> Path
  model :: Path -> Maybe a
