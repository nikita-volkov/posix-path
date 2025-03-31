module PosixPath.Classes where

import PosixPath
import PosixPath.BaseExtras.Prelude

class ModelsSomePaths a where
  demodel :: a -> Path
  model :: Path -> Maybe a
