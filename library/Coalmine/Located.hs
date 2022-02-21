module Coalmine.Located
  ( -- *
    Located (..),
    analyse,
  )
where

import Coalmine.InternalPrelude
import Text.Megaparsec

-- *

data Located s a
  = Located
      !(PosState s)
      !(PosState s)
      a
  deriving (Functor, Show, Eq, Foldable, Traversable)

-- |
-- Process using a provided pure refinement function.
analyse :: Located s a -> (a -> Either e b) -> Either (Located s e) b
analyse (Located a b c) mapper =
  first (Located a b) (mapper c)
