module Coalmine.Located
  ( -- *
    Located (..),
    refine,
  )
where

import Coalmine.InternalPrelude
import Text.Megaparsec

-- *

data Located a
  = Located
      !(PosState Text)
      !(PosState Text)
      a
  deriving (Functor, Show, Eq, Foldable, Traversable)

sequenceErr :: Located (Either e a) -> Either (Located e) a
sequenceErr (Located a b c) =
  first (Located a b) c

refine :: (a -> Either e b) -> Located a -> Either (Located e) b
refine mapper (Located a b c) =
  first (Located a b) (mapper c)
