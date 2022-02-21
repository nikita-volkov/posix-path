module Coalmine.Located
  ( -- *
    Located (..),
    refine,
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

sequenceErr :: Located s (Either e a) -> Either (Located s e) a
sequenceErr (Located a b c) =
  first (Located a b) c

refine :: (a -> Either e b) -> Located s a -> Either (Located s e) b
refine mapper (Located a b c) =
  first (Located a b) (mapper c)
