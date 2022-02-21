module Coalmine.Located
  ( -- *
    Located (..),
    analyse,
  )
where

import Coalmine.InternalPrelude
import Text.Megaparsec

-- *

data Located a
  = Located
      !Int
      -- ^ Start offset.
      !Int
      -- ^ End offset.
      a
  deriving (Functor, Show, Eq, Foldable, Traversable)

instance Applicative Located where
  pure =
    Located 0 0
  Located lStart lEnd lVal <*> Located rStart rEnd rVal =
    Located (min lStart rStart) (max lEnd rEnd) (lVal rVal)

instance Monad Located where
  return =
    pure
  Located lStart lEnd lVal >>= k =
    case k lVal of
      Located rStart rEnd rVal ->
        Located (min lStart rStart) (max lEnd rEnd) rVal

-- |
-- Process using a provided pure refinement function.
analyse :: Located a -> (a -> Either e b) -> Either (Located e) b
analyse (Located a b c) mapper =
  first (Located a b) (mapper c)
