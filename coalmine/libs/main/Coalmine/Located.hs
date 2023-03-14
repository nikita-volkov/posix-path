module Coalmine.Located
  ( -- * --
    Located (..),
    analyse,
    renderInMegaparsecStyle,
  )
where

import Coalmine.InternalPrelude
import Coalmine.Located.Rendering qualified as Rendering

-- * --

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
  Located _ _ lVal <*> Located rStart rEnd rVal =
    Located rStart rEnd (lVal rVal)

instance Monad Located where
  return =
    pure
  Located _ _ lVal >>= k =
    k lVal

instance Comonad Located where
  extract (Located _ _ a) =
    a
  duplicate (Located a b c) =
    Located a b (Located a b c)

-- |
-- Process using a provided pure refinement function.
analyse :: Located a -> (a -> Either e b) -> Either (Located e) b
analyse (Located a b c) mapper =
  first (Located a b) (mapper c)

-- |
-- Pretty-print an error message, asssociating it with the input,
-- Megaparsec-style.
renderInMegaparsecStyle :: Located Text -> Text -> Text
renderInMegaparsecStyle (Located start end message) input =
  Rendering.render input start end message
