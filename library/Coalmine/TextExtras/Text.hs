module Coalmine.TextExtras.Text
  ( NaturalSortKey,
    toNaturalSortKey,
  )
where

import qualified Algorithms.NaturalSort as NaturalSort
import Coalmine.InternalPrelude hiding (init)

-- * Natural sorting

newtype NaturalSortKey
  = NaturalSortKey NaturalSort.SortKey
  deriving (Eq, Ord, Show)

-- | Projection function.
-- Useful in combination with 'sortWith' kind of functions.
toNaturalSortKey :: Text -> NaturalSortKey
toNaturalSortKey =
  NaturalSortKey . NaturalSort.sortKey
