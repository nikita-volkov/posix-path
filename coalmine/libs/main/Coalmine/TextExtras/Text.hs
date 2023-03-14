module Coalmine.TextExtras.Text
  ( NaturalSortKey,
    toNaturalSortKey,
    nonNull,
    mapNonNull,
  )
where

import Algorithms.NaturalSort qualified as NaturalSort
import Coalmine.InternalPrelude hiding (init, null)
import Data.Text hiding (empty)

-- * Natural sorting

newtype NaturalSortKey
  = NaturalSortKey NaturalSort.SortKey
  deriving (Eq, Ord, Show)

-- | Projection function.
-- Useful in combination with 'sortWith' kind of functions.
toNaturalSortKey :: Text -> NaturalSortKey
toNaturalSortKey =
  NaturalSortKey . NaturalSort.sortKey

-- * Basic extras

nonNull :: (Alternative f) => Text -> f Text
nonNull text =
  if null text
    then pure text
    else empty

mapNonNull :: (Alternative f) => (Text -> a) -> Text -> f a
mapNonNull fn text =
  if null text
    then pure (fn text)
    else empty
