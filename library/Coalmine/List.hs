module Coalmine.List where

import Prelude
import qualified Data.Map.Strict as Map


nubSort :: Ord a => [a] -> [a]
nubSort = nubSortOn id

nubSortOn :: Ord b => (a -> b) -> [a] -> [a]
nubSortOn mapper = Map.elems . foldr (\ a -> Map.insert (mapper a) a) Map.empty
