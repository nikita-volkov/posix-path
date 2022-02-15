module Coalmine.BaseExtras.List where

import qualified Data.Map.Strict as Map
import Prelude

nubSort :: Ord a => [a] -> [a]
nubSort = nubSortOn id

nubSortOn :: Ord b => (a -> b) -> [a] -> [a]
nubSortOn mapper = Map.elems . foldr (\a -> Map.insert (mapper a) a) Map.empty

-- |
-- Non-recursive transform over a list, like 'maybe'.
--
-- > list 1 (\v _ -> v - 2) [5,6,7] == 3
-- > list 1 (\v _ -> v - 2) []      == 1
-- > \nil cons xs -> maybe nil (uncurry cons) (uncons xs) == list nil cons xs
eliminate :: b -> (a -> [a] -> b) -> [a] -> b
eliminate onNil onCons =
  \case
    h : t -> onCons h t
    _ -> onNil

mapTail :: ([a] -> [a]) -> [a] -> [a]
mapTail mapper =
  eliminate [] (\h t -> h : mapper t)

mapEachInTail :: (a -> a) -> [a] -> [a]
mapEachInTail mapper =
  mapTail (fmap mapper)

mapHeadAndTail :: (a -> b) -> ([a] -> [b]) -> [a] -> [b]
mapHeadAndTail hMapper tMapper =
  eliminate [] (\h t -> hMapper h : tMapper t)

foldMapHeadAndTail :: Monoid b => (a -> b) -> ([a] -> b) -> [a] -> b
foldMapHeadAndTail hMapper tMapper =
  eliminate mempty (\h t -> hMapper h <> tMapper t)

-- |
-- Same as 'foldMap', but applies a different mapping function to head.
foldMapHeadAndEachOfTail :: Monoid b => (a -> b) -> (a -> b) -> [a] -> b
foldMapHeadAndEachOfTail hMapper tMapper =
  eliminate mempty (\h t -> foldr (\t next -> tMapper t <> next) (hMapper h) t)

foldrHeadAndEachOfTail ::
  (a -> b -> b) ->
  (a -> b) ->
  b ->
  [a] ->
  b
foldrHeadAndEachOfTail onTailElement onHead onNone = \case
  head : tail -> foldr onTailElement (onHead head) tail
  _ -> onNone

isLongerThanOne :: [a] -> Bool
isLongerThanOne =
  \case
    _ : _ : _ -> True
    _ -> False

dropFromEnd :: Int -> [a] -> [a]
dropFromEnd amount =
  reverse . drop amount . reverse
