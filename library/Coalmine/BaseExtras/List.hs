module Coalmine.BaseExtras.List where

import Coalmine.InternalPrelude
import qualified Data.Map.Strict as Map

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
  eliminate mempty (\h t -> foldl' (\acc t -> acc <> tMapper t) (hMapper h) t)

foldrHeadAndEachOfTail ::
  -- | Step function to be executed on each element of the tail.
  (a -> b -> b) ->
  -- | Mapping function to be executed on the head element.
  (a -> b) ->
  -- | Default value to be used when the list is empty.
  --
  -- It is not the initial state of the accumulator.
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

headMaybe :: [a] -> Maybe a
headMaybe =
  listToMaybe

lastMaybe :: [a] -> Maybe a
lastMaybe =
  foldl (const Just) Nothing

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = headMaybe . mapMaybe f

ifNotNull :: ([a] -> b) -> [a] -> Maybe b
ifNotNull k a =
  if null a
    then Nothing
    else Just (k a)

-- |
-- Destructure two lists in the most generic way.
--
-- You can think of it as of destructuring the following data-structure:
--
-- > data ZipFolding a b
-- >   = Pair a b (ZipFolding a b)
-- >   | LeftRemainder [a]
-- >   | RightRemainder [b]
zipFold :: ([a] -> c) -> ([b] -> c) -> (a -> b -> c -> c) -> c -> [a] -> [b] -> c
zipFold leftRemainder rightRemainder pair nil = f
  where
    f (x : xs) (y : ys) = pair x y (f xs ys)
    f [] [] = nil
    f [] right = rightRemainder right
    f left [] = leftRemainder left

zipFoldSharing :: (a -> c -> c) -> (b -> c -> c) -> (a -> b -> c -> c) -> c -> [a] -> [b] -> c
zipFoldSharing leftRemainder rightRemainder pair nil =
  zipFold
    (foldr leftRemainder nil)
    (foldr rightRemainder nil)
    pair
    nil
