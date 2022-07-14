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

zipWithTotally :: (a -> b -> c) -> [a] -> [b] -> Either (Int, Int) [c]
zipWithTotally pair left right =
  zipFold leftRemainder rightRemainder pair' nil left right 0 []
  where
    leftRemainder remainder sharedSize _ = Left (sharedSize + length remainder, sharedSize)
    rightRemainder remainder sharedSize _ = Left (sharedSize, sharedSize + length remainder)
    pair' a b deeper !sharedSize !revList =
      deeper (succ sharedSize) (pair a b : revList)
    nil _ revList = Right (reverse revList)

-- |
-- A more generic version of the original list-specialized version:
--
-- > intercalate :: [a] -> [[a]] -> [a]
intercalate :: Monoid a => a -> [a] -> a
intercalate = mapIntercalate id

mapIntercalate :: Monoid m => (a -> m) -> m -> [a] -> m
mapIntercalate proj separator = \case
  [] -> mempty
  head : tail ->
    foldl' (\acc element -> acc <> separator <> proj element) (proj head) tail

-- * --

-- | @atLength atLen atEnd n ls@ unravels list @ls@ to position @n@. Precisely:
--
-- > atLength atLenPred atEndPred n ls
-- >  | n < 0         = atLenPred ls
-- >  | length ls < n = atEndPred (n - length ls)
-- >  | otherwise     = atLenPred (drop n ls)
atLength ::
  -- | Called when @length ls >= n@, passed (@drop n ls@)
  -- NB: arg passed to this function may be []
  ([a] -> b) ->
  -- | Called when @length ls < n@
  b ->
  Int ->
  [a] ->
  b
atLength atLenPred atEnd n0 ls0
  | n0 < 0 = atLenPred ls0
  | otherwise = go n0 ls0
  where
    -- go's first arg n >= 0
    go 0 ls = atLenPred ls
    go _ [] = atEnd -- n > 0 here
    go n (_ : xs) = go (n - 1) xs

-- ** Some special cases of atLength

-- | @(isLongerThan n xs) = (length xs > n)@
isLongerThan :: Int -> [a] -> Bool
isLongerThan n lst
  | n < 0 = True
  | otherwise = atLength (not . null) False n lst

-- | @(isNotShorterThan n xs) = (length xs >= n)@
isNotShorterThan :: Int -> [a] -> Bool
isNotShorterThan = atLength (const True) False

-- | @(hasLength n xs) = (length xs == n)@
hasLength :: Int -> [a] -> Bool
hasLength n
  | n < 0 = const False
  | otherwise = atLength null False n

-- | @(isNotAsLong n xs) = (length xs /= n)@
isNotAsLong :: Int -> [a] -> Bool
isNotAsLong n lst
  | n < 0 = True
  | otherwise = atLength (not . null) True n lst

-- | @(isNotLongerThan n xs) = (length xs <= n)@
isNotLongerThan :: Int -> [a] -> Bool
isNotLongerThan n lst
  | n < 0 =
      False
  | otherwise =
      atLength null True n lst

-- | @(isShorterThan n xs) == (length xs < n)@
isShorterThan :: Int -> [a] -> Bool
isShorterThan = atLength (const False) True
