module Coalmine.Optics where

import Coalmine.InternalPrelude
import Coalmine.UnorderedContainersExtras.HashSet qualified as HashSet
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Set.Optics
import Optics

feedingMooreOf :: Fold a b -> Moore b c -> a -> Moore b c
feedingMooreOf optic =
  foldlOf' optic step
  where
    step (Moore _ progress) b =
      progress b

-- |
--
-- >>> toListOf foldedDuplicated [1,2,1,3,5,4,3,1]
-- [1,3]
foldedDuplicated :: (Eq a, Hashable a, Foldable f) => Fold (f a) a
foldedDuplicated =
  foldedFilteringByCount (> 1)

-- |
--
-- >>> toListOf foldedSingleton [1,2,1,3,5,4,3,1]
-- [2,4,5]
foldedSingleton :: (Eq a, Hashable a, Foldable f) => Fold (f a) a
foldedSingleton =
  foldedFilteringByCount (== 1)

foldedFilteringByCount :: (Eq a, Hashable a, Foldable f) => (Int -> Bool) -> Fold (f a) a
foldedFilteringByCount p =
  foldedCounting % filtered (p . snd) % _1

foldedCounting :: (Eq a, Hashable a, Foldable f) => Fold (f a) (a, Int)
foldedCounting =
  folding folder
  where
    folder i =
      feedingFoldable i countEach & extract & HashMap.toList
    feedingFoldable :: (Foldable f) => f a -> Moore a b -> Moore a b
    feedingFoldable =
      foldr step id
      where
        step a next (Moore _ !progress) =
          next (progress a)
    countEach :: (Eq a, Hashable a) => Moore a (HashMap a Int)
    countEach =
      loop HashMap.empty
      where
        loop !a =
          Moore a (\b -> loop (HashMap.alter alterer b a))
        alterer =
          Just . maybe 1 succ

-- |
--
-- >>> toListOf foldedUnique [1,2,1,3,1,4,2]
-- [1,2,3,4]
foldedUnique :: (Eq a, Hashable a, Foldable f) => Fold (f a) a
foldedUnique =
  folding HashSet.fromFoldable

-- |
--
-- >>> toListOf mapAssoc (Map.fromList [('b',2),('c',3),('a',1)])
-- [('a',1),('b',2),('c',3)]
mapAssoc :: Fold (Map k v) (k, v)
mapAssoc =
  folding Map.toList

-- |
--
-- >>> toListOf hashMapAssoc (HashMap.fromList [('b',2),('c',3),('a',1)])
-- [('a',1),('b',2),('c',3)]
hashMapAssoc :: Fold (HashMap k v) (k, v)
hashMapAssoc =
  folding HashMap.toList
