module Coalmine.Optics where

import qualified Coalmine.HashSet as HashSet
import Coalmine.InternalPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Set.Optics
import qualified MooreMachines as Mm
import Optics

feedingMooreOf :: Fold a b -> Moore b c -> a -> Moore b c
feedingMooreOf optic =
  foldlOf' optic step
  where
    step moore b =
      Mm.feeding b moore

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
      Mm.feedingFoldable i Mm.countEach & extract & HashMap.toList

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
