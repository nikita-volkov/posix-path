module Coalmine.Optics where

import Coalmine.InternalPrelude
import qualified Coalmine.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified MooreMachines as Mm


feedingMooreOf :: Fold a b -> Moore b c -> a -> Moore b c
feedingMooreOf optic =
  foldlOf' optic step
  where
    step moore b =
      Mm.feeding b moore

{-|

>>> toListOf uniqueFolded [1,2,1,3,1,4,2]
[1,2,3,4]

-}
uniqueFolded :: (Eq a, Hashable a, Foldable f) => Fold (f a) a
uniqueFolded =
  folding HashSet.fromFoldable

{-|

>>> toListOf mapAssoc (Map.fromList [('b',2),('c',3),('a',1)])
[('a',1),('b',2),('c',3)]

-}
mapAssoc :: Fold (Map k v) (k, v)
mapAssoc =
  folding Map.toList

{-|

>>> toListOf hashMapAssoc (HashMap.fromList [('b',2),('c',3),('a',1)])
[('a',1),('b',2),('c',3)]

-}
hashMapAssoc :: Fold (HashMap k v) (k, v)
hashMapAssoc =
  folding HashMap.toList
