module Coalmine.VectorExtras.Generic where

import Coalmine.InternalPrelude hiding (Vector, foldMap, foldl', foldr, length, null)
import Coalmine.VectorExtras.Generic.Mutable qualified as Mut
import Data.Vector.Generic
import Data.Vector.Generic.Mutable qualified as Mut

-- |
-- Notice: It is your responsibility to ensure that the indices
-- in the assoc list are within bounds.
fromAssocListWithGen :: Vector v a => Int -> (Int -> a) -> [(Int, a)] -> v a
fromAssocListWithGen size genDefVal list =
  runST $ do
    mv <- Mut.generate size genDefVal
    Mut.writeAssocList mv list
    unsafeFreeze mv

-- |
-- Notice: It is your responsibility to ensure that the indices
-- in the assoc list are within bounds.
fromAssocListWithDef :: Vector v a => Int -> a -> [(Int, a)] -> v a
fromAssocListWithDef size defVal list =
  runST $ do
    mv <- Mut.replicate size defVal
    Mut.writeAssocList mv list
    unsafeFreeze mv

-- |
-- >>> fromReverseListN 3 [1,2,3] :: Data.Vector.Vector Int
-- [3,2,1]
{-# INLINE fromReverseListN #-}
fromReverseListN :: Vector v a => Int -> [a] -> v a
fromReverseListN size list =
  initialized size $ \mv -> Mut.writeListInReverseOrderStartingFrom mv (pred size) list

{-# INLINE initialized #-}
initialized :: Vector v a => Int -> (forall s. Mutable v s a -> ST s ()) -> v a
initialized size initialize = runST $ do
  mv <- Mut.unsafeNew size
  initialize mv
  unsafeFreeze mv

{-# INLINE lastMaybe #-}
lastMaybe :: Vector v a => v a -> Maybe a
lastMaybe v = v !? pred (length v)

{-# INLINE mapToList #-}
mapToList :: Vector v a => (a -> b) -> v a -> [b]
mapToList mapper =
  foldr (\l r -> mapper l : r) []

ifNotNull :: Vector v a => (v a -> b) -> v a -> Maybe b
ifNotNull k a =
  if null a
    then Nothing
    else Just (k a)
