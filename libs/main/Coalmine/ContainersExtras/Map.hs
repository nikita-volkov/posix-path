module Coalmine.ContainersExtras.Map where

import Coalmine.InternalPrelude hiding (empty, insert)
import Data.Map.Strict

insertLookup :: (Ord k) => (v -> v -> v) -> k -> v -> Map k v -> (Maybe v, Map k v)
insertLookup f k v = insertLookupWithKey f' k v
  where
    f' _ new old = f new old

replace :: (Ord k) => k -> v -> Map k v -> (Maybe v, Map k v)
replace k v = insertLookupWithKey f k v
  where
    f _ _ _ = v

lookupAndDelete :: (Ord k) => k -> Map k v -> (Maybe v, Map k v)
lookupAndDelete = alterF $ \case
  Nothing -> (Nothing, Nothing)
  Just v -> (Just v, Nothing)

lookupOrReplace :: (Ord k) => v -> k -> Map k v -> (Either v v, Map k v)
lookupOrReplace val =
  alterF $ \case
    Just oldVal -> (Left oldVal, Just oldVal)
    Nothing -> (Right val, Just val)

insertOrUpdate :: (Ord k) => (v -> v) -> k -> v -> Map k v -> Map k v
insertOrUpdate updater key defaultVal =
  alter alterer key
  where
    alterer = \case
      Just oldVal -> Just (updater oldVal)
      Nothing -> Just defaultVal

{-# INLINE traverse_ #-}
traverse_ :: (Applicative m) => (k -> v -> m ()) -> Map k v -> m ()
traverse_ m =
  foldrWithKey step init
  where
    init = pure ()
    step k v acc = m k v *> acc

{-# INLINE replicateM #-}
replicateM :: (Monad m, Ord k) => Int -> m (k, v) -> m (Map k v)
replicateM amount m =
  let iterate index !state =
        if index < amount
          then do
            (k, v) <- m
            iterate (succ index) (insert k v state)
          else return state
   in iterate 0 empty
