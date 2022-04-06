module Coalmine.ContainersExtras.Map where

import Coalmine.Prelude
import Data.Map.Strict

-- |
-- Or @insertLookup@.
replace :: Ord k => k -> v -> Map k v -> (Maybe v, Map k v)
replace k v = insertLookupWithKey f k v
  where
    f _ _ _ = v

lookupAndDelete :: Ord k => k -> Map k v -> (Maybe v, Map k v)
lookupAndDelete = alterF $ \case
  Nothing -> (Nothing, Nothing)
  Just v -> (Just v, Nothing)

lookupOrReplace :: Ord k => v -> k -> Map k v -> (Either v v, Map k v)
lookupOrReplace val =
  alterF $ \case
    Just oldVal -> (Left oldVal, Just oldVal)
    Nothing -> (Right val, Just val)

insertOrUpdate :: Ord k => (v -> v) -> k -> v -> Map k v -> Map k v
insertOrUpdate updater key defaultVal =
  alter alterer key
  where
    alterer = \case
      Just oldVal -> Just (updater oldVal)
      Nothing -> Just defaultVal
