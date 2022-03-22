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
