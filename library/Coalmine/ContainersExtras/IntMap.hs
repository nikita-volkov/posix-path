module Coalmine.ContainersExtras.IntMap where

import Coalmine.Prelude
import Data.IntMap.Strict

-- |
-- Or @insertLookup@.
replace :: Int -> v -> IntMap v -> (Maybe v, IntMap v)
replace k v = insertLookupWithKey f k v
  where
    f _ _ _ = v

lookupAndDelete :: Int -> IntMap v -> (Maybe v, IntMap v)
lookupAndDelete = alterF $ \case
  Nothing -> (Nothing, Nothing)
  Just v -> (Just v, Nothing)
