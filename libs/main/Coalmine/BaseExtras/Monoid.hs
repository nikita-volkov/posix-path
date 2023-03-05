module Coalmine.BaseExtras.Monoid where

import Coalmine.InternalPrelude

filtered :: (Monoid m) => (a -> Bool) -> (a -> m) -> a -> m
filtered p k x
  | p x = k x
  | otherwise = mempty
