module Coalmine.BaseExtras.Alternative where

import Coalmine.InternalPrelude

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded predicate value =
  if predicate value
    then pure value
    else empty
