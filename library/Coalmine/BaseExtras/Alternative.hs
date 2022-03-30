module Coalmine.BaseExtras.Alternative where

import Prelude

validate :: Alternative m => (a -> Bool) -> a -> m a
validate predicate value =
  if predicate value
    then pure value
    else empty
