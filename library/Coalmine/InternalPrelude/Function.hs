module Coalmine.InternalPrelude.Function where

import Prelude

applyAll :: [a -> b] -> a -> [b]
applyAll fnList arg =
  fmap ($ arg) fnList
