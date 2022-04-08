module Coalmine.BaseExtras.Function where

import Coalmine.InternalPrelude

apply :: a -> (a -> b) -> b
apply arg fn =
  fn arg

applyAll :: [a -> b] -> a -> [b]
applyAll fnList arg =
  fmap ($ arg) fnList
