module Coalmine.InternalPrelude.Function where

import Prelude

apply :: a -> (a -> b) -> b
apply arg fn =
  fn arg

applyAll :: [a -> b] -> a -> [b]
applyAll fnList arg =
  fmap ($ arg) fnList
