module Coalmine.BaseExtras.Integer where

import Coalmine.Prelude

countDigits :: Integral a => a -> Int
countDigits = go 1 . abs
  where
    go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds
