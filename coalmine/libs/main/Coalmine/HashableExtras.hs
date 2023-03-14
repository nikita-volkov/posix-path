module Coalmine.HashableExtras where

import Coalmine.InternalPrelude hiding (empty, insert)

extendHash :: (Hashable a) => a -> Int -> Int
extendHash =
  flip hashWithSalt

extendHashWithInt :: Int -> Int -> Int
extendHashWithInt =
  extendHash
