module Coalmine.HashableExtras where

import Data.Hashable
import Prelude hiding (empty, insert)

extendHash :: Hashable a => a -> Int -> Int
extendHash =
  flip hashWithSalt

extendHashWithInt :: Int -> Int -> Int
extendHashWithInt =
  extendHash
