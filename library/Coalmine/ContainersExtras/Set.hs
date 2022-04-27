module Coalmine.ContainersExtras.Set where

import Coalmine.InternalPrelude
import Data.Set

asSingleton :: Set a -> Maybe a
asSingleton a =
  case size a of
    1 -> case findMin a of
      a -> Just a
    _ -> Nothing
