module Coalmine.Interpret where

import Coalmine.Prelude

-- *

newtype Interpret a
  = Interpret (a -> Either (InterpretErr a) a)
  deriving (Semigroup)

-- **

data family InterpretErr a
