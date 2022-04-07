module Coalmine.Res where

import Coalmine.InternalPrelude

type Res a =
  Either (ResErr a) a

data family ResErr a
