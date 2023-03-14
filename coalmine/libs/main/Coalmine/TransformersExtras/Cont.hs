module Coalmine.TransformersExtras.Cont where

import Coalmine.InternalPrelude

terminate :: r -> Cont r a
terminate r =
  cont $ const r
