module Coalmine.TransformersExtras.Cont where

import Coalmine.InternalPrelude

const :: r -> Cont r a
const r =
  cont $ Coalmine.InternalPrelude.const r
