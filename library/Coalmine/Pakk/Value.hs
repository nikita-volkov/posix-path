module Coalmine.Pakk.Value where

import Coalmine.InternalPrelude
import Coalmine.Pakk.Decoding qualified as Decoding
import Coalmine.Pakk.Encoding qualified as Encoding
import Coalmine.Pakk.Schema qualified as Schema

data Value

simulateFromSchema :: Schema.Schema -> Int -> (Value, Int)
simulateFromSchema =
  error "TODO"
