module Coalmine.Pakk.ValueClass where

import Coalmine.InternalPrelude
import Coalmine.Pakk.Codec qualified as Codec

-- |
-- Provides a data type with a representation in the Pakk format.
class PakkValue a where
  pakkValueCodec :: Codec.Codec a
