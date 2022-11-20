module Coalmine.Pakk.Value where

import Coalmine.InternalPrelude
import Coalmine.Pakk.Decoding qualified as Decoding
import Coalmine.Pakk.Encoding qualified as Encoding
import Coalmine.Pakk.Schema qualified as Schema
import Coalmine.PureRandom.SizedSeeded qualified as SizedSeeded

data Value
  = VariantValue Int Text Value

simulateFromSchema :: Schema.Schema -> SizedSeeded.SizedSeeded Value
simulateFromSchema = \case
  Schema.ProductSchema fieldSchemas -> do
    SizedSeeded.oneOf . fromList . zipWith fieldValue [0 ..] $ fieldSchemas
  where
    fieldValue i (name, schema) =
      VariantValue i name <$> simulateFromSchema schema
