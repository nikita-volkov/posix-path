module Coalmine.Comms.Value where

import Coalmine.Comms.Decoders qualified as Decoders
import Coalmine.Comms.Schema qualified as Schema
import Coalmine.InternalPrelude
import Coalmine.PtrKit.Decoder qualified as Decoder
import Coalmine.PureRandom.SizedSeeded qualified as SizedSeeded

data Value
  = VariantValue Int Text Value
  | ProductValue [(Text, Value)]
  | SeqValue (BVec Value)

decode :: Schema.Schema -> Decoder.Decoder Value
decode =
  error "TODO"

simulateFromSchema :: Schema.Schema -> SizedSeeded.SizedSeeded Value
simulateFromSchema = \case
  Schema.ProductSchema fieldSchemas ->
    fmap ProductValue . traverse fieldValue $ fieldSchemas
    where
      fieldValue (name, schema) =
        (name,) <$> simulateFromSchema schema
  Schema.SumSchema variantSchemas ->
    SizedSeeded.oneOf . fromList . zipWith variantValue [0 ..] $ variantSchemas
    where
      variantValue i (name, schema) =
        VariantValue i name <$> simulateFromSchema schema
  Schema.SeqSchema minLength maxLength elementSchema ->
    error "TODO"
  _ ->
    error "TODO"
