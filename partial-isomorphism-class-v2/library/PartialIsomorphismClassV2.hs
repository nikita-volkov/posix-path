module PartialIsomorphismClassV2 where

import Coalmine.Prelude
import Data.Aeson qualified as Aeson
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Lazy qualified as Text.Lazy

-- TODO: Rename to refinement.
-- Implying that the type is a sort of an extension to some base type,
-- hence meaining that it may only have one base type and the base type may have multiple refinements.
class Specialization a where
  type General a
  type SpecializationError a
  generalize :: a -> General a
  specialize :: General a -> Either (SpecializationError a) a

instance Specialization Text where
  type General Text = ByteString
  type SpecializationError Text = _
  generalize = TextEncoding.encodeUtf8
  specialize = TextEncoding.decodeUtf8'

instance Specialization Aeson.Value where
  type General Aeson.Value = Text
  type SpecializationError Aeson.Value = Text
  generalize = Text.Lazy.toStrict . Aeson.encodeText
  specialize = first fromString . Aeson.eitherDecodeStrictText
