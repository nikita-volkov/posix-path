module PartialIsomorphismClass.V2 where

import Coalmine.Prelude
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Encoding
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
  type SpecializationError Text = Text.Encoding.UnicodeException
  generalize = Text.Encoding.encodeUtf8
  specialize = Text.Encoding.decodeUtf8'

instance Specialization Aeson.Value where
  type General Aeson.Value = Text
  type SpecializationError Aeson.Value = Text
  generalize = Text.Lazy.toStrict . Aeson.encodeToLazyText
  specialize = first fromString . Aeson.eitherDecodeStrictText
