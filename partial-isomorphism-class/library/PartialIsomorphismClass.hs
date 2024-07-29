module PartialIsomorphismClass where

import Coalmine.Prelude
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Lazy qualified as Text.Lazy

class GeneralizationOf special general where
  generalizeFrom :: special -> general
  specializeTo :: general -> Maybe special

specializeFrom ::
  forall general special.
  (GeneralizationOf special general) =>
  general ->
  Maybe special
specializeFrom = specializeTo

generalizeTo ::
  forall general special.
  (GeneralizationOf special general) =>
  special ->
  general
generalizeTo = generalizeFrom

-- * Instances

instance GeneralizationOf Natural Integer where
  generalizeFrom = fromIntegral
  specializeTo n = if n < 0 then Nothing else Just (fromIntegral n)

instance GeneralizationOf Text ByteString where
  generalizeFrom = TextEncoding.encodeUtf8
  specializeTo = either (const Nothing) Just . TextEncoding.decodeUtf8'

instance GeneralizationOf Aeson.Value Text where
  generalizeFrom = Text.Lazy.toStrict . Aeson.encodeToLazyText
  specializeTo = either (const Nothing) Just . Aeson.eitherDecodeStrictText

instance GeneralizationOf Aeson.Value LazyByteString where
  generalizeFrom = Aeson.encode
  specializeTo = Aeson.decode

instance GeneralizationOf Aeson.Value ByteString where
  generalizeFrom = ByteString.Lazy.toStrict . Aeson.encode
  specializeTo = Aeson.decodeStrict
