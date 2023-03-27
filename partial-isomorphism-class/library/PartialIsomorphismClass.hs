module PartialIsomorphismClass where

import Coalmine.Prelude
import Data.Text.Encoding qualified as TextEncoding

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
