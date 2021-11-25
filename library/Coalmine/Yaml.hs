module Coalmine.Yaml where

import Coalmine.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml

parseByteString :: ByteString -> Either Text Aeson.Value
parseByteString input = left (fromString . Yaml.prettyPrintParseException) (Yaml.decodeEither' input)
