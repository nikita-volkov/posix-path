module Coalmine.Yaml where

import Coalmine.InternalPrelude
import Data.Aeson qualified as Aeson
import Data.Yaml qualified as Yaml

parseByteString :: ByteString -> Either Text Aeson.Value
parseByteString input = left (fromString . Yaml.prettyPrintParseException) (Yaml.decodeEither' input)
