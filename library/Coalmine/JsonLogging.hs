module Coalmine.JsonLogging where

import Coalmine.Prelude
import qualified Data.Aeson as Ae

-- |
-- Encoding as YAML for logging purposes.
class JsonLogging a where
  toAesonWithJsonLogging :: a -> Ae.Value

renderAsYamlForLogging :: JsonLogging a => a -> Text
renderAsYamlForLogging =
  error "TODO"
