module Coalmine.YamlLogging where

import Coalmine.Prelude
import Coalmine.YamlEncoder (YamlEncoder)
import qualified Coalmine.YamlEncoder as YamlEncoder

-- |
-- Encoding as YAML for logging purposes.
class YamlLogging a where
  yamlLoggingEncoder :: YamlEncoder a

renderWithYamlLogging :: YamlLogging a => a -> Text
renderWithYamlLogging =
  error "TODO"
