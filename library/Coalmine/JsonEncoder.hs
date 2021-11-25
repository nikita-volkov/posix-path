module Coalmine.JsonEncoder where

import Coalmine.Prelude
import qualified Jsonifier

-- |
-- Specification of how to serialize a data structure as YAML.
--
-- Can be used to generate 'Schema' for generating format spec
-- to external processes or for generating code.
data JsonEncoder a
  = JsonEncoder Schema (a -> Jsonifier.Json)

literal :: Text -> JsonEncoder a
literal = error "TODO"

renderValueAsText :: JsonEncoder a -> a -> Text
renderValueAsText =
  error "TODO"

renderSchema :: JsonEncoder a -> Schema
renderSchema =
  error "TODO"

-- |
-- Description of the structure of YAML to be produced.
--
-- Can be used to produce specifications and code generators.
data Schema
