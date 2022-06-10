module Coalmine.RestEasy where

import qualified AesonValueParser
import qualified Coalmine.BaseExtras.List as List
import Coalmine.InternalPrelude
import Coalmine.JsonSchema
import Coalmine.Parsing
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text
import qualified Data.Vector as BVec
import qualified Jsonifier

-- * Execution

-- |
-- Compile routes into a server and run it.
serve ::
  [Route] ->
  -- | Port.
  Int ->
  IO ()
serve =
  error "TODO"

-- * --

data RequestBody i

instance Functor RequestBody

jsonRequestBody :: Schema i -> RequestBody i
jsonRequestBody =
  error "TODO"

data Route

postRoute :: [RequestBody req] -> (req -> IO Response) -> Route
postRoute =
  error "TODO"

staticSegmentRoute :: Text -> [Route] -> Route
staticSegmentRoute =
  error "TODO"

dynamicSegmentRoute :: Text -> Schema a -> (a -> [Route]) -> Route
dynamicSegmentRoute =
  error "TODO"

data Response

response ::
  -- | Status code.
  Int ->
  -- | Status text.
  Text ->
  [ResponseContent] ->
  Response
response =
  error "TODO"

data ResponseContent

jsonifierJsonResponseContent :: Jsonifier.Json -> ResponseContent
jsonifierJsonResponseContent =
  error "TODO"

cerealBinaryResponseContent :: Cereal.Put -> ResponseContent
cerealBinaryResponseContent =
  error "TODO"
