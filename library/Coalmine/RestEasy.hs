module Coalmine.RestEasy where

import qualified AesonValueParser
import qualified Coalmine.BaseExtras.List as List
import Coalmine.InternalPrelude
import Coalmine.Parsing
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text
import qualified Data.Vector as BVec
import qualified Jsonifier
import qualified Network.HTTP.Media as HttpMedia
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

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

-- |
-- Request body parser.
data RequestBody i

instance Functor RequestBody

jsonRequestBody :: AesonValueParser.Value i -> RequestBody i
jsonRequestBody =
  error "TODO"

binaryRequestBody :: Cereal.Get a -> RequestBody a
binaryRequestBody =
  error "TODO"

data Route

postRoute :: [RequestBody req] -> (req -> IO Response) -> Route
postRoute =
  error "TODO"

staticSegmentRoute :: Text -> [Route] -> Route
staticSegmentRoute =
  error "TODO"

dynamicSegmentRoute :: Attoparsec.Parser seg -> (seg -> [Route]) -> Route
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
