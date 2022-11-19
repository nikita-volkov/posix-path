module Coalmine.RestEasy where

import AesonValueParser qualified
import Coalmine.BaseExtras.List qualified as List
import Coalmine.InternalPrelude
import Coalmine.Parsing
import Coalmine.RestEasy.BodyConsumers qualified as BodyConsumers
import Coalmine.RestEasy.MimeTypeLists qualified as MimeTypeLists
import Coalmine.RestEasy.Responses qualified as Responses
import Data.Attoparsec.Text qualified as Attoparsec
import Data.ByteString qualified as ByteString
import Data.Serialize qualified as Cereal
import Data.Text qualified as Text
import Data.Vector qualified as BVec
import Jsonifier qualified
import Network.HTTP.Media qualified as HttpMedia
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

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

-- * Request Body Parser

-- |
-- Request body parser.
data RequestBody a
  = RequestBody
      [Text]
      -- ^ Expected types.
      (IO ByteString -> IO (Either Text a))
      -- ^ Consumer. Calls the provided chunk-producing action until it
      -- produces an empty chunk.

instance Functor RequestBody

jsonRequestBody :: AesonValueParser.Value a -> RequestBody a
jsonRequestBody parser =
  RequestBody MimeTypeLists.json (BodyConsumers.aesonValueParser parser)

binaryRequestBody :: Cereal.Get a -> RequestBody a
binaryRequestBody get =
  RequestBody MimeTypeLists.binary (BodyConsumers.cereal get)

-- * Route execution

runRoutes :: [Route] -> [Text] -> Wai.Request -> IO Wai.Response
runRoutes routes segments request =
  go routes
  where
    go = \case
      Route runRoute : routes ->
        case runRoute segments request of
          Nothing -> go routes
          Just produceResponse -> produceResponse
      _ ->
        return $ Responses.notFound

-- * Route

newtype Route
  = Route ([Text] -> Wai.Request -> Maybe (IO Wai.Response))

postRoute :: [RequestBody req] -> (req -> IO Response) -> Route
postRoute bodyParsers handler =
  Route $ \segments request ->
    error "TODO"

staticSegmentRoute :: Text -> [Route] -> Route
staticSegmentRoute expectedSegment childRoutes =
  Route $ \segments request ->
    case segments of
      h : t ->
        if h == expectedSegment
          then Just $ runRoutes childRoutes t request
          else Nothing
      _ -> Nothing

dynamicSegmentRoute :: Attoparsec.Parser seg -> (seg -> [Route]) -> Route
dynamicSegmentRoute =
  error "TODO"

-- * Response

data Response

response ::
  -- | Status code.
  Int ->
  -- | Status text.
  Text ->
  -- | Renderings of various content-types. The choice is automatically
  -- determined based on the "Accept" header in the request.
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
