module Coalmine.RestEasy where

import qualified AesonValueParser
import qualified Coalmine.BaseExtras.List as List
import Coalmine.InternalPrelude
import Coalmine.Parsing
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

-- |
-- Use a schema to parse some JSON input.
parseJson :: Schema a -> ByteString -> Maybe a
parseJson =
  error "TODO"

-- |
-- Use a schema to render some value as JSON.
renderJson :: Schema a -> a -> ByteString
renderJson =
  error "TODO"

-- * --

data Authenticated request = Authenticated
  { authenticatedToken :: !ByteString,
    authenicatedRequest :: !request
  }
  deriving (Functor)

-- Something closely related to security policy.
data SecurityPolicy sess

sessionInHeaderApiKeySecurityPolicy :: SessionStore sess -> SecurityPolicy sess
sessionInHeaderApiKeySecurityPolicy =
  error "TODO"

data Authenticator cred sess

data SessionStore sess

authenticator :: (cred -> IO (Maybe sess)) -> Authenticator cred sess
authenticator =
  error "TODO"

-- * Schema

data Schema value
  = Schema
      !Text
      -- ^ Name of the format.
      -- Empty means no name.
      (value -> Jsonifier.Json)
      -- ^ Projection into a composable representation directly renderable to JSON string.
      (AesonValueParser.Value value)

instance Invariant Schema where
  invmap f g (Schema name enc dec) =
    Schema name (enc . g) (fmap f dec)

objectSchema :: ObjectSchema a a -> Schema a
objectSchema =
  error "TODO"

arraySchema ::
  -- | Min size.
  Int ->
  -- | Max size. It is a good practice to specify a reasonable amount for
  -- security reasons.
  Int ->
  Schema a ->
  Schema (BVec a)
arraySchema =
  error "TODO"

stringSchema ::
  -- | Min length.
  Int ->
  -- | Max length. It is a good practice to specify a reasonable length for
  -- security reasons.
  Int ->
  Schema Text
stringSchema minLength maxLength =
  Schema
    "String"
    Jsonifier.textString
    ( AesonValueParser.string . AesonValueParser.matchedText $ \text ->
        let length = Text.length text
         in if length < minLength
              then Left $ "Shorter than " <> showAs minLength
              else
                if length > maxLength
                  then Left $ "Longer than " <> showAs maxLength
                  else Right text
    )

oneOfSchema :: [OneOfSchemaVariant a] -> Schema a
oneOfSchema =
  error "TODO"

uuidSchema :: Schema UUID
uuidSchema =
  error "TODO"

-- ** One Of Schema

data OneOfSchemaVariant sum
  = forall variant.
    OneOfSchemaVariant
      (sum -> Maybe variant)
      -- ^ Narrow from the sum.
      (variant -> sum)
      -- ^ Broaden to the sum.
      (Schema variant)
      -- ^ Variant schema

instance Invariant OneOfSchemaVariant where
  invmap f g (OneOfSchemaVariant narrow broaden schema) =
    OneOfSchemaVariant (narrow . g) (f . broaden) schema

oneOfSchemaVariant ::
  -- | Attempt to extract the variant from the sum.
  (sum -> Maybe variant) ->
  -- | Map the variant to the sum.
  (variant -> sum) ->
  -- | Schema of the variant.
  Schema variant ->
  OneOfSchemaVariant sum
oneOfSchemaVariant = OneOfSchemaVariant

-- ** Object Schema

data ObjectSchema i o

instance Profunctor ObjectSchema

instance Functor (ObjectSchema i)

instance Applicative (ObjectSchema i)

requiredSchemaField :: Text -> Schema a -> ObjectSchema a a
requiredSchemaField =
  error "TODO"

unrequiredSchemaField :: Text -> Schema a -> ObjectSchema (Maybe a) (Maybe a)
unrequiredSchemaField =
  error "TODO"

-- * --

data RequestBody i

instance Functor RequestBody

jsonRequestBody :: Schema i -> RequestBody i
jsonRequestBody =
  error "TODO"

data Route

secureGetRoute :: SecurityPolicy sess -> StateT sess IO Response -> Route
secureGetRoute =
  error "TODO"

insecurePostRoute :: [RequestBody req] -> (req -> IO Response) -> Route
insecurePostRoute =
  error "TODO"

securePostRoute :: SecurityPolicy sess -> [RequestBody req] -> (req -> StateT sess IO Response) -> Route
securePostRoute =
  error "TODO"

securePutRoute :: SecurityPolicy sess -> [RequestBody req] -> (req -> StateT sess IO Response) -> Route
securePutRoute =
  error "TODO"

authPostRoute :: SecurityPolicy sess -> [RequestBody (IO (Maybe sess))] -> Route
authPostRoute =
  error "TODO"

staticSegmentRoute :: Text -> [Route] -> Route
staticSegmentRoute =
  error "TODO"

dynamicSegmentRoute :: Schema a -> (a -> [Route]) -> Route
dynamicSegmentRoute =
  error "TODO"

data Response

response :: Int -> Text -> [ResponseContent] -> Response
response =
  error "TODO"

data ResponseContent

jsonResponseContent :: Schema a -> a -> ResponseContent
jsonResponseContent =
  error "TODO"
