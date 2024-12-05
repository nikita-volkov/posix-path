module Coalmine.JsonSchema where

import AesonValueParser qualified
import Coalmine.InternalPrelude
import Data.Text qualified as Text
import Jsonifier qualified

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

data Schema value
  = Schema
      -- | Name of the format.
      -- Empty means no name.
      !Text
      -- | Projection into a composable representation directly renderable to JSON string.
      (value -> Jsonifier.Json)
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
              then Left $ "Shorter than " <> (from . show) minLength
              else
                if length > maxLength
                  then Left $ "Longer than " <> (from . show) maxLength
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
      -- | Narrow from the sum.
      (sum -> Maybe variant)
      -- | Broaden to the sum.
      (variant -> sum)
      -- | Variant schema
      (Schema variant)

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

instance Profunctor ObjectSchema where
  dimap = error "TODO"

instance Functor (ObjectSchema i) where
  fmap = error "TODO"

instance Applicative (ObjectSchema i) where
  pure = error "TODO"
  (<*>) = error "TODO"

requiredSchemaField :: Text -> Schema a -> ObjectSchema a a
requiredSchemaField =
  error "TODO"

unrequiredSchemaField :: Text -> Schema a -> ObjectSchema (Maybe a) (Maybe a)
unrequiredSchemaField =
  error "TODO"
