module Yashin.Parsers.Attributes where

import Amazonka qualified as Azk
import Amazonka.DynamoDB qualified as Azk
import Coalmine.Prelude hiding (String)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as BVec

-- * Model

newtype Attributes a = Attributes
  { parser :: HashMap Text Azk.AttributeValue -> Either [AttributesError] a
  }

data AttributesError = AttributesError
  { attribute :: Text,
    valueError :: ValueError
  }

data ValueError
  = MissingValueError
  | InvalidAttributeTypeValueError InvalidAttributeType
  | BinaryValueError Text
  | BinarySetValueError SetError
  | StringValueError Text
  | StringSetValueError SetError

data InvalidAttributeType = InvalidAttributeType
  { actual :: AttributeType,
    expected :: Set AttributeType
  }

data AttributeType
  = BinaryAttributeType
  | BinarySetAttributeType
  | StringAttributeType
  | StringSetAttributeType

data Value a = Value
  { binary :: Maybe (Azk.Base64 -> Either Text a),
    binarySet :: Maybe (BVec Azk.Base64 -> Either SetError a),
    string :: Maybe (Text -> Either Text a),
    stringSet :: Maybe (BVec Text -> Either SetError a)
  }

data SetError = SetError
  { index :: Int,
    error :: Text
  }

-- * Mappings

-- ** Instances

-- *** AttributeType

deriving instance Eq AttributeType

deriving instance Ord AttributeType

deriving instance Enum AttributeType

deriving instance Bounded AttributeType

-- *** Attributes

deriving instance Functor Attributes

instance Applicative Attributes where
  pure a =
    Attributes \_ -> Right a
  (<*>) (Attributes leftParser) (Attributes rightParser) =
    Attributes \input ->
      liftA2 ($) (leftParser input) (rightParser input)

instance Alternative Attributes where
  empty =
    Attributes \_ -> Left []
  (<|>) (Attributes leftParser) (Attributes rightParser) =
    Attributes \input ->
      case leftParser input of
        Left leftErrors ->
          case rightParser input of
            Left rightErrors ->
              Left (leftErrors <> rightErrors)
            Right res ->
              Right res
        Right res ->
          Right res

-- *** Value

deriving instance Functor Value

instance Semigroup (Value a) where
  (<>) = error "TODO"

instance Monoid (Value a) where
  mempty = Value Nothing Nothing Nothing Nothing

-- ** Functions

attribute :: Text -> Value a -> Attributes a
attribute name parser =
  Attributes \map -> case HashMap.lookup name map of
    Nothing -> valueError MissingValueError
    Just attributeValue -> case attributeValue of
      Azk.B base64 -> case parser.binary of
        Nothing -> invalidAttributeType BinaryAttributeType
        Just parser -> case parser base64 of
          Left binaryError -> valueError (BinaryValueError binaryError)
          Right res -> Right res
      Azk.BS base64Vec -> case parser.binarySet of
        Nothing -> invalidAttributeType BinarySetAttributeType
        Just parser -> case parser base64Vec of
          Left binaryError -> valueError (BinarySetValueError binaryError)
          Right res -> Right res
      _ ->
        error "TODO"
  where
    valueError valueError =
      Left
        [ AttributesError
            { attribute = name,
              valueError = valueError
            }
        ]
    invalidAttributeType actual =
      valueError
        ( InvalidAttributeTypeValueError
            ( InvalidAttributeType
                { actual = actual,
                  expected =
                    fromList
                      ( catMaybes
                          [ parser.binary $> BinaryAttributeType
                          ]
                      )
                }
            )
        )

stringValue :: (Text -> Either Text a) -> Value a
stringValue =
  error "TODO"

stringSetValue :: (Ord a) => (Text -> Either Text a) -> Value (Set a)
stringSetValue elemParser =
  mempty
    { stringSet = Just \vec ->
        fmap (fromList . toList) $ BVec.iforM vec $ \i -> first (SetError i) . elemParser
    }
