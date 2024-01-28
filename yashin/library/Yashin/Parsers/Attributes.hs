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
  | UnexpectedAttributeTypeValueError UnexpectedAttributeType
  | ListValueError ListError
  | BinaryValueError Text
  | BinarySetValueError SetError
  | StringValueError Text
  | StringSetValueError SetError
  | BoolValueError Text

data UnexpectedAttributeType = UnexpectedAttributeType
  { actual :: AttributeType,
    expected :: Set AttributeType
  }

data AttributeType
  = ListAttributeType
  | BinaryAttributeType
  | BinarySetAttributeType
  | StringAttributeType
  | StringSetAttributeType
  | BoolAttributeType

data Value a = Value
  { list :: Maybe (BVec Azk.AttributeValue -> Either ListError a),
    binary :: Maybe (Azk.Base64 -> Either Text a),
    binarySet :: Maybe (BVec Azk.Base64 -> Either SetError a),
    string :: Maybe (Text -> Either Text a),
    stringSet :: Maybe (BVec Text -> Either SetError a),
    bool :: Maybe (Bool -> Either Text a)
  }

data ListError = ListError
  { index :: Int,
    error :: ValueError
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
  (<>) left right =
    Value
      { list = left.list <|> right.list,
        binary = left.binary <|> right.binary,
        binarySet = left.binarySet <|> right.binarySet,
        string = left.string <|> right.string,
        stringSet = left.stringSet <|> right.stringSet,
        bool = left.bool <|> right.bool
      }

instance Monoid (Value a) where
  mempty = Value Nothing Nothing Nothing Nothing Nothing Nothing

-- ** Functions

runValue :: forall a. Value a -> Azk.AttributeValue -> Either ValueError a
runValue parser = \case
  Azk.B input ->
    match (.binary) BinaryAttributeType BinaryValueError input
  Azk.BS input ->
    match (.binarySet) BinarySetAttributeType BinarySetValueError input
  Azk.S input ->
    match (.string) StringAttributeType StringValueError input
  Azk.SS input ->
    match (.stringSet) StringSetAttributeType StringSetValueError input
  Azk.BOOL input ->
    match (.bool) BoolAttributeType BoolValueError input
  _ ->
    error "TODO"
  where
    match ::
      (Value a -> Maybe (input -> Either error a)) ->
      AttributeType ->
      (error -> ValueError) ->
      input ->
      Either ValueError a
    match parserSelector actualType errorAdapter input =
      case parserSelector parser of
        Nothing ->
          Left
            ( UnexpectedAttributeTypeValueError
                UnexpectedAttributeType
                  { actual = actualType,
                    expected =
                      fromList
                        ( catMaybes
                            [ parser.binary $> BinaryAttributeType
                            ]
                        )
                  }
            )
        Just parser -> case parser input of
          Left parserError -> Left (errorAdapter parserError)
          Right res -> Right res

attribute :: Text -> Value a -> Attributes a
attribute name parser =
  Attributes \map -> case HashMap.lookup name map of
    Nothing -> Left (valueError MissingValueError)
    Just attributeValue -> first valueError $ runValue parser attributeValue
  where
    valueError x =
      [ AttributesError
          { attribute = name,
            valueError = x
          }
      ]

list :: Value a -> Value (BVec a)
list element =
  mempty
    { list = Just \vec ->
        fmap (fromList . toList)
          $ BVec.iforM vec
          $ \i -> first (ListError i) . runValue element
    }

binary :: (ByteString -> Either Text a) -> Value a
binary =
  error "TODO"

binarySet :: (ByteString -> Either Text a) -> Value a
binarySet =
  error "TODO"

string :: (Text -> Either Text a) -> Value a
string =
  error "TODO"

stringSet :: (Ord a) => (Text -> Either Text a) -> Value (Set a)
stringSet elemParser =
  mempty
    { stringSet = Just \vec ->
        fmap (fromList . toList) $ BVec.iforM vec $ \i -> first (SetError i) . elemParser
    }

bool :: (Bool -> Maybe a) -> Value a
bool =
  error "TODO"
