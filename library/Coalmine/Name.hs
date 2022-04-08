module Coalmine.Name where

import qualified AesonValueParser
import Coalmine.InternalPrelude
import qualified Coalmine.Name.Attoparsec as Attoparsec
import qualified Coalmine.Name.Megaparsec as Megaparsec
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec
import qualified TextBuilderDev as TextBuilder

-- |
-- Case-agnostic name with words separated and consisting only of digits and Latin letters.
newtype Name = Name (BVec Text)

-- * Instances

deriving instance Eq Name

deriving instance Ord Name

deriving instance Hashable Name

instance Show Name where
  show (Name parts) = parts & toList & fmap toString & intercalate "-"

instance IsString Name where
  fromString =
    either (error . Megaparsec.errorBundlePretty) Name
      . Megaparsec.runParser (Megaparsec.nameWords <* Megaparsec.eof @Void @Text) ""
      . fromString

instance Semigroup Name where
  (<>) (Name a) (Name b) = Name (a <> b)

instance Monoid Name where
  mempty = Name mempty
  mappend = (<>)

instance LenientParser Name where
  lenientParser = attoparsec

instance ToString Name where
  toString = toString . toText

instance ToText Name where
  toText = toText . toTextBuilder

instance ToTextBuilder Name where
  toTextBuilder = toSpinalCaseTextBuilder

-- *

attoparsec :: Attoparsec.Parser Name
attoparsec = Attoparsec.nameWords <&> Name

megaparsec :: Ord err => Megaparsec.Parsec err Text Name
megaparsec = Megaparsec.nameWords <&> Name

stringAesonValueParser :: AesonValueParser.String Name
stringAesonValueParser = AesonValueParser.megaparsedText megaparsec

-- *

toLowerCamelCaseText :: Name -> Text
toLowerCamelCaseText (Name vec) = case toList vec of
  head : tail -> mconcat (head : fmap Text.toTitle tail)
  _ -> mempty

toUpperCamelCaseText :: Name -> Text
toUpperCamelCaseText (Name vec) = foldMap Text.toTitle vec

toSpinalCaseText :: Name -> Text
toSpinalCaseText (Name vec) = Text.intercalate "-" (toList vec)

toSnakeCaseText :: Name -> Text
toSnakeCaseText (Name vec) = Text.intercalate "_" (toList vec)

-- *

toLowerCamelCaseTextBuilder :: Name -> TextBuilder
toLowerCamelCaseTextBuilder (Name vec) = case toList vec of
  head : tail -> mconcat (TextBuilder.text head : fmap (TextBuilder.text . Text.toTitle) tail)
  _ -> mempty

toUpperCamelCaseTextBuilder :: Name -> TextBuilder
toUpperCamelCaseTextBuilder (Name vec) = foldMap (TextBuilder.text . Text.toTitle) vec

toSpinalCaseTextBuilder :: Name -> TextBuilder
toSpinalCaseTextBuilder (Name vec) = TextBuilder.intercalate "-" (fmap TextBuilder.text vec)

toSnakeCaseTextBuilder :: Name -> TextBuilder
toSnakeCaseTextBuilder (Name vec) = TextBuilder.intercalate "_" (fmap TextBuilder.text vec)

-- *

refineText :: Text -> Either Text Name
refineText = Megaparsec.refineText megaparsec

-- *

class FromSpinalCaseName a where
  fromSpinalCaseName :: Name -> a

instance FromSpinalCaseName Text where
  fromSpinalCaseName = toSpinalCaseText

instance FromSpinalCaseName TextBuilder where
  fromSpinalCaseName = toSpinalCaseTextBuilder

-- *

class FromUpperCamelCaseName a where
  fromUpperCamelCaseName :: Name -> a

instance FromUpperCamelCaseName Text where
  fromUpperCamelCaseName = toUpperCamelCaseText

instance FromUpperCamelCaseName TextBuilder where
  fromUpperCamelCaseName = toUpperCamelCaseTextBuilder
