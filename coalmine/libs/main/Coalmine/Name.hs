module Coalmine.Name where

import Coalmine.CerealExtras.Get qualified as CerealExtrasGet
import Coalmine.CerealExtras.Put qualified as CerealExtrasPut
import Coalmine.InternalPrelude
import Coalmine.Name.Attoparsec qualified as Attoparsec
import Coalmine.Name.Constants qualified as Constants
import Coalmine.Name.Gens qualified as Gens
import Coalmine.Name.Megaparsec qualified as Megaparsec
import Coalmine.Parsing
import Coalmine.Printing
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Serialize qualified as Cereal
import Data.Text qualified as Text
import Test.QuickCheck qualified as QuickCheck
import Text.Megaparsec qualified as Megaparsec
import TextBuilderDev qualified as TextBuilder

type Slug = Name

-- |
-- Case-agnostic name with words separated and consisting only of digits and Latin letters.
newtype Name = Name {nameParts :: BVec Text}

-- * Instances

instance QuickCheck.Arbitrary Name where
  arbitrary = Name <$> Gens.normalParts Constants.maxParts Constants.maxBytesInPart

instance Cereal.Serialize Name where
  put (Name parts) = do
    CerealExtrasPut.vec CerealExtrasPut.compactText parts
  get =
    fmap Name . CerealExtrasGet.secureCompactVec Constants.maxParts $ do
      text <- CerealExtrasGet.secureCompactText Constants.maxBytesInPart
      case parse Attoparsec.part text of
        Right word -> return word
        Left err -> fail $ to err <> "\nInput: " <> to text

deriving instance Eq Name

deriving instance Ord Name

deriving instance Hashable Name

instance Show Name where
  show (Name parts) = parts & toList & fmap to & intercalate "-"

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

instance ToJSON Name where
  toJSON = toJSON . to @Text . toCompactBuilder

instance ToJSONKey Name where
  toJSONKey = contramap (to @Text . toCompactBuilder) toJSONKey

instance CompactPrinting Name where
  toCompactBuilder = toSpinalCaseTextBuilder

instance BroadPrinting Name where
  toBroadBuilder = fromTextBuilder . toCompactBuilder

-- * QuickCheck

normalQuickCheckGen :: Int -> Int -> QuickCheck.Gen Name
normalQuickCheckGen maxParts maxBytesInPart =
  Name <$> Gens.normalParts maxParts maxBytesInPart

onlyFirstAlphaFirstQuickCheckGen :: Int -> Int -> QuickCheck.Gen Name
onlyFirstAlphaFirstQuickCheckGen maxParts maxBytesInPart =
  Name <$> Gens.onlyFirstAlphaFirstParts maxParts maxBytesInPart

allAlphaFirstQuickCheckGen :: Int -> Int -> QuickCheck.Gen Name
allAlphaFirstQuickCheckGen maxParts maxBytesInPart =
  Name <$> Gens.allAlphaFirstParts maxParts maxBytesInPart

-- * Parsing

attoparsec :: Attoparsec.Parser Name
attoparsec = Attoparsec.parts <&> Name

megaparsec :: Megaparsec.Parsec Void Text Name
megaparsec = Megaparsec.nameWords <&> Name

-- * --

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

-- * --

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

-- * --

fromTextListUnchecked :: [Text] -> Name
fromTextListUnchecked =
  Name . fromList

-- * --

refineText :: Text -> Either Text Name
refineText = Megaparsec.refineText megaparsec
