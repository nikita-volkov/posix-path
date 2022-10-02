module Coalmine.Name where

import qualified AesonValueParser
import qualified Coalmine.CerealExtras.Compact as CerealExtrasCompact
import qualified Coalmine.CerealExtras.Get as CerealExtrasGet
import qualified Coalmine.CerealExtras.Put as CerealExtrasPut
import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as MultilineTextBuilder
import qualified Coalmine.Name.Attoparsec as Attoparsec
import qualified Coalmine.Name.Constants as Constants
import qualified Coalmine.Name.Gens as Gens
import qualified Coalmine.Name.Megaparsec as Megaparsec
import Coalmine.Parsing
import Coalmine.Printing
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Megaparsec as Megaparsec
import qualified TextBuilderDev as TextBuilder

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

megaparsec :: Ord err => Megaparsec.Parsec err Text Name
megaparsec = Megaparsec.nameWords <&> Name

stringAesonValueParser :: AesonValueParser.String Name
stringAesonValueParser = AesonValueParser.megaparsedText megaparsec

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
