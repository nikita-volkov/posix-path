module Coalmine.Slug where

import Coalmine.InternalPrelude
import Coalmine.Name qualified as Name
import Data.Attoparsec.Text qualified as Attoparsec
import Test.QuickCheck qualified as QuickCheck
import Text.Megaparsec qualified as Megaparsec

type Slug = Name.Name

normalQuickCheckGen :: Int -> Int -> QuickCheck.Gen Slug
normalQuickCheckGen = Name.normalQuickCheckGen

onlyFirstAlphaFirstQuickCheckGen :: Int -> Int -> QuickCheck.Gen Slug
onlyFirstAlphaFirstQuickCheckGen = Name.onlyFirstAlphaFirstQuickCheckGen

allAlphaFirstQuickCheckGen :: Int -> Int -> QuickCheck.Gen Slug
allAlphaFirstQuickCheckGen = Name.allAlphaFirstQuickCheckGen

attoparsec :: Attoparsec.Parser Slug
attoparsec = Name.attoparsec

megaparsec :: Megaparsec.Parsec Void Text Slug
megaparsec = Name.megaparsec

toLowerCamelCaseText :: Slug -> Text
toLowerCamelCaseText = Name.toLowerCamelCaseText

toUpperCamelCaseText :: Slug -> Text
toUpperCamelCaseText = Name.toUpperCamelCaseText

toSpinalCaseText :: Slug -> Text
toSpinalCaseText = Name.toSpinalCaseText

toSnakeCaseText :: Slug -> Text
toSnakeCaseText = Name.toSnakeCaseText

toLowerCamelCaseTextBuilder :: Slug -> TextBuilder
toLowerCamelCaseTextBuilder = Name.toLowerCamelCaseTextBuilder

toUpperCamelCaseTextBuilder :: Slug -> TextBuilder
toUpperCamelCaseTextBuilder = Name.toUpperCamelCaseTextBuilder

toSpinalCaseTextBuilder :: Slug -> TextBuilder
toSpinalCaseTextBuilder = Name.toSpinalCaseTextBuilder

toSnakeCaseTextBuilder :: Slug -> TextBuilder
toSnakeCaseTextBuilder = Name.toSnakeCaseTextBuilder

fromTextListUnchecked :: [Text] -> Slug
fromTextListUnchecked = Name.fromTextListUnchecked

refineText :: Text -> Either Text Slug
refineText = Name.refineText
