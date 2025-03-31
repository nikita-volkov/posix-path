module PosixPath.Structures.NameSegment
  ( NameSegment,
    attoparsecParserOf,
    toText,
  )
where

import Algorithms.NaturalSort qualified
import Data.Attoparsec.Text qualified as Attoparsec
import Data.List qualified as List
import Data.Text qualified as Text
import PosixPath.BaseExtras.Prelude
import Test.QuickCheck qualified as QuickCheck

newtype NameSegment = NameSegment Text
  deriving (Eq, Show)

instance Ord NameSegment where
  compare (NameSegment text1) (NameSegment text2) =
    on compare Algorithms.NaturalSort.sortKey text1 text2

instance Arbitrary NameSegment where
  arbitrary = do
    maxSize <- QuickCheck.getSize
    textLength <- QuickCheck.chooseInt (1, min maxSize 1)
    chars <- replicateM textLength do
      QuickCheck.suchThat arbitrary (not . flip List.elem unsupportedChars)
    pure (NameSegment (Text.pack chars))
  shrink (NameSegment text) = do
    text <- QuickCheck.shrink (Text.unpack text)
    if List.null text
      then []
      else pure (NameSegment (Text.pack text))

-- https://stackoverflow.com/a/35352640/485115
unsupportedChars :: [Char]
unsupportedChars = "./\\:\NUL*?\"<>|"

attoparsecParserOf :: Attoparsec.Parser Text
attoparsecParserOf = Attoparsec.takeWhile1 (not . flip List.elem unsupportedChars)

toText :: NameSegment -> Text
toText (NameSegment text) = text
