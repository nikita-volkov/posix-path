module Coalmine.Name.Megaparsec where

import Coalmine.InternalPrelude
import Text.Megaparsec hiding (sepBy, sepBy1)
import Text.Megaparsec.Char qualified as MegaparsecChar
import VectorBuilder.MonadPlus

refineText :: Parsec Void Text a -> Text -> Either Text a
refineText p = left (fromString . errorBundlePretty) . runParser (p <* eof) ""

complete :: Parsec Void Text o -> Parsec Void Text o
complete parser = parser <* eof

nameWords :: Parsec Void Text (BVec Text)
nameWords = sepBy1 nameWord (MegaparsecChar.char '-')

nameWord :: Parsec Void Text Text
nameWord = takeWhile1P (Just "lowercase latin character or digit") (\a -> isAsciiLower a || isDigit a)
