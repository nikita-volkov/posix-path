module Coalmine.Name.Megaparsec where

import Coalmine.Prelude
import Text.Megaparsec hiding (sepBy)
import qualified Text.Megaparsec.Char as MegaparsecChar
import VectorBuilder.MonadPlus

refineText :: Parsec Void Text a -> Text -> Either Text a
refineText p = left (fromString . errorBundlePretty) . runParser (p <* eof) ""

complete parser = parser <* eof

nameWords = sepBy nameWord (MegaparsecChar.char '-')

nameWord = takeWhile1P (Just "lowercase latin character or digit") (\a -> isAsciiLower a || isDigit a)
