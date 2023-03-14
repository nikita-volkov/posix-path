module Coalmine.Name.Megaparsec where

import Coalmine.InternalPrelude
import Text.Megaparsec hiding (sepBy, sepBy1)
import Text.Megaparsec.Char qualified as MegaparsecChar
import VectorBuilder.MonadPlus

refineText :: Parsec Void Text a -> Text -> Either Text a
refineText p = left (fromString . errorBundlePretty) . runParser (p <* eof) ""

complete parser = parser <* eof

nameWords = sepBy1 nameWord (MegaparsecChar.char '-')

nameWord = takeWhile1P (Just "lowercase latin character or digit") (\a -> isAsciiLower a || isDigit a)
