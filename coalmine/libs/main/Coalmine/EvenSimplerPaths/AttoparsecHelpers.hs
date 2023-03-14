module Coalmine.EvenSimplerPaths.AttoparsecHelpers where

import Coalmine.AttoparsecExtras.Text
import Coalmine.EvenSimplerPaths.Charsets qualified as Charsets
import Coalmine.InternalPrelude hiding (takeWhile)
import Data.Attoparsec.Text

complete parser = parser <* endOfInput

absDirPath = char '/' *> dirPath

dirPath = dirs <* optional (char '/')

dirs = sepBy dir (char '/')

dir = takeWhile1 $ \a -> a /= '/' && a /= '\\'

filePathDirs = many $ dir <* char '/'

fileName = textNotOfCharset Charsets.notFileName

fileName1 = textNotOfCharset1 Charsets.notFileName

extension = char '.' *> fileName1

abs = char '/' $> True <|> pure False

sepByNonEmpty :: Parser a -> Parser b -> Parser (NonEmpty a)
sepByNonEmpty element separator =
  (:|) <$> element <*> many (separator *> element)
