module Coalmine.EvenSimplerPaths.AttoparsecHelpers where

import Coalmine.AttoparsecExtras.Text
import qualified Coalmine.EvenSimplerPaths.Charsets as Charsets
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
