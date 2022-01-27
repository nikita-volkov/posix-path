module Coalmine.SimplePaths.AttoparsecHelpers where

import Coalmine.Prelude
import Data.Attoparsec.Text

complete parser = parser <* endOfInput

absDirPath = char '/' *> dirPath

dirPath = dirs <* optional (char '/')

dirs = sepBy dir (char '/')

dir = takeWhile1 $ \a -> a /= '/' && a /= '\\'

filePathDirs = many $ dir <* char '/'

fileName = takeWhile1 $ \a -> a /= '.'

extension = char '.' *> takeWhile1 (\a -> a /= '.')

abs = char '/' $> True <|> pure False
