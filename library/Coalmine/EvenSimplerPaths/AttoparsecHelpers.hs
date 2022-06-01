module Coalmine.EvenSimplerPaths.AttoparsecHelpers where

import Coalmine.InternalPrelude hiding (takeWhile)
import Data.Attoparsec.Text

complete parser = parser <* endOfInput

absDirPath = char '/' *> dirPath

dirPath = dirs <* optional (char '/')

dirs = sepBy dir (char '/')

dir = takeWhile1 $ \a -> a /= '/' && a /= '\\'

filePathDirs = many $ dir <* char '/'

fileName = takeWhile $ \a -> a /= '.' && a /= '/' && a /= '\\'

fileName1 = takeWhile1 $ \a -> a /= '.' && a /= '/' && a /= '\\'

extension = char '.' *> takeWhile1 (\a -> a /= '.' && a /= '/' && a /= '\\')

abs = char '/' $> True <|> pure False
