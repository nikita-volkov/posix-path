module PosixPath.Structures.Name.AttoparsecParsers where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Bool
import Data.Function
import Data.List qualified
import Data.Text (Text)
import PosixPath.Structures.Name.Charsets qualified as Charsets

fileName :: Parser Text
fileName = takeWhile (not . flip Data.List.elem Charsets.notFileName)

fileName1 :: Parser Text
fileName1 = takeWhile1 (not . flip Data.List.elem Charsets.notFileName)

extension :: Parser Text
extension = char '.' *> fileName1
