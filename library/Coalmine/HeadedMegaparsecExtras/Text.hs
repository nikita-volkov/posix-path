module Coalmine.HeadedMegaparsecExtras.Text where

import Coalmine.InternalPrelude hiding (bit, expr, filter, head, option, some, sortBy, tail, try)
import Data.CaseInsensitive (CI, FoldCase)
import HeadedMegaparsec hiding (string)
import Text.Megaparsec (Parsec, Stream, TraversableStream, VisualStream)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MegaparsecChar
import qualified Text.Megaparsec.Char.Lexer as MegaparsecLexer

type Parser e = HeadedParsec e Text

eol :: Ord e => Parser e Text
eol = parse MegaparsecChar.eol
