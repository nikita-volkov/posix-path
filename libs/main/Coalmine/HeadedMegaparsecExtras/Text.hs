module Coalmine.HeadedMegaparsecExtras.Text where

import Coalmine.InternalPrelude hiding (bit, expr, filter, head, option, some, sortBy, tail, try)
import Coalmine.Located qualified as Located
import Coalmine.MegaparsecExtras qualified as MegaparsecExtras
import Data.CaseInsensitive (CI, FoldCase)
import HeadedMegaparsec hiding (string)
import Text.Megaparsec (Parsec, Stream, TraversableStream, VisualStream)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as MegaparsecChar
import Text.Megaparsec.Char.Lexer qualified as MegaparsecLexer

-- * --

type Parser e = HeadedParsec e Text

eol :: Ord e => Parser e Text
eol = parse MegaparsecChar.eol
