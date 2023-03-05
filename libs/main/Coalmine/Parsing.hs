module Coalmine.Parsing where

import Coalmine.HeadedMegaparsecExtras qualified as HeadedMegaparsecExtras
import Coalmine.InternalPrelude
import Data.Attoparsec.Text qualified
import HeadedMegaparsec qualified
import Text.Megaparsec qualified as Megaparsec

class Parsing parser where
  type ParserInput parser
  parse :: parser a -> ParserInput parser -> Either Text a

instance Parsing Data.Attoparsec.Text.Parser where
  type ParserInput Data.Attoparsec.Text.Parser = Text
  parse parser input =
    Data.Attoparsec.Text.parseOnly (parser <* Data.Attoparsec.Text.endOfInput) input
      & first fromString

instance
  (Megaparsec.TraversableStream strm, Megaparsec.VisualStream strm) =>
  Parsing (HeadedMegaparsec.HeadedParsec Void strm)
  where
  type ParserInput (HeadedMegaparsec.HeadedParsec Void strm) = strm
  parse =
    HeadedMegaparsecExtras.toRefiner

type TextParser = Data.Attoparsec.Text.Parser

parseWith :: (Parsing parser) => parser a -> ParserInput parser -> Either Text a
parseWith = parse
