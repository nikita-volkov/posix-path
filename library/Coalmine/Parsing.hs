module Coalmine.Parsing where

import Prelude
import qualified Data.Attoparsec.Text


class Parsing parser where
  type ParserInput parser
  parse :: parser a -> ParserInput parser -> Either Text a

instance Parsing Data.Attoparsec.Text.Parser where
  type ParserInput Data.Attoparsec.Text.Parser = Text
  parse parser input =
    Data.Attoparsec.Text.parseOnly (parser <* Data.Attoparsec.Text.endOfInput) input &
    first fromString
