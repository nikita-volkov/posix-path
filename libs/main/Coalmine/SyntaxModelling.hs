module Coalmine.SyntaxModelling where

import Coalmine.InternalPrelude
import Data.Attoparsec.Text qualified as Attoparsec

-- |
-- Unary syntactic representation for a data-type.
--
-- The data-types are supposed to be provided via newtype-wrappers
-- to provide various syntaxes for data-types.
--
-- Provides means to parse and render a value in such a way
-- that parsing a rendered value will be successful and
-- will produce the same value.
-- That is __the law__ of this typeclass.
class Syntax a where
  attoparsec :: Attoparsec.Parser a
  textBuilder :: a -> TextBuilder

validateLaws :: (Syntax a, Eq a) => a -> Bool
validateLaws val =
  case Attoparsec.parseOnly (attoparsec <* Attoparsec.endOfInput) (to @Text (textBuilder val)) of
    Right parsedVal -> parsedVal == val
    Left _ -> False
