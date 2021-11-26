-- |
-- Indentation-sensitive line-by-line parsing.
module Coalmine.MultilineParser where

import qualified Coalmine.CharPredicates as CharPredicates
import Coalmine.Prelude
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as M

-- *

data LinesState
  = LinesState
      !Int
      -- ^ Line.
      !Int
      -- ^ Indentation.

-- *

newtype Lines a
  = Lines (LinesState -> A.Parser (a, LinesState))

instance Functor Lines where
  fmap = error "TODO"

instance Applicative Lines where
  pure = error "TODO"
  (<*>) = error "TODO"

instance Alternative Lines where
  empty = error "TODO"
  Lines l <|> Lines r =
    Lines $ \state ->
      error "TODO"

instance Monad Lines where
  return = pure
  (>>=) = error "TODO"

-- *

-- |
-- Runs a parser on an indented input.
--
-- When the indentation is shorter and line is not blank,
-- it is the same as EOF for the wrapped parser.
indented :: Int -> Lines a -> Lines a
indented = error "TODO"

-- |
-- Same as @\p -> 'line' 'spaces' >>= \i -> 'indented' i p@.
autoindented :: Lines a -> Lines a
autoindented = \p -> line spaces >>= \i -> indented i p

-- |
-- Parse a single line starting at the indentation of the current level.
--
-- If the current line is not indented enough,
-- it is the same as reaching end of input, so this parser will fail.
line :: Line a -> Lines a
line = error "TODO"

-- *

-- |
-- Parser in the context of a single line with indentation of the scope applied.
--
-- Reaching a line end is the same as reaching end of input.
newtype Line a
  = Line
      ( -- Line.
        Int ->
        -- Amount of indentation characters.
        Int ->
        -- Column offset.
        Int ->
        -- Parser producing result and new column offset.
        A.Parser (a, Int)
      )

-- *

-- |
-- Current location.
-- Use this to associate results with location in the input.
--
-- One typical application of this is in multi-stage parsers.
-- This function lets you relate errors from further stages
-- with a specific location in the parsed source code.
location :: Line (Int, Int)
location =
  Line $ \line indentation column ->
    pure ((line, indentation + column), column)

-- |
-- Parse space-like chars as amount of spaces.
-- Tab is interpreted as two spaces.
--
-- Useful for detecting indentation.
spaces :: Line Int
spaces = error "TODO"

-- |
-- Narrow a UTF-8 char.
utf8Char :: (Char -> Maybe a) -> Line a
utf8Char = error "TODO"

exactString :: Text -> Line a
exactString = error "TODO"
