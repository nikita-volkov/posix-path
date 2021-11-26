-- |
-- Indentation-sensitive line-by-line parsing.
module Coalmine.MultilineParser where

import qualified Coalmine.CharPredicates as CharPredicates
import Coalmine.Prelude
import qualified Data.Text as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as M

-- *

data Located a
  = Located
      !Int
      -- ^ Line.
      !Int
      -- ^ Column.
      a
  deriving (Functor)

data Loc
  = Loc
      !Int
      -- ^ Line.
      !Int
      -- ^ Column.

-- *

data LinesState
  = LinesState
      !Int
      !Int
      ![Text]

-- *

newtype Lines a
  = Lines (LinesState -> Either (Int, Int, Text) (a, LinesState))

instance Functor Lines where
  fmap = error "TODO"

instance Applicative Lines where
  pure = error "TODO"
  (<*>) = error "TODO"

instance Alternative Lines where
  empty = error "TODO"
  Lines l <|> Lines r =
    Lines $ \state -> case l state of
      Right r -> Right r
      Left _ -> r state

-- *

location :: Lines Loc
location = error "TODO"

located :: Lines a -> Lines (Located a)
located (Lines run) = Lines $ \state@(LinesState line col _) ->
  first (Located line col) <$> run state

-- |
-- Runs a parser on an indented input.
--
-- When the indentation is shorter and line is not blank,
-- it is the same as EOF for the wrapped parser.
indented :: Int -> Lines a -> Lines a
indented = error "TODO"

-- |
-- Same as @\p -> 'spaces' >>= \i -> 'indented' i p@.
autoindented :: Lines a -> Lines a
autoindented = error "TODO"

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
  = -- | Parsec transformer over the state of column offset.
    Line (M.ParsecT Void Text (State Int) a)

-- *

-- |
-- Parse space-like chars as amount of spaces.
-- Tab is interpreted as two spaces.
--
-- Useful for detecting indentation.
spaces :: Line Int
spaces = error "TODO"

-- |
-- Narrow a UTF char.
utf8Char :: (Char -> Maybe a) -> Line a
utf8Char = error "TODO"

exactString :: Text -> Line a
exactString = error "TODO"
