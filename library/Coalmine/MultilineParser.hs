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
      -- ^ Indentation in amount of chars.
      !Text
      -- ^ Specific chars used for indentation.

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
      l state <|> r state

instance Monad Lines where
  return = pure
  (>>=) = error "TODO"

-- *

-- |
-- Detects the extra indentation on current line
-- and runs the provided parser in the context of this indentation.
--
-- In other words in the lifted parser you should assume that you're
-- dealing with the content with this indendation unapplied.
--
-- Returns the specific characters detected as indentation
-- in the first line of this block and unapplied throughout it.
indented :: Lines a -> Lines (Text, a)
indented (Lines runLines) =
  Lines $ \(LinesState lineNum indentationNum indentationText) -> do
    extraIndentation <- A.takeWhile CharPredicates.isSpaceOrTab
    let extraIndentationSize = Text.length extraIndentation
        linesState =
          if extraIndentationSize == 0
            then LinesState lineNum indentationNum indentationText
            else LinesState lineNum (indentationNum + extraIndentationSize) (indentationText <> extraIndentation)
    (res, linesState) <- runLines linesState
    return ((extraIndentation, res), linesState)

-- |
-- Parse a single line starting at the indentation of the current level.
--
-- If the current line is not indented enough,
-- it is the same as reaching the end of input, so this parser will fail.
line :: Line a -> Lines a
line (Line runLine) =
  Lines $ \(LinesState lineNum indentationNum indentationText) -> do
    unless (indentationNum == 0) $ void $ A.string indentationText
    (res, columnNum) <- runLine lineNum indentationNum
    eolP <|> A.endOfInput
    return (res, (LinesState (succ lineNum) indentationNum indentationText))
  where
    eolP =
      void (A.char '\n') <|> (A.char '\r' *> (void (A.char '\n') <|> pure ()))

-- *

-- |
-- Parser in the context of a single line with indentation of the scope applied.
--
-- Reaching a line end is the same as reaching end of input.
newtype Line a
  = Line
      ( -- Line offset.
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
  Line $ \line column ->
    pure ((line, column), column)

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
