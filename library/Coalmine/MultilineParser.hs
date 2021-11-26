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
      !Bool
      -- ^ Whether we're at the first line.
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
-- Detects the extra indentation on the current line
-- and runs the provided parser in the context of this indentation
-- having already been applied.
--
-- In other words in the lifted parser you should assume that you're
-- dealing with the content as if it is not indented.
--
-- Returns the specific characters detected as indentation
-- in the first line of this block and unapplied throughout it.
indented :: Lines a -> Lines (Text, a)
indented (Lines runLines) =
  Lines $ \(LinesState isFirstLine lineNum indentationNum indentationText) -> do
    unless (indentationNum == 0) $ void $ A.string indentationText
    extraIndentation <- A.takeWhile CharPredicates.isSpaceOrTab
    let extraIndentationSize = Text.length extraIndentation
        linesState =
          if extraIndentationSize == 0
            then LinesState True lineNum indentationNum indentationText
            else LinesState True lineNum (indentationNum + extraIndentationSize) (indentationText <> extraIndentation)
    (res, linesState) <- runLines linesState
    case linesState of
      LinesState nestedIsFirstLine lineNum _ _ ->
        let linesState =
              LinesState
                (isFirstLine && nestedIsFirstLine)
                lineNum
                indentationNum
                indentationText
         in return ((extraIndentation, res), linesState)

-- |
-- Parse a single line starting at the indentation of the current level.
-- The line parser must consume the input to the end of the line.
--
-- If the current line is not indented enough,
-- it is the same as reaching the end of input, so this parser will fail.
line :: Line a -> Lines a
line (Line runLine) =
  Lines $ \(LinesState isFirstLine lineNum indentationNum indentationText) -> do
    unless (isFirstLine || indentationNum == 0) $ void $ A.string indentationText
    (res, columnNum) <- error "TODO"
    eolP <|> A.endOfInput
    return (res, (LinesState False (succ lineNum) indentationNum indentationText))
  where
    eolP =
      void (A.char '\n') <|> (A.char '\r' *> (void (A.char '\n') <|> pure ()))

-- |
-- Get the current line offset.
-- Use this to associate results with location in the input.
--
-- One typical application of this is in multi-stage parsers.
-- This function lets you relate errors from further stages
-- with a specific location in the parsed source code.
lineOffset :: Lines Int
lineOffset = error "TODO"

-- *

-- |
-- Parser in the context of a single line with indentation of the scope applied.
--
-- Reaching a line end is the same as reaching end of input.
newtype Line a
  = Line
      ( -- Column offset.
        Int ->
        -- Remaining input on the line.
        -- Already ensured not to contain newlines.
        Text ->
        -- Parser producing result and new column offset
        -- or failing with the colum offset at the error.
        Either (Text, Int) (Text, Int, a)
      )

-- *

-- |
-- Current location.
-- Use this to associate results with location in the input.
--
-- One typical application of this is in multi-stage parsers.
-- This function lets you relate errors from further stages
-- with a specific location in the parsed source code.
columnOffset :: Line Int
columnOffset = error "TODO"

-- |
-- Narrow a char.
char :: (Char -> Maybe a) -> Line a
char narrow =
  error "TODO"

takeWhile :: (Char -> Bool) -> Line Text
takeWhile p =
  error "TODO"

skipWhile :: (Char -> Bool) -> Line ()
skipWhile p =
  error "TODO"

-- |
-- It is your responsibility to ensure that the matched text
-- does not contain newline characters.
exactString :: Text -> Line a
exactString = error "TODO"

-- *

-- |
-- Location pointing to a chunk of data
-- possibly spanning multiple lines.
data Selection
  = SingleLineSelection
      !Int
      -- ^ Line.
      !Int
      -- ^ Start column.
      !Int
      -- ^ End column.
  | MultilineSelection
      !Int
      -- ^ Start line.
      !Int
      -- ^ Start column.
      !Int
      -- ^ End line.
      !Int
      -- ^ End column.
  | UnendedSelection
      !Int
      -- ^ Start line.
      !Int
      -- ^ Start column.

data Located a
  = Located !Selection a
