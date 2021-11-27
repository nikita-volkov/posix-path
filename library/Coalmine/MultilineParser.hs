-- |
-- Indentation-sensitive line-by-line parsing.
module Coalmine.MultilineParser where

import qualified Coalmine.CharPredicates as CharPredicates
import Coalmine.Prelude hiding (maybe)
import qualified Coalmine.VectorExtras.Generic as Vec
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as M

-- *

data BlockState
  = BlockState
      !Bool
      -- ^ Whether we're at the first line.
      !Int
      -- ^ Line.
      !Int
      -- ^ Indentation in amount of chars.
      !Text
      -- ^ Specific chars used for indentation.

-- *

parseBlock :: Block a -> Text -> Either Text a
parseBlock =
  error "TODO"

newtype Block a
  = Block (BlockState -> A.Parser (a, BlockState))
  deriving
    (Functor, Applicative, Alternative, Monad, MonadPlus)
    via (StateT BlockState A.Parser)

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
indented :: Block a -> Block (Text, a)
indented (Block runBlock) =
  Block $ \(BlockState isFirstLine lineNum indentationNum indentationText) -> do
    unless (indentationNum == 0) $ void $ A.string indentationText
    extraIndentation <- A.takeWhile CharPredicates.isSpaceOrTab
    let extraIndentationSize = Text.length extraIndentation
        linesState =
          if extraIndentationSize == 0
            then BlockState True lineNum indentationNum indentationText
            else BlockState True lineNum (indentationNum + extraIndentationSize) (indentationText <> extraIndentation)
    (res, linesState) <- runBlock linesState
    case linesState of
      BlockState nestedIsFirstLine lineNum _ _ ->
        let linesState =
              BlockState
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
line :: Line a -> Block a
line (Line runLine) =
  Block $ \(BlockState isFirstLine lineNum indentationNum indentationText) -> do
    unless (isFirstLine || indentationNum == 0) $ void $ A.string indentationText
    (res, columnNum) <- runLine lineNum indentationNum
    eolP <|> A.endOfInput
    return (res, (BlockState False (succ lineNum) indentationNum indentationText))
  where
    eolP =
      void (A.char '\n') <|> (A.char '\r' *> (void (A.char '\n') <|> pure ()))

-- *

-- |
-- Parser in the context of a single line with indentation of the scope applied.
--
-- Reaching a line end is the same as reaching end of input.
--
-- No Alternative instance is provided,
-- but alternation can be achieved using 'oneOfLines'.
newtype Line a
  = Line
      ( -- Line offset.
        Int ->
        -- Column offset.
        Int ->
        -- Parser producing result and new column offset.
        A.Parser (a, Int)
      )
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT Int (StateT Int A.Parser))

-- *

-- |
-- Current location.
-- Use this to associate results with location in the input.
--
-- One typical application of this is in multi-stage parsers.
-- This function lets you relate errors from further stages
-- with a specific location in the parsed source code.
locatedOnLine :: Line a -> Line (Located a)
locatedOnLine (Line runLine) =
  Line $ \line column ->
    runLine line column <&> \(res, endColumn) ->
      (Located (SingleLineSelection line column endColumn) res, endColumn)

-- |
-- Narrow a char.
narrowedChar :: (Char -> Maybe a) -> Line a
narrowedChar narrow =
  Line $ \line column -> do
    Just res <- A.satisfyWith narrow' isJust
    case succ column of
      column -> return (res, column)
  where
    narrow' x =
      if x == '\r' || x == '\n'
        then Nothing
        else narrow x

validatedChar :: (Char -> Bool) -> Line Char
validatedChar predicate =
  Line $ \line column -> do
    char <- A.satisfy predicate'
    case succ column of
      column -> return (char, column)
  where
    predicate' x =
      x /= '\n' && x /= '\r' && predicate x

specificChar :: Char -> Line ()
specificChar char =
  Line $ \line column -> do
    A.char char
    case succ column of
      column -> return ((), column)

takeWhile :: (Char -> Bool) -> Line Text
takeWhile p =
  Line $ \_ column -> A.runScanner column step
  where
    step column char =
      if char /= '\n' && char /= '\r' && p char
        then Just $! succ column
        else Nothing

skipWhile :: (Char -> Bool) -> Line ()
skipWhile p =
  error "TODO"

-- |
-- It is your responsibility to ensure that the matched text
-- does not contain newline characters.
exactString :: Text -> Line ()
exactString = error "TODO"

-- |
-- Expect one of multiple named line contents.
oneOfLines :: [(Text, Line a)] -> Line a
oneOfLines = error "TODO"

maybe :: Line a -> Line (Maybe a)
maybe = error "TODO"

-- * Combinators

many ::
  -- |
  -- Update the accumulator.
  -- Applied to every element after the first one.
  (acc -> elem -> acc) ->
  -- |
  -- Initial accumulator.
  acc ->
  -- |
  -- Parse the element.
  -- Nothing signals to exit the loop.
  Line (Maybe elem) ->
  Line acc
many step acc elem =
  go acc
  where
    go !acc = do
      _elem <- elem
      case _elem of
        Just _elem -> go (step acc _elem)
        Nothing -> return acc

sepBy ::
  -- |
  -- Update the accumulator.
  -- Applied to every element after the first one.
  (acc -> elem -> acc) ->
  -- |
  -- Convert the first element into the initial accumulator.
  acc ->
  -- |
  -- Separator parser signaling whether a separator has been
  -- successfully consumed.
  --
  -- False signals to exit the loop.
  --
  -- This allows us to avoid the need in Alternative.
  Line Bool ->
  Line (Maybe elem) ->
  Line elem ->
  Line acc
sepBy step acc sep firstElem elem =
  do
    _firstElem <- firstElem
    case _firstElem of
      Just _firstElem -> go (step acc _firstElem)
      Nothing -> return acc
  where
    go !acc = do
      _sep <- sep
      if _sep
        then do
          _elem <- elem
          go (step acc _elem)
        else return acc

sepBy1 ::
  -- |
  -- Update the accumulator.
  -- Applied to every element after the first one.
  (acc -> elem -> acc) ->
  -- |
  -- Convert the first element into the initial accumulator.
  (elem -> acc) ->
  -- |
  -- Separator parser signaling whether a separator has been
  -- successfully consumed.
  --
  -- False signals to exit the loop.
  --
  -- This allows us to avoid the need in Alternative.
  Line Bool ->
  Line elem ->
  Line acc
sepBy1 step map sep elem =
  do
    head <- elem
    go (map head)
  where
    go !acc = do
      _sep <- sep
      if _sep
        then do
          _elem <- elem
          go (step acc _elem)
        else return acc

vecSepBy1 ::
  Vector v elem =>
  -- |
  -- Separator parser signaling whether a separator has been
  -- successfully consumed.
  --
  -- False signals to exit the loop.
  --
  -- This allows us to avoid the need in Alternative.
  Line Bool ->
  Line elem ->
  Line (v elem)
vecSepBy1 sep elem =
  sepBy1 step map sep elem <&> extract
  where
    map elem = (1, [elem])
    step (!n, !list) elem =
      (succ n, elem : list)
    extract (n, list) =
      Vec.fromReverseListN n list

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
