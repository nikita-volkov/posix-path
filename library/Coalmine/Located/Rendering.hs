-- |
-- Rendering in the following style:
--
-- > 5:1:
-- > 5 | create type "language" as enum ('c', 'c#', 'c++');
-- >   | ^
-- > unexpected 'c'
-- > expecting "--", "/*", end of input, or white space
module Coalmine.Located.Rendering where

import qualified Coalmine.BaseExtras.Integer as Integer
import Coalmine.Inter
import Coalmine.Prelude
import qualified Coalmine.TextAppender as TextAppender
import qualified Data.Text as Text

render startOffset endOffset input =
  Text.foldr step finish input [] 0 0 0 Nothing False 0 0 0
  where
    step
      char
      next
      !collectedLines
      !currentOffset
      !currentLineStart
      !currentLineNum
      !earlyEnd
      startReached
      startLineNum
      startFirstLineOffset
      endLastLineOffset =
        if startReached
          then
            if currentOffset < endOffset
              then case char of
                '\r' ->
                  next
                    collectedLines
                    (succ currentOffset)
                    currentLineStart
                    currentLineNum
                    (Just currentOffset)
                    startReached
                    startLineNum
                    startFirstLineOffset
                    endLastLineOffset
                '\n' ->
                  next
                    (line : collectedLines)
                    (succ currentOffset)
                    (succ currentOffset)
                    (succ currentLineNum)
                    Nothing
                    startReached
                    startLineNum
                    startFirstLineOffset
                    endLastLineOffset
                _ ->
                  next
                    collectedLines
                    (succ currentOffset)
                    currentLineStart
                    currentLineNum
                    earlyEnd
                    startReached
                    startLineNum
                    startFirstLineOffset
                    endLastLineOffset
              else
                if char == '\r' || char == '\n'
                  then
                    finish
                      (line : collectedLines)
                      (succ currentOffset)
                      currentLineStart
                      (succ currentLineNum)
                      Nothing
                      startReached
                      startLineNum
                      startFirstLineOffset
                      currentOffset
                  else
                    next
                      collectedLines
                      (succ currentOffset)
                      currentLineStart
                      currentLineNum
                      Nothing
                      startReached
                      startLineNum
                      startFirstLineOffset
                      endLastLineOffset
          else
            if currentOffset >= startOffset
              then
                step
                  char
                  next
                  collectedLines
                  currentOffset
                  currentLineStart
                  currentLineNum
                  earlyEnd
                  True
                  startLineNum
                  currentLineStart
                  endLastLineOffset
              else error "TODO"
        where
          line =
            input
              & Text.drop currentLineStart
              & Text.take (fromMaybe currentOffset earlyEnd)
    finish collectedLines currentOffset currentLineStart currentLineNum earlyEnd startReached startLineNum startFirstLineOffset endLastLineOffset =
      error "TODO"

megaparsecErrorMessageLayout startLine startColumn quote explanation =
  [i|
    $startLine:$startColumn:
    $quote
    $explanation
  |]

select firstLineNum startFirstLineOffset endLastLineOffset inputLines =
  case inputLines of
    linesHead : linesTail ->
      contentLine : firstLineCursors : buildTail (succ firstLineNum) linesTail
      where
        contentLine =
          contentLinePrefix firstLineNum <> linesHead
        firstLineCursors =
          -- Last line?
          if null linesTail
            then
              cursorLinePrefix <> Text.replicate startFirstLineOffset " "
                <> Text.replicate (endLastLineOffset - startFirstLineOffset) "^"
            else
              cursorLinePrefix <> Text.replicate startFirstLineOffset " "
                <> Text.replicate (Text.length linesHead - startFirstLineOffset) "^"
        buildTail lineNum = \case
          linesHead : linesTail ->
            -- Last line?
            if null linesTail
              then contentLine : lastLineCursors : []
              else contentLine : intermediateLineCursors : buildTail (succ lineNum) linesTail
            where
              contentLine =
                contentLinePrefix lineNum <> linesHead
              lastLineCursors =
                cursorLinePrefix <> Text.replicate endLastLineOffset "^"
              intermediateLineCursors =
                cursorLinePrefix <> Text.replicate (Text.length linesHead) "^"
          [] -> []
    [] -> []
  where
    linesTotal = length inputLines
    lastLineNum = firstLineNum + pred linesTotal
    barOffset = Integer.countDigits lastLineNum + 1
    cursorLinePrefix = Text.replicate barOffset " " <> "| "
    contentLinePrefix n = (fromString . show) n <> Text.replicate (barOffset - Integer.countDigits n) " " <> "| "
