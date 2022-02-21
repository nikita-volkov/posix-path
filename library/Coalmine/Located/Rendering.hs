module Coalmine.Located.Rendering where

import Coalmine.Inter
import Coalmine.Prelude
import qualified Coalmine.TextAppender as TextAppender
import qualified Data.Text as Text

-- 5:1:
-- 5 | create type "language" as enum ('c', 'c#', 'c++');
--   | ^
-- unexpected 'c'
-- expecting "--", "/*", end of input, or white space

megaparsecErrorMessageLayout startLine startColumn =
  [i|
    $startLine:$startColumn:
    $startLine
  |]

select inputLines startFirstLineOffset endLastLineOffset =
  case inputLines of
    linesHead : linesTail ->
      linesHead : firstLineCursors : buildTail linesTail
      where
        firstLineCursors =
          -- Last line?
          if null linesTail
            then
              Text.replicate startFirstLineOffset " "
                <> Text.replicate (endLastLineOffset - startFirstLineOffset) "^"
            else
              Text.replicate startFirstLineOffset " "
                <> Text.replicate (Text.length linesHead - startFirstLineOffset) "^"
        buildTail = \case
          linesHead : linesTail ->
            -- Last line?
            if null linesTail
              then linesHead : lastLineCursors : []
              else linesHead : intermediateLineCursors : buildTail linesTail
            where
              lastLineCursors =
                Text.replicate endLastLineOffset "^"
              intermediateLineCursors =
                Text.replicate (Text.length linesHead) "^"
          [] -> []
    [] -> []
