module Coalmine.TextAppender
  ( -- *
    TextAppender,
    finalize,
    append,
  )
where

import Prelude

-- *

-- |
-- Data structure optimized specifically for collecting text chunk by chunk
-- via appending.
data TextAppender
  = TextAppender
      !Int
      -- ^ Allocation size.
      ![Text]
      -- ^ Chunks.

finalize :: TextAppender -> Text
finalize =
  error "TODO"

append :: Text -> TextAppender -> TextAppender
append =
  error "TODO"
