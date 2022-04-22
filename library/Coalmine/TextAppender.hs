module Coalmine.TextAppender
  ( -- *
    TextAppender,
    init,
    append,
    finalize,
  )
where

import Coalmine.InternalPrelude hiding (init)

-- *

-- TODO: reimplement it as
-- data TextAppender
--   = TextAppender
--       !Int
--       -- ^ Allocation size.
--       ![Text]
--       -- ^ Chunks.

-- |
-- Data structure optimized specifically for collecting text chunk by chunk
-- via appending.
newtype TextAppender
  = TextAppender TextBuilder

finalize :: TextAppender -> Text
finalize (TextAppender builder) =
  buildText builder

init :: TextAppender
init =
  TextAppender mempty

append :: Text -> TextAppender -> TextAppender
append chunk (TextAppender builder) =
  TextAppender $ builder <> toTextBuilder chunk
