module Coalmine.TextAppender
  ( -- *
    TextAppender,
    finalize,
    append,
  )
where

import Coalmine.InternalPrelude

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

append :: Text -> TextAppender -> TextAppender
append chunk (TextAppender builder) =
  TextAppender $ builder <> fromText chunk
