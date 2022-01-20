module Coalmine.ByteArrayArray where

import Coalmine.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Internal as TextInternal
import GHC.Exts

-- *

-- |
-- A space-efficient representation of an array of byte-arrays.
--
-- Useful for representing an array of 'Text' for instance.
data ByteArrayArray = ByteArrayArray
  { -- | Bytes.
    array :: !ByteArray#,
    -- | Offsets.
    offsets :: !(UVec Int),
    -- | Length.
    length :: !Int
  }

-- *

toTextList :: ByteArrayArray -> [Text]
toTextList ByteArrayArray {..} =
  error "TODO"
