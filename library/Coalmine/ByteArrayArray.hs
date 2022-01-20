module Coalmine.ByteArrayArray where

import Coalmine.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Internal as TextInternal
import qualified Data.Vector.Unboxed as UVec
import GHC.Exts

-- *

-- |
-- A space-efficient representation of an array of byte-arrays.
--
-- Useful for representing an array of 'Text' for instance.
data ByteArrayArray = ByteArrayArray
  { -- | Bytes.
    array :: !ByteArray#,
    -- | Ends.
    ends :: !(UVec Int)
  }

-- *

{-# INLINE toTextList #-}
toTextList :: ByteArrayArray -> [Text]
toTextList ByteArrayArray {..} =
  build $ \step start ->
    let step' end next offset =
          step (TextInternal.Text textArray offset (end - offset)) (next end)
        finish _ = start
     in UVec.foldr step' finish ends 0
  where
    textArray = TextArray.Array array
