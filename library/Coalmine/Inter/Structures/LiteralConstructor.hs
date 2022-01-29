module Coalmine.Inter.Structures.LiteralConstructor where

import qualified Coalmine.Inter.Normalization as N
import qualified Coalmine.MultilineTextBuilder as B
import Coalmine.Prelude
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax
import qualified THLego.Helpers as Helpers

-- *

data LiteralConstructor
  = LiteralConstructor
      !Int
      -- ^ Current line local indent.
      ![Text]
      -- ^ Current chunks.

-- *

start :: LiteralConstructor
start = LiteralConstructor 0 []

-- |
-- Finish, producing the indentation level of the last line.
finish :: LiteralConstructor -> (Int, Text)
finish (LiteralConstructor indent chunks) =
  let content = mconcat $ reverse chunks
   in (indent, content)

-- *

returnCarriage :: Int -> LiteralConstructor -> LiteralConstructor
returnCarriage indent (LiteralConstructor _ chunks) =
  let indentText = Text.replicate indent " "
      chunks' = indentText : "\n" : chunks
   in LiteralConstructor indent chunks'

addLineContent :: Text -> LiteralConstructor -> LiteralConstructor
addLineContent text (LiteralConstructor indent chunks) =
  LiteralConstructor indent (text : chunks)
