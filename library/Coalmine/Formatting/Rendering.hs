module Coalmine.Formatting.Rendering where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.Building
import Coalmine.Prelude hiding (intercalate, null)
import qualified Data.Text as Text
import qualified TextBuilder as Tb

-- |
-- Rendering input tree.
data Tree
  = -- | Vector of isolated line contents.
    MultilineTree !(BVec Tb.TextBuilder)
  | IndentedTree !Int !Tree
  | MergedTree !Tree !Tree

render :: Tree -> Tb.TextBuilder
render = renderAtIndent 0

renderAtIndent :: Int -> Tree -> Tb.TextBuilder
renderAtIndent indent = \case
  MultilineTree lines -> error "TODO"
  IndentedTree indent' tree -> renderAtIndent (indent + indent') tree
  MergedTree left right -> renderAtIndent indent left <> renderAtIndent indent right
