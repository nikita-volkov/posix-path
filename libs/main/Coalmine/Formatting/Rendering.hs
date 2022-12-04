module Coalmine.Formatting.Rendering where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.Building
import Coalmine.InternalPrelude hiding (intercalate, null)
import Data.Text qualified as Text
import TextBuilderDev qualified as Tb

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
