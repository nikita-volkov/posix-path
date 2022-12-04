module Coalmine.MarkRight.Builder
  ( Builder,
    freeze,
  )
where

import Coalmine.InternalPrelude
import Coalmine.MarkRight.Ast qualified as Ast
import Data.Text qualified as Text

newtype Builder
  = Builder (Acc Ast.Node)
  deriving (Semigroup, Monoid)

freeze :: Builder -> Ast.Tree
freeze =
  error "TODO"
