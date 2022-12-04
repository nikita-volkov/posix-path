module Coalmine.MarkRight.Ast where

import Coalmine.Inter
import Coalmine.InternalPrelude hiding (List)
import Data.Text qualified as Text

-- | AST of formatted text.
data Tree
  = Tree [Node]
  deriving (Lift, Show, Eq, Generic)

data Node
  = ParagraphNode Text
  | ListNode List
  deriving (Lift, Show, Eq, Generic)

data List = List
  { numbered :: Bool,
    head :: ListElement,
    tail :: [ListElement]
  }
  deriving (Lift, Show, Eq, Generic)

data ListElement
  = NodeListElement Node
  deriving (Lift, Show, Eq, Generic)
