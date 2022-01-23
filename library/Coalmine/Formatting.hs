module Coalmine.Formatting where

import Coalmine.Building
import qualified Coalmine.Formatting.Rendering as Finalization
import Coalmine.InternalPrelude hiding (intercalate, null)
import qualified Coalmine.List as List
import qualified Data.Text as Text
import qualified TextBuilder as Tb

data Multiline

indent :: Int -> Multiline -> Multiline
indent = error "TODO"

line :: Uniline -> Multiline
line = error "TODO"

data Uniline
