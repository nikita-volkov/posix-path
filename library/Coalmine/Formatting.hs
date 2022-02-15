module Coalmine.Formatting where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.Building
import qualified Coalmine.Formatting.Rendering as Finalization
import Coalmine.InternalPrelude hiding (intercalate, null)
import qualified Data.Text as Text
import qualified TextBuilder as Tb

data Multiline

indent :: Int -> Multiline -> Multiline
indent = error "TODO"

line :: Uniline -> Multiline
line = error "TODO"

data Uniline
