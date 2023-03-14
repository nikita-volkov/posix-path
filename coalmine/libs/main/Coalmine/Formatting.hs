module Coalmine.Formatting where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.Building
import Coalmine.Formatting.Rendering qualified as Finalization
import Coalmine.InternalPrelude hiding (intercalate, null)
import Data.Text qualified as Text
import TextBuilderDev qualified as Tb

data Multiline

indent :: Int -> Multiline -> Multiline
indent = error "TODO"

line :: Uniline -> Multiline
line = error "TODO"

data Uniline
