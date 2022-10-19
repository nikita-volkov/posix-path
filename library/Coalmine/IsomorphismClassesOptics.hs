module Coalmine.IsomorphismClassesOptics where

import Coalmine.InternalPrelude
import Coalmine.IsomorphismClasses
import qualified Data.Map
import qualified Data.Map.Strict
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import Optics

frozen :: Freezing melted frozen => Iso' melted frozen
frozen = iso freeze melt

melted :: Freezing melted frozen => Iso' frozen melted
melted = iso melt freeze
