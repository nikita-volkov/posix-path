module Coalmine.TextConversionsExtras.Instances where

import qualified Data.Text as Text
import Data.Text.Conversions
import Prelude

instance ToText Char where
  toText = Text.singleton
