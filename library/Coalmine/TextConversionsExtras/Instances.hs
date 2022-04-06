module Coalmine.TextConversionsExtras.Instances where

import Coalmine.InternalPrelude
import qualified Data.Text as Text

instance ToText Char where
  toText = Text.singleton
