module Coalmine.ToTextBuilder where

import TextBuilder
import Prelude

class ToTextBuilder a where
  toTextBuilder :: a -> TextBuilder

instance ToTextBuilder TextBuilder where
  toTextBuilder = id

instance ToTextBuilder Text where
  toTextBuilder = text

instance ToTextBuilder String where
  toTextBuilder = fromString
