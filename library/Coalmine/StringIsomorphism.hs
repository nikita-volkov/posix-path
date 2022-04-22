module Coalmine.StringIsomorphism where

import Coalmine.InternalPrelude
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder

class IsString a => IsomorphicToString a where
  toString :: a -> String

instance IsomorphicToString String where
  toString = id

instance IsomorphicToString Text where
  toString = Text.unpack

instance IsomorphicToString TextBuilder where
  toString = fromTextBuilder

instance IsomorphicToString TextLazy.Text where
  toString = TextLazy.unpack

instance IsomorphicToString TextLazyBuilder.Builder where
  toString = TextLazy.unpack . TextLazyBuilder.toLazyText
