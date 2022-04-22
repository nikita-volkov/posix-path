module Coalmine.TextIsomorphism where

import Coalmine.InternalPrelude
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder

class IsomorphicToText a where
  toText :: a -> Text
  fromText :: Text -> a

instance IsomorphicToText Text where
  toText = id
  fromText = id

instance IsomorphicToText String where
  toText = fromString
  fromText = Text.unpack

instance IsomorphicToText TextBuilder where
  toText = fromTextBuilder
  fromText = toTextBuilder

instance IsomorphicToText TextLazy.Text where
  toText = TextLazy.toStrict
  fromText = TextLazy.fromStrict

instance IsomorphicToText TextLazyBuilder.Builder where
  toText = TextLazy.toStrict . TextLazyBuilder.toLazyText
  fromText = TextLazyBuilder.fromText
