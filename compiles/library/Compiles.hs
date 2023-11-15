module Compiles where

import Data.ByteString.Builder qualified
import Data.ByteString.Lazy qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import TextBuilderDev qualified
import Prelude

-- | Evidence of a type having a single possible assembled projection.
--
-- All builders have it, but not only.
class Compiles a where
  type Compiled a
  compile :: a -> Compiled a
  disperse :: Compiled a -> a

instance Compiles TextBuilderDev.TextBuilder where
  type Compiled TextBuilderDev.TextBuilder = Text
  compile = TextBuilderDev.buildText
  disperse = TextBuilderDev.text

instance Compiles Data.Text.Lazy.Builder.Builder where
  type Compiled Data.Text.Lazy.Builder.Builder = Data.Text.Lazy.Text
  compile = Data.Text.Lazy.Builder.toLazyText
  disperse = Data.Text.Lazy.Builder.fromLazyText

instance Compiles Data.Text.Lazy.Text where
  type Compiled Data.Text.Lazy.Text = Text
  compile = Data.Text.Lazy.toStrict
  disperse = Data.Text.Lazy.fromStrict

instance Compiles Data.ByteString.Builder.Builder where
  type Compiled Data.ByteString.Builder.Builder = Data.ByteString.Lazy.ByteString
  compile = Data.ByteString.Builder.toLazyByteString
  disperse = Data.ByteString.Builder.lazyByteString

instance Compiles Data.ByteString.Lazy.ByteString where
  type Compiled Data.ByteString.Lazy.ByteString = ByteString
  compile = Data.ByteString.Lazy.toStrict
  disperse = Data.ByteString.Lazy.fromStrict
