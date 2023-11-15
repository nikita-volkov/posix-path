module Frost where

import Data.ByteString.Builder qualified
import Data.ByteString.Lazy qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import TextBuilderDev qualified
import Prelude

-- | Evidence of a type having a single possible frozen projection.
--
-- All builders have it, but not only.
class Freezes a where
  type Frozen a
  freeze :: a -> Frozen a
  melt :: Frozen a -> a

instance Freezes TextBuilderDev.TextBuilder where
  type Frozen TextBuilderDev.TextBuilder = Text
  freeze = TextBuilderDev.buildText
  melt = TextBuilderDev.text

instance Freezes Data.Text.Lazy.Builder.Builder where
  type Frozen Data.Text.Lazy.Builder.Builder = Data.Text.Lazy.Text
  freeze = Data.Text.Lazy.Builder.toLazyText
  melt = Data.Text.Lazy.Builder.fromLazyText

instance Freezes Data.Text.Lazy.Text where
  type Frozen Data.Text.Lazy.Text = Text
  freeze = Data.Text.Lazy.toStrict
  melt = Data.Text.Lazy.fromStrict

instance Freezes Data.ByteString.Builder.Builder where
  type Frozen Data.ByteString.Builder.Builder = Data.ByteString.Lazy.ByteString
  freeze = Data.ByteString.Builder.toLazyByteString
  melt = Data.ByteString.Builder.lazyByteString

instance Freezes Data.ByteString.Lazy.ByteString where
  type Frozen Data.ByteString.Lazy.ByteString = ByteString
  freeze = Data.ByteString.Lazy.toStrict
  melt = Data.ByteString.Lazy.fromStrict
