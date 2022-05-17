-- |
-- Instances for the externally defined data-types used across this lib.
module Coalmine.IsomorphismClassInstances where

import Coalmine.InternalPrelude
import qualified Coalmine.TimeExtras.Conversions as TimeConversions
import Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified Data.Vector.Generic as VectorGeneric
import qualified Data.Vector.Unboxed as VectorUnboxed
import qualified TextBuilderDev

--

instance IsomorphicTo a b => IsomorphicTo (Deque a) (Deque b) where
  to = fmap to

instance IsomorphicTo (Deque a) [a] where
  to = fromList

instance IsomorphicTo (Deque a) (BVec a) where
  to = from @[a] . to

--

instance IsomorphicTo TextBuilder TextBuilder where
  to = id

instance IsomorphicTo TextBuilder String where
  to = TextBuilderDev.string

instance IsomorphicTo TextBuilder Text where
  to = TextBuilderDev.text

instance IsomorphicTo TextBuilder TextLazy.Text where
  to = TextBuilderDev.lazyText

instance IsomorphicTo TextBuilder TextLazyBuilder.Builder where
  to = to . to @TextLazy.Text

--

instance IsomorphicTo String TextBuilder where
  to = to . to @Text

instance IsomorphicTo [a] (Deque a) where
  to = toList

--

instance IsomorphicTo (BVec a) (Deque a) where
  to = to . to @[a]

--

instance IsomorphicTo Text TextBuilder where
  to = TextBuilderDev.buildText

--

instance IsomorphicTo TextLazy.Text TextBuilder where
  to = to . to @Text

--

instance IsomorphicTo TextLazyBuilder.Builder TextBuilder where
  to = to . to @Text
