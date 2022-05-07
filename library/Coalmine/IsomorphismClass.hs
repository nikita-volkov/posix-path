module Coalmine.IsomorphismClass where

import Coalmine.InternalPrelude
import Coalmine.TextIsomorphism
import qualified Coalmine.TimeExtras.Conversions as TimeConversions
import Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder

-- * --

-- |
-- Bidirectional conversion between two types with no loss of information.
--
-- Unlike conversion classes from other libs this class is lawful.
-- The law is:
--
-- @'from' . 'to' = 'id'@
class IsomorphicTo a b where
  to :: b -> a
  from :: a -> b

instance IsomorphicTo String Text where
  to = fromText
  from = toText

instance IsomorphicTo Text String where
  to = toText
  from = fromText

instance IsomorphicTo Text TextBuilder where
  to = toText
  from = fromText

instance IsomorphicTo Text TextLazy.Text where
  to = toText
  from = fromText

instance IsomorphicTo Text TextLazyBuilder.Builder where
  to = toText
  from = fromText

instance IsomorphicTo TextBuilder Text where
  to = fromText
  from = toText

instance IsomorphicTo TextLazy.Text Text where
  to = fromText
  from = toText

instance IsomorphicTo TextLazyBuilder.Builder Text where
  to = fromText
  from = toText
