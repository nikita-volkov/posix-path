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
class IsomophicTo a b where
  to :: a -> b
  from :: b -> a

instance IsomophicTo String Text where
  to = toText
  from = fromText

instance IsomophicTo Text String where
  to = fromText
  from = toText

instance IsomophicTo Text TextBuilder where
  to = fromText
  from = toText

instance IsomophicTo Text TextLazy.Text where
  to = fromText
  from = toText

instance IsomophicTo Text TextLazyBuilder.Builder where
  to = fromText
  from = toText

instance IsomophicTo TextBuilder Text where
  to = toText
  from = fromText

instance IsomophicTo TextLazy.Text Text where
  to = toText
  from = fromText

instance IsomophicTo TextLazyBuilder.Builder Text where
  to = toText
  from = fromText
