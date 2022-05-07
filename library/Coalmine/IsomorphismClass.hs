module Coalmine.IsomorphismClass where

import Coalmine.InternalPrelude
import Coalmine.TextIsomorphism
import qualified Coalmine.TimeExtras.Conversions as TimeConversions
import Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder

-- |
-- Bidirectional conversion between two types with no loss of information.
--
-- Unlike conversion classes from other libs this class is lawful.
-- The law is:
--
-- @'from' . 'to' = 'id'@
--
-- This class is particularly easy to use in combination with the @TypeApplications@ extension
-- making it clear to the reader what sort of conversion he sees. E.g.,
--
-- > fromString = from @String
--
-- > toText = to @Text
--
-- The types are also self-evident:
--
-- > > :t from @String
-- > from @String :: IsomorphicTo String b => String -> b
--
-- > > :t to @Text
-- > to @Text :: IsomorphicTo Text b => b -> Text
class IsomorphicTo a b where
  to :: b -> a
  from :: a -> b

instance IsomorphicTo String String where
  to = id
  from = id

instance IsomorphicTo String Text where
  to = fromText
  from = toText

instance IsomorphicTo String TextBuilder where
  to = from @Text . to @Text
  from = from @Text . to @Text

instance IsomorphicTo String TextLazy.Text where
  to = TextLazy.unpack
  from = TextLazy.pack

instance IsomorphicTo String TextLazyBuilder.Builder where
  to = TextLazy.unpack . TextLazyBuilder.toLazyText
  from = TextLazyBuilder.fromString

instance IsomorphicTo Text Text where
  to = id
  from = id

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

instance IsomorphicTo TextBuilder TextBuilder where
  to = id
  from = id

instance IsomorphicTo TextBuilder String where
  to = from @String
  from = to @String

instance IsomorphicTo TextBuilder Text where
  to = toTextBuilder
  from = fromTextBuilder

instance IsomorphicTo TextLazy.Text TextLazy.Text where
  to = id
  from = id

instance IsomorphicTo TextLazy.Text String where
  to = fromString
  from = TextLazy.unpack

instance IsomorphicTo TextLazy.Text Text where
  to = fromText
  from = toText

instance IsomorphicTo TextLazyBuilder.Builder TextLazyBuilder.Builder where
  to = id
  from = id

instance IsomorphicTo TextLazyBuilder.Builder String where
  to = fromString
  from = to . to @Text

instance IsomorphicTo TextLazyBuilder.Builder Text where
  to = fromText
  from = toText
