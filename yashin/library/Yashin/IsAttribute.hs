module Yashin.IsAttribute where

import Amazonka qualified as Azk
import Amazonka.DynamoDB qualified as Azk
import Coalmine.Prelude
import Data.Scientific qualified as Scientific
import Data.Text.Lazy.Builder.Scientific qualified
import TextBuilderDev qualified
import Yashin.Parsers.Attributes qualified as D

class IsAttribute a where
  encode :: a -> Azk.AttributeValue
  decoder :: D.Value a

instance IsAttribute Bool where
  encode = Azk.BOOL
  decoder = D.bool Just

instance IsAttribute Text where
  encode = Azk.S
  decoder = D.string Right

instance IsAttribute ByteString where
  encode = Azk.B . Azk.Base64
  decoder = D.binary Right

instance (Integral a, Bounded a) => IsAttribute a where
  encode = Azk.N . to . TextBuilderDev.decimal
  decoder = D.number (maybe (Left "Out of bounds") Right . Scientific.toBoundedInteger)

instance (IsAttribute a) => IsAttribute (BVec a) where
  encode = Azk.L . fmap encode
  decoder = D.list decoder

instance IsAttribute (Set ByteString) where
  encode = Azk.BS . fromList . fmap Azk.Base64 . toList
  decoder = D.binarySet Right

instance IsAttribute (Set Text) where
  encode = Azk.SS . fromList . toList
  decoder = D.stringSet Right

instance IsAttribute (Set Scientific) where
  encode = Azk.NS . fromList . fmap (to . Data.Text.Lazy.Builder.Scientific.scientificBuilder) . toList
  decoder = D.numberSet Right

instance (Integral a, Bounded a) => IsAttribute (Set a) where
  encode = Azk.NS . fromList . fmap (to . TextBuilderDev.decimal) . toList
  decoder = D.numberSet (maybe (Left "Out of bounds") Right . Scientific.toBoundedInteger)
