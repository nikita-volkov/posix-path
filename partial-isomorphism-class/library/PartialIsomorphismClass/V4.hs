module PartialIsomorphismClass.V4 where

import Coalmine.Prelude
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Encoding

class Decoding a where
  type Encoded a
  type DecodingError a
  encode :: a -> Encoded a
  decode :: Encoded a -> Either (DecodingError a) a

instance Decoding Text where
  type Encoded Text = ByteString
  type DecodingError Text = Text.Encoding.UnicodeException
  encode = Text.Encoding.encodeUtf8
  decode = Text.Encoding.decodeUtf8'
