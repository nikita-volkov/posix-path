{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods #-}

module UnicodeClass where

import Data.ByteString qualified
import Data.ByteString.Lazy qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Text.Encoding.Error qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Encoding qualified
import Prelude

class Unicode a where
  type UnicodeEncoded a
  encodeUtf8 :: a -> UnicodeEncoded a
  decodeUtf8 :: UnicodeEncoded a -> Maybe a
  decodeUtf8Lenient :: UnicodeEncoded a -> a

instance Unicode Data.Text.Text where
  type UnicodeEncoded Data.Text.Text = Data.ByteString.ByteString
  encodeUtf8 = Data.Text.Encoding.encodeUtf8
  decodeUtf8 = either (const Nothing) Just . Data.Text.Encoding.decodeUtf8'
  decodeUtf8Lenient = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode

instance Unicode Data.Text.Lazy.Text where
  type UnicodeEncoded Data.Text.Lazy.Text = Data.ByteString.Lazy.ByteString
  encodeUtf8 = Data.Text.Lazy.Encoding.encodeUtf8
  decodeUtf8 = either (const Nothing) Just . Data.Text.Lazy.Encoding.decodeUtf8'
  decodeUtf8Lenient = Data.Text.Lazy.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
