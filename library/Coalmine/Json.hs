module Coalmine.Json where

import Coalmine.InternalPrelude
import Jsonifier qualified as Jf

-- |
-- JSON literal.
--
-- A more correct way to represent it than the one in the \"aeson\" lib.
data Json
  = ObjectJson !(BVec (Text, Json))
  | ArrayJson !(BVec Json)
  | StringJson !Text
  | NumberJson !Scientific
  | TrueJson
  | FalseJson
  | NullJson

toUtf8ByteString :: Json -> ByteString
toUtf8ByteString = Jf.toByteString . toJsonifier

toJsonifier :: Json -> Jf.Json
toJsonifier = \case
  ObjectJson a -> Jf.object (fmap (second toJsonifier) a)
  ArrayJson a -> Jf.array (fmap toJsonifier a)
  StringJson a -> Jf.textString a
  NumberJson a -> Jf.scientificNumber a
  TrueJson -> Jf.true
  FalseJson -> Jf.false
  NullJson -> Jf.null
