module Coalmine.Name.Gens where

import Coalmine.InternalPrelude
import qualified Coalmine.Name.Charsets as Charsets
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as BVec
import Test.QuickCheck.Gen

parts :: Int -> Int -> Gen (BVec Text)
parts maxParts maxBytesInPart = do
  partsAmount <- chooseInt (1, maxParts)
  BVec.replicateM partsAmount (part maxBytesInPart)

part :: Int -> Gen Text
part maxBytesInPart = go
  where
    go = do
      partSize <- chooseInt (1, maxBytesInPart)
      text <- Text.pack <$> replicateM partSize partChar
      if ByteString.length (TextEncoding.encodeUtf8 text) > maxBytesInPart
        then go
        else return text

partChar :: Gen Char
partChar =
  elements $ toList $ Charsets.part
