module Coalmine.Name.Gens where

import Coalmine.InternalPrelude
import qualified Coalmine.Name.Charsets as Charsets
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as BVec
import qualified StructureKit.Charset as Charset
import Test.QuickCheck.Gen

normalParts :: Int -> Int -> Gen (BVec Text)
normalParts maxParts maxBytesInPart = do
  partsAmount <- chooseInt (1, maxParts)
  BVec.replicateM partsAmount (normalPart maxBytesInPart)

onlyFirstAlphaFirstParts :: Int -> Int -> Gen (BVec Text)
onlyFirstAlphaFirstParts maxParts maxBytesInPart = do
  head <- alphaFirstPart maxBytesInPart
  tail <- do
    tailSize <- chooseInt (0, pred maxParts)
    replicateM tailSize (normalPart maxBytesInPart)
  return $ fromList (head : tail)

allAlphaFirstParts :: Int -> Int -> Gen (BVec Text)
allAlphaFirstParts maxParts maxBytesInPart = do
  partsAmount <- chooseInt (1, maxParts)
  BVec.replicateM partsAmount (alphaFirstPart maxBytesInPart)

normalPart :: Int -> Gen Text
normalPart maxBytesInPart = go
  where
    go = do
      partSize <- chooseInt (1, maxBytesInPart)
      text <- Text.pack <$> replicateM partSize partChar
      if ByteString.length (TextEncoding.encodeUtf8 text) > maxBytesInPart
        then go
        else return text

alphaFirstPart :: Int -> Gen Text
alphaFirstPart maxBytesInPart = go
  where
    go = do
      tailSize <- chooseInt (1, maxBytesInPart)
      text <-
        Text.pack <$> do
          head <- lowerLatinPartChar
          tail <- replicateM tailSize partChar
          return (head : tail)
      if ByteString.length (TextEncoding.encodeUtf8 text) > maxBytesInPart
        then go
        else return text

partChar :: Gen Char
partChar =
  elements $ toList $ Charsets.part

lowerLatinPartChar :: Gen Char
lowerLatinPartChar =
  elements $ toList $ Charset.lowerLatin
