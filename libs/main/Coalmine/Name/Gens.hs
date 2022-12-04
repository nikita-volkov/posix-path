module Coalmine.Name.Gens where

import Coalmine.InternalPrelude
import Coalmine.Name.Charsets qualified as Charsets
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Vector qualified as BVec
import StructureKit.Charset qualified as Charset
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
