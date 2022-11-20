module Coalmine.Pakk.Codec where

import Coalmine.InternalPrelude
import Coalmine.Pakk.Decoding qualified as Decoding
import Coalmine.Pakk.Encoding qualified as Encoding
import Coalmine.Pakk.Schema qualified as Schema
import Data.Vector qualified as BVec

serializeAsByteStringWithSchema :: Codec a -> ByteString
serializeAsByteStringWithSchema =
  error "TODO"

serializeAsByteStringWithoutSchema :: Codec a -> ByteString
serializeAsByteStringWithoutSchema =
  error "TODO"

data DecodingError
  = SchemaMismatchDecodingError
      Schema.Schema
      -- ^ Expected.
      Schema.Schema
      -- ^ Actual.

-- |
-- Encoder, decoder and structure metadata all united in a single composable abstraction.
--
-- Guarantees to provide isomorphic encoder and decoder and
-- a schema that matches them.
data Codec a = Codec
  { schema :: Schema.Schema,
    encode :: a -> Encoding.Encoding,
    decode :: Decoding.StreamingPtrDecoder a
  }

productCodec :: ProductCodec a a -> Codec a
productCodec ProductCodec {..} =
  Codec (Schema.ProductSchema (toList schema)) encode decode

sumCodec :: [VariantCodec a] -> Codec a
sumCodec variants =
  Codec schema encode decode
  where
    schema =
      variants
        & fmap (\variant -> (variant.name, variant.schema))
        & Schema.SumSchema
    encode val =
      foldr step finish variants 0
      where
        step variant next !idx =
          case variant.encode val of
            Nothing -> next (succ idx)
            Just encoding -> Encoding.varLengthInteger (fromIntegral idx) <> encoding
        finish idx =
          -- Alternatively we can encode the index with no body.
          -- However there's no benefit of that found yet.
          Encoding.failure $ "No variant projection found"
    decode = do
      idx <- fromIntegral <$> Decoding.varLengthNatural
      case vec BVec.!? idx of
        Just decoder -> decoder
        Nothing -> Decoding.failure $ "Invalid index: " <> showAs idx
      where
        vec =
          BVec.fromList $ fmap (.decode) $ variants

-- |
-- Composable codec of product fields.
data ProductCodec i o = ProductCodec
  { schema :: Acc (Text, Schema.Schema),
    encode :: i -> Encoding.Encoding,
    decode :: Decoding.StreamingPtrDecoder o
  }

instance Functor (ProductCodec i) where
  fmap = error "TODO"

instance Applicative (ProductCodec i) where
  pure a =
    ProductCodec
      mempty
      (const mempty)
      (pure a)
  ProductCodec lSchema lEncode lDecode <*> ProductCodec rSchema rEncode rDecode =
    ProductCodec
      (lSchema <> rSchema)
      (\i -> lEncode i <> rEncode i)
      (lDecode <*> rDecode)

data VariantCodec a = VariantCodec
  { name :: Text,
    schema :: Schema.Schema,
    encode :: a -> Maybe Encoding.Encoding,
    decode :: Decoding.StreamingPtrDecoder a
  }

variant :: Text -> (a -> Maybe b) -> (b -> a) -> Codec b -> VariantCodec a
variant name unpack pack codec =
  VariantCodec
    name
    codec.schema
    ( \a -> case unpack a of
        Nothing -> Nothing
        Just b -> Just (codec.encode b)
    )
    (fmap pack codec.decode)
