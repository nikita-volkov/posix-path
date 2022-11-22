module Coalmine.Comms.Codec where

import Coalmine.Comms.Decoding qualified as Decoding
import Coalmine.Comms.Encoding qualified as Encoding
import Coalmine.Comms.Schema qualified as Schema
import Coalmine.InternalPrelude
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

instance Profunctor ProductCodec where
  dimap f1 f2 codec =
    ProductCodec
      codec.schema
      (codec.encode . f1)
      (fmap f2 codec.decode)

field :: Text -> Codec a -> ProductCodec a a
field name codec =
  error "TODO"

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

-- * Validation

isCorrect :: CodecCorrectness -> Bool
isCorrect =
  error "TODO"

-- |
-- Composite over the report on all the properties of a correct codec.
data CodecCorrectness = CodecCorrectness
  { -- | Whether encoding and decoding produces the same value.
    encodeDecode :: ~Bool,
    schemaMatch :: ~Bool
  }

-- |
-- Test a codec on a given value and produce a structured report about
-- the properties that are satisfied.
-- Codec is considered correct only when it satisfies all properties,
-- which can be checked via a combination of 'validate' and 'isCorrect' functions.
--
-- Can be seen as a universal interface that can be mapped into all property-testing
-- libraries.
-- The specific adapters are expected to be provided as separate libraries,
-- thus both not limiting the user to a specific set of libraries and
-- leaving the interface open for other libraries to be integrated via.
-- An open interface may also turn usable for tasks that have not been discovered yet.
-- This approach also lets the user control the dependencies.
validate :: Codec a -> a -> CodecCorrectness
validate =
  error "TODO"
