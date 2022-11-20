module Coalmine.Pakk.Codec where

import Coalmine.InternalPrelude
import Coalmine.Pakk.Decoding qualified as Decoding
import Coalmine.Pakk.Encoding qualified as Encoding
import Coalmine.Pakk.Schema qualified as Schema

serializeAsByteStringWithSchema :: Codec a -> ByteString
serializeAsByteStringWithSchema =
  error "TODO"

serializeAsByteStringWithoutSchema :: Codec a -> ByteString
serializeAsByteStringWithoutSchema =
  error "TODO"

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
  error "TODO"

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
  { schema :: Acc (Text, Schema.Schema),
    encode :: Integer -> a -> Either Integer Encoding.Encoding,
    decode :: Acc (Decoding.StreamingPtrDecoder a)
  }

variant :: Text -> (a -> Maybe b) -> (b -> a) -> Codec b -> VariantCodec a
variant name unpack pack codec =
  VariantCodec
    (pure (name, codec.schema))
    ( \idx a -> case unpack a of
        Nothing -> Left (succ idx)
        Just b -> Right (Encoding.varLengthInteger idx <> codec.encode b)
    )
    (pure (fmap pack codec.decode))
