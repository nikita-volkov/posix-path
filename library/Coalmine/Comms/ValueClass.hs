module Coalmine.Comms.ValueClass where

import Coalmine.Comms.Codec qualified as Codec
import Coalmine.InternalPrelude

-- |
-- Provides a data type with a representation in the Pakk format.
class PakkValue a where
  pakkValueCodec :: Codec.Codec a

instance (PakkValue a, PakkValue b) => PakkValue (a, b) where
  pakkValueCodec =
    Codec.productCodec $
      (,)
        <$> lmap fst (Codec.field "0" pakkValueCodec)
        <*> lmap snd (Codec.field "1" pakkValueCodec)

instance (PakkValue a, PakkValue b, PakkValue c) => PakkValue (a, b, c) where
  pakkValueCodec =
    Codec.productCodec $
      (,,)
        <$> lmap (\(x, _, _) -> x) (Codec.field "0" pakkValueCodec)
        <*> lmap (\(_, x, _) -> x) (Codec.field "1" pakkValueCodec)
        <*> lmap (\(_, _, x) -> x) (Codec.field "2" pakkValueCodec)

instance (PakkValue a, PakkValue b) => PakkValue (Either a b) where
  pakkValueCodec =
    Codec.sumCodec
      [ Codec.variant
          "left"
          ( \case
              Left a -> Just a
              _ -> Nothing
          )
          Left
          pakkValueCodec,
        Codec.variant
          "right"
          ( \case
              Right a -> Just a
              _ -> Nothing
          )
          Right
          pakkValueCodec
      ]
