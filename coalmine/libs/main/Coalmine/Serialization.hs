module Coalmine.Serialization where

import Coalmine.InternalPrelude
import Data.Scientific qualified as Scientific
import Data.Serialize qualified as Cereal
import Data.Vector qualified as BVec

-- * Execution

variantGet :: Variant sum -> Cereal.Get sum
variantGet (Variant map1 map2 codec) =
  codec.get <&> map2

-- * Variant

data Variant sum
  = forall i o.
    Variant
      (sum -> Maybe i)
      -- ^ Narrow from the sum.
      (o -> sum)
      -- ^ Broaden to the sum.
      (Codec i o)
      -- ^ Variant codec.

instance Invariant Variant where
  invmap f g (Variant narrow broaden schema) =
    Variant (narrow . g) (f . broaden) schema

variant ::
  -- | Attempt to extract the variant from the sum.
  (sum -> Maybe variant) ->
  -- | Map the variant to the sum.
  (variant -> sum) ->
  -- | Codec of the variant.
  InvCodec variant ->
  Variant sum
variant = Variant

-- * Codec

data Codec a b = Codec
  { put :: a -> Cereal.Put,
    get :: Cereal.Get b
  }

instance Functor (Codec a) where
  fmap f (Codec put get) =
    Codec put (fmap f get)

instance Applicative (Codec a) where
  pure = Codec mempty . pure
  (<*>) = error "TODO"

instance Profunctor Codec where
  dimap f g (Codec put get) =
    Codec (put . f) (fmap g get)

type InvCodec a = Codec a a

sum :: [Variant sum] -> InvCodec sum
sum variants =
  Codec encoder decoder
  where
    encoder = do
      error "TODO"
    decoder = do
      tag <- fromIntegral <$> (natural 4).get
      case decodersVec BVec.!? tag of
        Just decoder -> decoder
        Nothing -> fail "Invalid tag"
    decodersVec = BVec.fromList $ fmap variantGet variants

-- |
-- Secure integer.
integer ::
  -- | Max byte-size.
  Int ->
  InvCodec Integer
integer =
  error "TODO"

-- |
-- Secure natural.
natural ::
  -- | Max byte-size.
  Int ->
  InvCodec Natural
natural =
  error "TODO"

int :: InvCodec Int
int =
  error "TODO"

scientific ::
  -- | Max byte-size of the coefficient.
  Int ->
  InvCodec Scientific
scientific coefficientSpace =
  Scientific.scientific
    <$> lmap Scientific.coefficient (integer coefficientSpace)
    <*> lmap Scientific.base10Exponent int
