module Coalmine.PtrKit.ValidatedEncoding
  ( ValidatedEncoding,

    -- * Elimination
    toByteString,

    -- * Construction and transformation
    inContext,
    failure,
    varLengthUnsignedInteger,

    -- * Errors
    Err (..),
  )
where

import Coalmine.InternalPrelude
import Coalmine.PtrKit.Encoding qualified as Encoding
import Data.ByteString.Internal qualified as ByteStringInternal

data Err = Err
  { reason :: Text,
    context :: [Text]
  }

newtype ValidatedEncoding = ValidatedEncoding
  { run :: [Text] -> Either Err Encoding.Encoding
  }

instance Semigroup ValidatedEncoding where
  left <> right =
    ValidatedEncoding $ \context ->
      (<>) <$> left.run context <*> right.run context

instance Monoid ValidatedEncoding where
  mempty =
    ValidatedEncoding $ const $ Right mempty

toByteString :: ValidatedEncoding -> Either Err ByteString
toByteString (ValidatedEncoding run) =
  run [] <&> Encoding.toByteString

inContext :: Text -> ValidatedEncoding -> ValidatedEncoding
inContext context (ValidatedEncoding run) =
  ValidatedEncoding (\path -> run (context : path))

failure :: Text -> ValidatedEncoding
failure reason =
  ValidatedEncoding (\path -> Left $ Err reason path)

-- |
-- Variable length representation of unsigned integers.
--
-- Uses the 8th bit of each octet to specify, whether another octet is needed.
varLengthUnsignedInteger :: (Integral a, Bits a, Show a) => a -> ValidatedEncoding
varLengthUnsignedInteger value =
  if value < 0
    then inContext "varLengthUnsignedInteger" (failure ("Negative value: " <> showAs value))
    else ValidatedEncoding $ const $ Right $ Encoding.varLengthUnsignedInteger value
