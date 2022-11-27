module Coalmine.PtrKit.ValidatingWriter
  ( ValidatingWriter,

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
import Coalmine.PtrKit.PtrIO qualified as PtrIO
import Coalmine.PtrKit.Writer qualified as Writer
import Data.ByteString.Internal qualified as ByteStringInternal

data Err = Err
  { reason :: Text,
    input :: Text,
    context :: [Text]
  }

newtype ValidatingWriter = ValidatingWriter
  { run :: [Text] -> Either Err Writer.Writer
  }

instance Semigroup ValidatingWriter where
  left <> right =
    ValidatingWriter $ \context ->
      (<>) <$> left.run context <*> right.run context

instance Monoid ValidatingWriter where
  mempty =
    ValidatingWriter $ const $ Right mempty

toByteString :: ValidatingWriter -> Either Err ByteString
toByteString (ValidatingWriter run) =
  run [] <&> Writer.toByteString

inContext :: Text -> ValidatingWriter -> ValidatingWriter
inContext context (ValidatingWriter run) =
  ValidatingWriter (\path -> run (context : path))

failure :: Text -> Text -> ValidatingWriter
failure reason input =
  ValidatingWriter (\path -> Left $ Err reason input path)

-- |
-- Variable length representation of unsigned integers.
--
-- Uses the 8th bit of each octet to specify, whether another octet is needed.
varLengthUnsignedInteger :: (Integral a, Bits a, Show a) => a -> ValidatingWriter
varLengthUnsignedInteger value =
  if value < 0
    then inContext "varLengthUnsignedInteger" (failure "Negative value" (showAs value))
    else ValidatingWriter $ const $ Right $ Writer.varLengthUnsignedInteger value
