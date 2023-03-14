module Coalmine.PtrKit.ValidatedEncoding
  ( ValidatedEncoding (..),

    -- * Elimination
    toByteString,

    -- * Construction and transformation
    inContext,
    failure,

    -- * Errors
    Err (..),
  )
where

import Coalmine.InternalPrelude
import Coalmine.PtrKit.Encoding qualified as Encoding

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
