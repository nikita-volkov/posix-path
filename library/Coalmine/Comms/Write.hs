module Coalmine.Comms.Write where

import Coalmine.InternalPrelude

newtype Write = Write {run :: Ptr Word8 -> Int -> IO WriteIteration}

instance Semigroup Write where
  left <> right =
    Write $ \ptr cap ->
      left.run ptr cap >>= \case
        FinishedWriteIteration ptr cap ->
          right.run ptr cap
        ExhaustedWriteIteration nextLeftWrite ->
          return $ ExhaustedWriteIteration $ nextLeftWrite <> right
        FailedWriteIteration err ->
          return $ FailedWriteIteration err

instance Monoid Write where
  mempty = Write $ \ptr cap ->
    pure $ FinishedWriteIteration ptr cap

failure :: Text -> Write
failure reason =
  Write $ \_ _ -> pure $ FailedWriteIteration reason

data WriteIteration
  = FinishedWriteIteration
      (Ptr Word8)
      -- ^ Pointer after the written data.
      Int
      -- ^ Capacity of that pointer.
  | -- | Need a pointer to continue writing to.
    ExhaustedWriteIteration
      Write
      -- ^ Next encoding to execute.
  | -- | Encoding failure.
    FailedWriteIteration
      Text
