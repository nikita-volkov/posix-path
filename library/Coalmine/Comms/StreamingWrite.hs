module Coalmine.Comms.StreamingWrite where

import Coalmine.InternalPrelude

-- |
-- Streaming write operation.
-- Fills the provided buffer up to a capacity and then gives back control.
newtype StreamingWrite = StreamingWrite {run :: Ptr Word8 -> Int -> IO WriteIteration}

instance Semigroup StreamingWrite where
  left <> right =
    StreamingWrite $ \ptr cap ->
      left.run ptr cap >>= \case
        FinishedWriteIteration ptr cap ->
          right.run ptr cap
        ExhaustedWriteIteration nextLeftWrite ->
          return $ ExhaustedWriteIteration $ nextLeftWrite <> right
        FailedWriteIteration err ptr cap ->
          return $ FailedWriteIteration err ptr cap

instance Monoid StreamingWrite where
  mempty = StreamingWrite $ \ptr cap ->
    pure $ FinishedWriteIteration ptr cap

failure :: Text -> StreamingWrite
failure reason =
  StreamingWrite $ \ptr cap -> pure $ FailedWriteIteration reason ptr cap

data WriteIteration
  = FinishedWriteIteration
      (Ptr Word8)
      -- ^ Pointer after the written data.
      Int
      -- ^ Capacity of that pointer.
  | -- | Need a pointer to continue writing to.
    ExhaustedWriteIteration
      StreamingWrite
      -- ^ Next encoding to execute.
  | -- | Encoding failure.
    FailedWriteIteration
      Text
      -- ^ Reason
      (Ptr Word8)
      -- ^ Pointer after the written data.
      Int
      -- ^ Capacity of that pointer.
