module Coalmine.PtrKit.StreamingPoker where

import Coalmine.InternalPrelude

-- |
-- Streaming poking operation.
-- Fills the provided buffer up to a capacity and then gives back control.
newtype StreamingPoker = StreamingPoker {run :: Ptr Word8 -> Int -> IO WriteIteration}

instance Semigroup StreamingPoker where
  left <> right =
    StreamingPoker $ \ptr cap ->
      left.run ptr cap >>= \case
        FinishedWriteIteration ptr cap ->
          right.run ptr cap
        ExhaustedWriteIteration nextLeftWrite ->
          return $ ExhaustedWriteIteration $ nextLeftWrite <> right
        FailedWriteIteration err ptr cap ->
          return $ FailedWriteIteration err ptr cap

instance Monoid StreamingPoker where
  mempty = StreamingPoker $ \ptr cap ->
    pure $ FinishedWriteIteration ptr cap

failure :: Text -> StreamingPoker
failure reason =
  StreamingPoker $ \ptr cap -> pure $ FailedWriteIteration reason ptr cap

data WriteIteration
  = FinishedWriteIteration
      (Ptr Word8)
      -- ^ Pointer after the written data.
      Int
      -- ^ Capacity of that pointer.
  | -- | Need a pointer to continue writing to.
    ExhaustedWriteIteration
      StreamingPoker
      -- ^ Next encoding to execute.
  | -- | Encoding failure.
    FailedWriteIteration
      Text
      -- ^ Reason
      (Ptr Word8)
      -- ^ Pointer after the written data.
      Int
      -- ^ Capacity of that pointer.
