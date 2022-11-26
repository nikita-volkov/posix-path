module Coalmine.PtrKit.StreamingPoker
  ( StreamingPoker,
    toLazyByteString,
    toLazyByteStringOfDefaultChunkSize,
    failure,
  )
where

import Coalmine.InternalPrelude

-- | Streaming poking operation.
--
-- Fills the provided buffer up to the provided capacity and then gives back control.
--
-- Abstracts over the lowest level of operation when serializing data for
-- interaction with the real-world, so it has the potential to be the most
-- effecient. In Haskell all real-world data-related operations interact with
-- pointers, so we do that too. So this abstraction does not even
-- utilize 'ByteString'.
--
-- In fact, since 'ByteString' is itself just an abstraction over a
-- pointer, 'StreamingPoker' can be used to stream 'ByteString' chunks of a
-- given size. You can see it as a builder optimized to produce bytestring
-- chunks with a linear complexity.
--
-- You can integrate that API with any streaming library quite easily, since
-- all of them support lists of bytestrings as the datasource. You can also
-- integrate with it close to zero cost by directly operating on data buffers.
-- Or you can do both and integrate with a streaming library more
-- efficiently.
--
-- This abstraction is not the best choice for constructing
-- a single strict 'ByteString', use 'Coalmine.PtrKit.ImmediatePoker' for that.
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

toLazyByteString ::
  -- | Chunk size.
  Int ->
  StreamingPoker ->
  LazyByteString
toLazyByteString =
  error "TODO"

toLazyByteStringOfDefaultChunkSize :: StreamingPoker -> LazyByteString
toLazyByteStringOfDefaultChunkSize =
  error "TODO"

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
