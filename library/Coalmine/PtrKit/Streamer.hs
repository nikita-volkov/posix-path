module Coalmine.PtrKit.Streamer
  ( Streamer,
    toLazyByteString,
    toLazyByteStringOfDefaultChunkSize,
    streamThruBuffer,
    failure,
    varLengthUnsignedInteger,
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
-- pointer, 'Streamer' can be used to stream 'ByteString' chunks of a
-- given size. You can see it as a builder optimized to produce bytestring
-- chunks with a linear complexity.
--
-- You can integrate that API with any streaming library quite easily, since
-- all of them support lists of bytestrings as the datasource. You can also
-- integrate with it at close to zero cost by directly operating on data buffers.
-- Or you can do both and integrate with a streaming library more
-- efficiently.
--
-- This abstraction is not the best choice for constructing
-- a single strict 'ByteString', use 'Coalmine.PtrKit.ValidatedEncoding' for that.
newtype Streamer = Streamer {run :: Ptr Word8 -> Int -> IO Status}

instance Semigroup Streamer where
  left <> right =
    Streamer $ \ptr cap ->
      left.run ptr cap >>= \case
        FinishedStatus ptr cap ->
          right.run ptr cap
        ExhaustedStatus nextLeftWrite ->
          return $ ExhaustedStatus $ nextLeftWrite <> right
        FailedStatus err ptr cap ->
          return $ FailedStatus err ptr cap

instance Monoid Streamer where
  mempty = Streamer $ \ptr cap ->
    pure $ FinishedStatus ptr cap

toLazyByteString ::
  -- | Chunk size.
  Int ->
  Streamer ->
  LazyByteString
toLazyByteString =
  error "TODO"

toLazyByteStringOfDefaultChunkSize :: Streamer -> LazyByteString
toLazyByteStringOfDefaultChunkSize =
  error "TODO"

-- | Evaluate the poker by repeatedly filling up a reusable buffer and calling
-- a continuation on it. Buffer allocation is encapsulated.
--
-- This is what you should use for integrating with sockets or file system.
streamThruBuffer ::
  Streamer ->
  -- | Reused buffer size.
  Int ->
  -- | Action to be repeatedly executed when the buffer is filled.
  -- The params are the pointer to read from and the length of data in it.
  (Ptr Word8 -> Int -> IO ()) ->
  -- | Action producing error details if there is one.
  IO (Maybe Text)
streamThruBuffer poker bufSize send =
  allocaBytes bufSize $ \ptr ->
    let exhaust (Streamer run) =
          run ptr bufSize >>= \case
            ExhaustedStatus next -> do
              send ptr bufSize
              exhaust next
            FinishedStatus ptrAfter _ -> do
              when (ptrAfter > ptr) $
                send ptr (minusPtr ptrAfter ptr)
              return Nothing
            FailedStatus reason ptrAfter _ -> do
              when (ptrAfter > ptr) $
                send ptr (minusPtr ptrAfter ptr)
              return $ Just reason
     in exhaust poker

failure :: Text -> Streamer
failure reason =
  Streamer $ \ptr cap -> pure $ FailedStatus reason ptr cap

varLengthUnsignedInteger :: (Integral a, Bits a, Show a) => a -> Streamer
varLengthUnsignedInteger =
  error "TODO"

data Status
  = FinishedStatus
      (Ptr Word8)
      -- ^ Pointer after the written data.
      Int
      -- ^ Capacity of that pointer.
  | -- | Need a pointer to continue writing to.
    ExhaustedStatus
      Streamer
      -- ^ Next encoding to execute.
  | -- | Encoding failure.
    FailedStatus
      Text
      -- ^ Reason
      (Ptr Word8)
      -- ^ Pointer after the written data.
      Int
      -- ^ Capacity of that pointer.
