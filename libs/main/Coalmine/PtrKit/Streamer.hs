-- |
-- This module provides only on the means for definition, execution and
-- composition of streamers. Specific definitions are to be defined
-- by extensions. They depend on specific formats.
-- Thus this library achieves stability and focus.
module Coalmine.PtrKit.Streamer
  ( Streamer (..),

    -- * Execution
    toLazyByteString,
    toLazyByteStringOfDefaultChunkSize,
    streamThruBuffer,

    -- * Definition
    failure,
  )
where

import Coalmine.InternalPrelude
import Coalmine.PtrKit.Writer qualified as Writer
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder.Prim qualified as ByteStringBuilderPrim
import Data.ByteString.Builder.Prim.Internal qualified as ByteStringBuilderPrimInternal
import Data.ByteString.Internal qualified as ByteStringInternal

data Status
  = -- | Pointer after the written data.
    FinishedStatus
      (Ptr Word8)
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
newtype Streamer = Streamer {run :: Ptr Word8 -> Ptr Word8 -> IO Status}

instance Semigroup Streamer where
  left <> right =
    Streamer $ \ptr boundPtr ->
      left.run ptr boundPtr >>= \case
        FinishedStatus ptr ->
          right.run ptr boundPtr
        ExhaustedStatus nextLeftWrite ->
          return $ ExhaustedStatus $ nextLeftWrite <> right
        FailedStatus err ptr ->
          return $ FailedStatus err ptr

instance Monoid Streamer where
  mempty = Streamer $ \ptr _ ->
    pure $ FinishedStatus ptr

-- * Execution

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
  --
  -- For TCP traffic @1380@ is the safe number derived from the widespread
  -- MTU frame size of @1500@ sans the overhead of various possible headers.
  Int ->
  -- | Action to be repeatedly executed when the buffer is filled.
  -- The params are the pointer to read from and the length of data in it.
  (Ptr Word8 -> Int -> IO ()) ->
  -- | Action producing error details if there is one.
  IO (Maybe Text)
streamThruBuffer poker bufSize send =
  allocaBytes bufSize $ \ptr ->
    let boundPtr = plusPtr ptr bufSize
        exhaust (Streamer run) =
          run ptr boundPtr >>= \case
            ExhaustedStatus next -> do
              send ptr bufSize
              exhaust next
            FinishedStatus ptrAfter -> do
              when (ptrAfter > ptr) $
                send ptr (minusPtr ptrAfter ptr)
              return Nothing
            FailedStatus reason ptrAfter -> do
              when (ptrAfter > ptr) $
                send ptr (minusPtr ptrAfter ptr)
              return $ Just reason
     in exhaust poker

-- * Definition

failure :: Text -> Streamer
failure reason =
  Streamer $ \ptr _ -> pure $ FailedStatus reason ptr

byteString :: ByteString -> Streamer
byteString input@(ByteStringInternal.BS inputFp inputSize) =
  Streamer $ \ptr boundPtr ->
    let capacity = minusPtr boundPtr ptr
     in if capacity >= inputSize
          then unsafeWithForeignPtr inputFp $ \inputPtr ->
            ByteStringInternal.memcpy ptr inputPtr inputSize
              $> FinishedStatus (plusPtr ptr inputSize)
          else do
            unsafeWithForeignPtr inputFp $ \inputPtr ->
              ByteStringInternal.memcpy ptr inputPtr capacity
            return $ ExhaustedStatus $ byteString $ ByteString.drop capacity input

writer :: Writer.Writer -> Streamer
writer writer@(Writer.Writer size poke) =
  Streamer $ \ptr boundPtr ->
    let capacity = minusPtr boundPtr ptr
     in if capacity >= size
          then poke ptr <&> FinishedStatus
          else (byteString (Writer.toByteString writer)).run ptr boundPtr
