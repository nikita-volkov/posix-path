module Coalmine.Comms.Decoding where

import Coalmine.InternalPrelude
import Data.ByteString.Internal qualified as ByteString

-- |
-- Decoder which can read from multiple chunks of data
-- represented in pointers.
-- This implies full compatibility with 'ByteString'
-- at zero cost but also provides for more low-level tools.
newtype StreamingPtrDecoder a = StreamingPtrDecoder
  { run ::
      -- Pointer to read the data from.
      Ptr Word8 ->
      -- Bytes avail in the pointer.
      Int ->
      -- Total offset amongst all inputs.
      Int ->
      -- Result of processing one chunk.
      IO (StreamingPtrDecoderIteration a)
  }
  deriving (Functor)

instance Applicative StreamingPtrDecoder where
  pure a = StreamingPtrDecoder $ \ptr avail totalOffset ->
    pure $ EmittingStreamingPtrDecoderIteration a ptr avail totalOffset
  left <*> right =
    error "TODO"

instance Monad StreamingPtrDecoder where
  return = pure
  (>>=) =
    error "TODO"

failure :: Text -> StreamingPtrDecoder a
failure =
  error "TODO"

varLengthNatural :: StreamingPtrDecoder Natural
varLengthNatural =
  error "TODO"

varLengthSignedInteger :: (Integral a, Bits a) => StreamingPtrDecoder a
varLengthSignedInteger =
  StreamingPtrDecoder processFirstByte
  where
    processFirstByte ptr avail totalOffset =
      if avail > 0
        then do
          byte <- peek ptr
          if testBit byte 6
            then
              processNextByte
                (testBit byte 7)
                6
                (fromIntegral byte)
                (plusPtr ptr 1)
                (pred avail)
                (succ totalOffset)
            else
              return $
                EmittingStreamingPtrDecoderIteration
                  (fromIntegral (fromIntegral @_ @Int8 byte))
                  (plusPtr ptr 1)
                  (pred avail)
                  (succ totalOffset)
        else
          return $
            ExpectingStreamingPtrDecoderIteration $
              StreamingPtrDecoder processFirstByte
    processNextByte negative !index !val ptr avail !totalOffset =
      if avail > 0
        then do
          byte <- peek @Word8 ptr
          let updatedVal = unsafeShiftL (fromIntegral byte) index .|. val
          if testBit byte 7
            then
              processNextByte
                negative
                (index + 7)
                updatedVal
                (plusPtr ptr 1)
                (pred avail)
                (succ totalOffset)
            else
              return $
                EmittingStreamingPtrDecoderIteration
                  (if negative then negate updatedVal else updatedVal)
                  (plusPtr ptr 1)
                  (pred avail)
                  (succ totalOffset)
        else
          return $
            ExpectingStreamingPtrDecoderIteration $
              StreamingPtrDecoder $
                processNextByte negative index val

-- | Result of processing one chunk of a streamed input.
data StreamingPtrDecoderIteration a
  = -- | Failed.
    FailedStreamingPtrDecoderIteration
      Int
      -- ^ Local offset in the last consumed input.
      Int
      -- ^ Total offset amongst all consumed inputs.
  | EmittingStreamingPtrDecoderIteration
      a
      -- ^ Result.
      (Ptr Word8)
      -- ^ Pointer to read the following data from.
      Int
      -- ^ Bytes avail in the pointer.
      Int
      -- ^ Total offset amongst all inputs.
  | ExpectingStreamingPtrDecoderIteration
      (StreamingPtrDecoder a)
  deriving (Functor)

feedByteString ::
  StreamingPtrDecoder a ->
  -- | Accumulated offset.
  Int ->
  ByteString ->
  StreamingPtrDecoderIteration a
feedByteString decoder totalOffset (ByteString.BS fp len) =
  unsafePerformIO . withForeignPtr fp $ \p ->
    decoder.run p len totalOffset
