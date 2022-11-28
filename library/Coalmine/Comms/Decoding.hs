module Coalmine.Comms.Decoding where

import Coalmine.InternalPrelude
import Data.ByteString.Internal qualified as ByteString

-- |
-- Decoder which can read from multiple chunks of data
-- represented in pointers.
-- This implies full compatibility with 'ByteString'
-- at zero cost but also provides for more low-level tools.
newtype PtrReader a = PtrReader
  { run ::
      -- Pointer to read the data from.
      Ptr Word8 ->
      -- Bytes avail in the pointer.
      Int ->
      -- Total offset amongst all inputs.
      Int ->
      -- Result of processing one chunk.
      IO (PtrReaderIteration a)
  }
  deriving (Functor)

instance Applicative PtrReader where
  pure a = PtrReader $ \ptr avail totalOffset ->
    pure $ EmittingPtrReaderIteration a ptr avail totalOffset
  left <*> right =
    error "TODO"

instance Monad PtrReader where
  return = pure
  (>>=) =
    error "TODO"

failure :: Text -> PtrReader a
failure =
  error "TODO"

varLengthNatural :: PtrReader Natural
varLengthNatural =
  error "TODO"

varLengthSignedInteger :: (Integral a, Bits a) => PtrReader a
varLengthSignedInteger =
  PtrReader processFirstByte
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
                EmittingPtrReaderIteration
                  (fromIntegral (fromIntegral @_ @Int8 byte))
                  (plusPtr ptr 1)
                  (pred avail)
                  (succ totalOffset)
        else
          return $
            ExpectingPtrReaderIteration $
              PtrReader processFirstByte
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
                EmittingPtrReaderIteration
                  (if negative then negate updatedVal else updatedVal)
                  (plusPtr ptr 1)
                  (pred avail)
                  (succ totalOffset)
        else
          return $
            ExpectingPtrReaderIteration $
              PtrReader $
                processNextByte negative index val

-- | Result of processing one chunk of a streamed input.
data PtrReaderIteration a
  = -- | Failed.
    FailedPtrReaderIteration
      Int
      -- ^ Local offset in the last consumed input.
      Int
      -- ^ Total offset amongst all consumed inputs.
  | EmittingPtrReaderIteration
      a
      -- ^ Result.
      (Ptr Word8)
      -- ^ Pointer to read the following data from.
      Int
      -- ^ Bytes avail in the pointer.
      Int
      -- ^ Total offset amongst all inputs.
  | ExpectingPtrReaderIteration
      (PtrReader a)
  deriving (Functor)

feedByteString ::
  PtrReader a ->
  -- | Accumulated offset.
  Int ->
  ByteString ->
  PtrReaderIteration a
feedByteString decoder totalOffset (ByteString.BS fp len) =
  unsafePerformIO . withForeignPtr fp $ \p ->
    decoder.run p len totalOffset
