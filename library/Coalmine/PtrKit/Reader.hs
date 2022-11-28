module Coalmine.PtrKit.Reader where

import Coalmine.InternalPrelude hiding (Reader)
import Data.ByteString.Internal qualified as ByteString

-- |
-- Decoder which can read from multiple chunks of data
-- represented in pointers.
-- This implies full compatibility with 'ByteString'
-- at zero cost but also provides for more low-level tools.
newtype Reader a = Reader
  { run ::
      -- Pointer to read the data from.
      Ptr Word8 ->
      -- Bytes avail in the pointer.
      Int ->
      -- Total offset amongst all inputs.
      Int ->
      -- Result of processing one chunk.
      IO (ReaderIteration a)
  }
  deriving (Functor)

instance Applicative Reader where
  pure a = Reader $ \ptr avail totalOffset ->
    pure $ EmittingReaderIteration a ptr avail totalOffset
  left <*> right =
    error "TODO"

instance Monad Reader where
  return = pure
  (>>=) =
    error "TODO"

failure :: Text -> Reader a
failure =
  error "TODO"

varLengthNatural :: Reader Natural
varLengthNatural =
  error "TODO"

varLengthSignedInteger :: (Integral a, Bits a) => Reader a
varLengthSignedInteger =
  Reader processFirstByte
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
                EmittingReaderIteration
                  (fromIntegral (fromIntegral @_ @Int8 byte))
                  (plusPtr ptr 1)
                  (pred avail)
                  (succ totalOffset)
        else
          return $
            ExpectingReaderIteration $
              Reader processFirstByte
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
                EmittingReaderIteration
                  (if negative then negate updatedVal else updatedVal)
                  (plusPtr ptr 1)
                  (pred avail)
                  (succ totalOffset)
        else
          return $
            ExpectingReaderIteration $
              Reader $
                processNextByte negative index val

-- | Result of processing one chunk of a streamed input.
data ReaderIteration a
  = -- | Failed.
    FailedReaderIteration
      Int
      -- ^ Local offset in the last consumed input.
      Int
      -- ^ Total offset amongst all consumed inputs.
  | EmittingReaderIteration
      a
      -- ^ Result.
      (Ptr Word8)
      -- ^ Pointer to read the following data from.
      Int
      -- ^ Bytes avail in the pointer.
      Int
      -- ^ Total offset amongst all inputs.
  | ExpectingReaderIteration
      (Reader a)
  deriving (Functor)

feedByteString ::
  Reader a ->
  -- | Accumulated offset.
  Int ->
  ByteString ->
  ReaderIteration a
feedByteString decoder totalOffset (ByteString.BS fp len) =
  unsafePerformIO . withForeignPtr fp $ \p ->
    decoder.run p len totalOffset
