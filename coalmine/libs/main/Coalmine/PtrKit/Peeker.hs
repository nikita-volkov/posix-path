module Coalmine.PtrKit.Peeker where

import Coalmine.InternalPrelude
import Data.ByteString.Internal qualified as ByteString

-- | Result of processing one chunk of a streamed input.
data Status a
  = -- | Failed.
    FailedStatus
      Text
      -- ^ Message.
      (Ptr Word8)
      -- ^ Pointer at which we've stopped.
  | -- | Finished with a result and a remainder.
    EmittingStatus
      a
      -- ^ Result.
      (Ptr Word8)
      -- ^ Pointer to read the following data from.
      (Ptr Word8)
      -- ^ Pointer after the data.
  | -- | The provided pointer is read from in completion,
    -- we still need more data though.
    ExhaustedStatus
      (Peeker a)
  deriving (Functor)

feedByteString ::
  Peeker a ->
  ByteString ->
  Status a
feedByteString (Peeker run) (ByteString.BS fp len) =
  unsafePerformIO . withForeignPtr fp $ \p ->
    run (plusPtr p len) p

-- |
-- Decoder which can read from multiple chunks of data
-- represented in pointers.
-- This implies full compatibility with 'ByteString'
-- at zero cost but also provides for more low-level tools.
newtype Peeker a = Peeker
  { run ::
      -- Pointer to read the data from.
      Ptr Word8 ->
      -- Pointer after the data.
      Ptr Word8 ->
      -- Result of processing one chunk.
      IO (Status a)
  }
  deriving (Functor)

instance Applicative Peeker where
  pure a = Peeker $ \currentPtr afterPtr ->
    pure $ EmittingStatus a currentPtr afterPtr
  left <*> right =
    error "TODO"

instance Monad Peeker where
  return = pure
  (>>=) =
    error "TODO"

failure :: Text -> Peeker a
failure =
  error "TODO"

varLengthSignedInteger :: (Integral a, Bits a) => Peeker a
varLengthSignedInteger =
  Peeker processFirstByte
  where
    processFirstByte currentPtr afterPtr =
      if currentPtr < afterPtr
        then do
          byte <- peek currentPtr
          if testBit byte 6
            then
              processNextByte
                (testBit byte 7)
                6
                (fromIntegral byte)
                (plusPtr currentPtr 1)
                afterPtr
            else
              return $
                EmittingStatus
                  (fromIntegral (fromIntegral @_ @Int8 byte))
                  (plusPtr currentPtr 1)
                  afterPtr
        else
          return $
            ExhaustedStatus $
              Peeker processFirstByte
    processNextByte negative !bitOffset !val currentPtr afterPtr =
      if currentPtr < afterPtr
        then do
          byte <- peek @Word8 currentPtr
          let updatedVal = unsafeShiftL (fromIntegral byte) bitOffset .|. val
          if testBit byte 7
            then
              processNextByte
                negative
                (bitOffset + 7)
                updatedVal
                (plusPtr currentPtr 1)
                afterPtr
            else
              return $
                EmittingStatus
                  (if negative then negate updatedVal else updatedVal)
                  (plusPtr currentPtr 1)
                  afterPtr
        else
          return $
            ExhaustedStatus $
              Peeker $
                processNextByte negative bitOffset val
