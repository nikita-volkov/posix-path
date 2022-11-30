module Coalmine.Comms.Decoder where

import Coalmine.BaseExtras.Integer qualified as Integer
import Coalmine.InternalPrelude
import Coalmine.PtrKit.Peeker qualified as Peeker

-- | Result of processing one chunk of a streamed input.
data Status a
  = -- | Failed.
    FailedStatus
      Text
      -- ^ Message.
      [Text]
      -- ^ Contexts.
      (Ptr Word8)
      -- ^ Pointer at which we've stopped.
      Int
      -- ^ Total offset amongst all consumed inputs before the beginning of this value.
  | EmittingStatus
      a
      -- ^ Result.
      (Ptr Word8)
      -- ^ Pointer to read the following data from.
      Int
      -- ^ Total offset amongst all consumed inputs.
  | -- | The provided pointer is read from in completion,
    -- we still need more data though.
    ExhaustedStatus
      ( -- Pointer to read the data from.
        Ptr Word8 ->
        -- Pointer after the data.
        Ptr Word8 ->
        -- Result of processing one chunk.
        IO (Status a)
      )
  deriving (Functor)

-- |
-- Decoder which can read from multiple chunks of data
-- represented in pointers.
-- This implies full compatibility with 'ByteString'
-- at zero cost but also provides for more low-level tools.
newtype Decoder a = Decoder
  { run ::
      -- Context path.
      [Text] ->
      -- Total offset amongst all inputs.
      Int ->
      -- Pointer to read the data from.
      Ptr Word8 ->
      -- Pointer after the data.
      Ptr Word8 ->
      -- Result of processing one chunk.
      IO (Status a)
  }
  deriving (Functor)

instance Applicative Decoder where
  pure a = Decoder $ \_ totalOffset ptr ptr' ->
    pure $ EmittingStatus a ptr totalOffset
  left <*> right =
    error "TODO"

instance Monad Decoder where
  return = pure
  Decoder runL >>= contR =
    Decoder run
    where
      run path offset ptr ptr' =
        runL path offset ptr ptr' >>= processStatusL
        where
          processStatusL = \case
            EmittingStatus resL ptrL offsetL ->
              case contR resL of Decoder runR -> runR path offsetL ptrL ptr'
            ExhaustedStatus runNextL ->
              return . ExhaustedStatus $ \ptr ptr' ->
                runNextL ptr ptr' >>= processStatusL

liftPeeker :: Peeker.Peeker a -> Decoder a
liftPeeker =
  Decoder . read
  where
    read peeker path !offset ptr ptr' =
      peeker.run ptr ptr' <&> \case
        Peeker.FailedStatus message ptr'' ->
          FailedStatus message path ptr'' (offset + minusPtr ptr'' ptr)
        Peeker.EmittingStatus result ptr'' _ ->
          EmittingStatus result ptr'' (offset + minusPtr ptr'' ptr)
        Peeker.ExhaustedStatus nextPeeker ->
          ExhaustedStatus $
            read nextPeeker path (offset + minusPtr ptr' ptr)

inContext :: Text -> Decoder a -> Decoder a
inContext =
  error "TODO"

failure :: Text -> Decoder a
failure =
  error "TODO"

varLengthUnsignedInteger ::
  (Integral a, Bits a) =>
  -- | Minimum value.
  a ->
  -- | Maximum value.
  a ->
  Decoder a
varLengthUnsignedInteger =
  error "TODO"

varLengthSignedInteger ::
  (Integral a, Bits a, Show a) =>
  -- | Minimum value.
  a ->
  -- | Maximum value.
  a ->
  -- | Zero offset. Points to the most probable value.
  a ->
  Decoder a
varLengthSignedInteger minVal maxVal valOffset =
  Decoder decode
  where
    absValuePositiveBound = maxVal - valOffset
    absValueNegativeBound = valOffset - minVal
    decode path startOffset =
      if maxVal == minVal
        then error "TODO: Fail with bad configuration"
        else decodeHead
      where
        bitsToOffset bits =
          startOffset + Integer.bytesNeededForBits bits
        decodeHead currentPtr afterPtr =
          if currentPtr < afterPtr
            then do
              byte <- peek @Word8 currentPtr
              if testBit byte 7
                then
                  let absValueState = fromIntegral (byte .&. 0b00111111)
                   in if absValueState > absValueNegativeBound
                        then
                          return $
                            let message = "Value is smaller than " <> showAs minVal
                             in FailedStatus message path currentPtr startOffset
                        else
                          if testBit byte 6
                            then decodeTailInNegativeMode 6 absValueState (plusPtr currentPtr 1) afterPtr
                            else error "TODO: finish"
                else error "TODO: decode tail in positive mode"
            else error "TODO: exhaust"
        decodeTailInNegativeMode !payloadBitOffset !absValueState currentPtr afterPtr =
          if currentPtr < afterPtr
            then do
              byte <- peek @Word8 currentPtr
              let updatedAbsValueState = absValueState .|. fromIntegral (byte .&. 0b01111111)
              if absValueState > absValueNegativeBound
                then
                  return $
                    let message = "Value is smaller than " <> showAs minVal
                     in FailedStatus message path currentPtr startOffset
                else
                  if testBit byte 7
                    then decodeTailInNegativeMode (payloadBitOffset + 7) updatedAbsValueState (plusPtr currentPtr 1) afterPtr
                    else error "TODO: finish"
            else error "TODO: exhaust"
