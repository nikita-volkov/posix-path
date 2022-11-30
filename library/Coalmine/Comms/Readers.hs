module Coalmine.Comms.Readers where

import Coalmine.BaseExtras.Integer qualified as Integer
import Coalmine.InternalPrelude hiding (Reader)
import Coalmine.PtrKit.Reader

varLengthUnsignedInteger ::
  (Integral a, Bits a) =>
  -- | Minimum value.
  a ->
  -- | Maximum value.
  a ->
  Reader a
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
  Reader a
varLengthSignedInteger minVal maxVal valOffset =
  Reader decode
  where
    absValuePositiveBound = maxVal - valOffset
    absValueNegativeBound = valOffset - minVal
    decode path startOffset =
      if maxVal == minVal
        then error "TODO: Fail with bad configuration"
        else decodeHead
      where
        decodeHead currentPtr afterPtr =
          if currentPtr < afterPtr
            then do
              byte <- peek @Word8 currentPtr
              let absValueState = fromIntegral (byte .&. 0b00111111)
              if testBit byte 7
                then
                  if absValueState > absValueNegativeBound
                    then
                      return $
                        let message = "Value is smaller than " <> showAs minVal
                         in FailedStatus message path startOffset
                    else
                      if testBit byte 6
                        then decodeTailInNegativeMode 6 absValueState (plusPtr currentPtr 1) afterPtr
                        else return $ EmittingStatus (negate absValueState) (plusPtr currentPtr 1) (succ startOffset)
                else decodeTailInPositiveMode 6 absValueState (plusPtr currentPtr 1) afterPtr
            else return $ ExhaustedStatus $ decodeHead
        decodeTailInNegativeMode !payloadBitOffset !absValueState currentPtr afterPtr =
          if currentPtr < afterPtr
            then do
              byte <- peek @Word8 currentPtr
              let updatedAbsValueState = absValueState .|. fromIntegral (byte .&. 0b01111111)
                  updatedPayloadBitOffset = payloadBitOffset + 7
              if absValueState > absValueNegativeBound
                then
                  return $
                    let message = "Value is smaller than " <> showAs minVal
                     in FailedStatus message path startOffset
                else
                  if testBit byte 7
                    then decodeTailInNegativeMode updatedPayloadBitOffset updatedAbsValueState (plusPtr currentPtr 1) afterPtr
                    else return $ EmittingStatus (valOffset - updatedAbsValueState) (plusPtr currentPtr 1) (startOffset + Integer.bytesNeededForBits updatedPayloadBitOffset)
            else return $ ExhaustedStatus $ decodeTailInNegativeMode payloadBitOffset absValueState
        decodeTailInPositiveMode !payloadBitOffset !absValueState currentPtr afterPtr =
          if currentPtr < afterPtr
            then do
              byte <- peek @Word8 currentPtr
              let updatedAbsValueState = absValueState .|. fromIntegral (byte .&. 0b01111111)
                  updatedPayloadBitOffset = payloadBitOffset + 7
              if absValueState > absValuePositiveBound
                then
                  return $
                    let message = "Value is larger than " <> showAs maxVal
                     in FailedStatus message path startOffset
                else
                  if testBit byte 7
                    then decodeTailInPositiveMode updatedPayloadBitOffset updatedAbsValueState (plusPtr currentPtr 1) afterPtr
                    else return $ EmittingStatus (valOffset + updatedAbsValueState) (plusPtr currentPtr 1) (startOffset + Integer.bytesNeededForBits updatedPayloadBitOffset)
            else return $ ExhaustedStatus $ decodeTailInPositiveMode payloadBitOffset absValueState
