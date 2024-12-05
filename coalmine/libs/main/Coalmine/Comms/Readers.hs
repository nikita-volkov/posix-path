{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Coalmine.Comms.Readers where

import Coalmine.BaseExtras.Integer qualified as Integer
import Coalmine.InternalPrelude hiding (Reader)
import Coalmine.PtrKit.Reader

boolean :: Reader Bool
boolean =
  error "TODO"

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
        decodeHead currentPtr maxPtr =
          if currentPtr <= maxPtr
            then do
              byte <- peek @Word8 currentPtr
              let absValueState = fromIntegral (byte .&. 0b00111111)
              if testBit byte 7
                then
                  if absValueState > absValueNegativeBound
                    then
                      return
                        $ let message = "Value is smaller than " <> (from . show) minVal
                           in FailedStatus message path startOffset
                    else
                      if testBit byte 6
                        then decodeTailInNegativeMode 6 absValueState (plusPtr currentPtr 1) maxPtr
                        else return $ EmittingStatus (negate absValueState) (plusPtr currentPtr 1) (succ startOffset)
                else decodeTailInPositiveMode 6 absValueState (plusPtr currentPtr 1) maxPtr
            else return $ ExhaustedStatus $ decodeHead
        decodeTailInNegativeMode !payloadBitOffset !absValueState currentPtr maxPtr =
          if currentPtr <= maxPtr
            then do
              byte <- peek @Word8 currentPtr
              let updatedAbsValueState = absValueState .|. fromIntegral (byte .&. 0b01111111)
                  updatedPayloadBitOffset = payloadBitOffset + 7
              if absValueState > absValueNegativeBound
                then
                  return
                    $ let message = "Value is smaller than " <> (from . show) minVal
                       in FailedStatus message path startOffset
                else
                  if testBit byte 7
                    then decodeTailInNegativeMode updatedPayloadBitOffset updatedAbsValueState (plusPtr currentPtr 1) maxPtr
                    else return $ EmittingStatus (valOffset - updatedAbsValueState) (plusPtr currentPtr 1) (startOffset + Integer.bytesNeededForBits updatedPayloadBitOffset)
            else return $ ExhaustedStatus $ decodeTailInNegativeMode payloadBitOffset absValueState
        decodeTailInPositiveMode !payloadBitOffset !absValueState currentPtr maxPtr =
          if currentPtr <= maxPtr
            then do
              byte <- peek @Word8 currentPtr
              let updatedAbsValueState = absValueState .|. fromIntegral (byte .&. 0b01111111)
                  updatedPayloadBitOffset = payloadBitOffset + 7
              if absValueState > absValuePositiveBound
                then
                  return
                    $ let message = "Value is larger than " <> (from . show) maxVal
                       in FailedStatus message path startOffset
                else
                  if testBit byte 7
                    then decodeTailInPositiveMode updatedPayloadBitOffset updatedAbsValueState (plusPtr currentPtr 1) maxPtr
                    else return $ EmittingStatus (valOffset + updatedAbsValueState) (plusPtr currentPtr 1) (startOffset + Integer.bytesNeededForBits updatedPayloadBitOffset)
            else return $ ExhaustedStatus $ decodeTailInPositiveMode payloadBitOffset absValueState
