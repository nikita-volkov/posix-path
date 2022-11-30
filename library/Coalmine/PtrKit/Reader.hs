module Coalmine.PtrKit.Reader where

import Coalmine.BaseExtras.Integer qualified as Integer
import Coalmine.InternalPrelude hiding (Reader)
import Coalmine.PtrKit.Peeker qualified as Peeker
import Data.ByteString.Internal qualified as ByteString

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
newtype Reader a = Reader
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

instance Applicative Reader where
  pure a = Reader $ \_ totalOffset ptr ptr' ->
    pure $ EmittingStatus a ptr totalOffset
  left <*> right =
    error "TODO"

instance Monad Reader where
  return = pure
  Reader runL >>= contR =
    Reader run
    where
      run path offset ptr ptr' =
        runL path offset ptr ptr' >>= processStatusL
        where
          processStatusL = \case
            EmittingStatus resL ptrL offsetL ->
              case contR resL of Reader runR -> runR path offsetL ptrL ptr'
            ExhaustedStatus runNextL ->
              return . ExhaustedStatus $ \ptr ptr' ->
                runNextL ptr ptr' >>= processStatusL

liftPeeker :: Peeker.Peeker a -> Reader a
liftPeeker =
  Reader . read
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

inContext :: Text -> Reader a -> Reader a
inContext =
  error "TODO"

failure :: Text -> Reader a
failure =
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
  Reader read
  where
    absValuePositiveBound = maxVal - valOffset
    absValueNegativeBound = valOffset - minVal
    read path startOffset =
      if maxVal == minVal
        then error "TODO: Fail with bad configuration"
        else readHead
      where
        bitsToOffset bits =
          startOffset + Integer.bytesNeededForBits bits
        readHead currentPtr afterPtr =
          if currentPtr < afterPtr
            then do
              byte <- peek @Word8 currentPtr
              if testBit byte 7
                then
                  let absValueState = fromIntegral (byte .&. 0b00111111)
                   in if absValueState > absValueNegativeBound
                        then error "TODO: handle value too large"
                        else
                          if testBit byte 6
                            then readTailInNegativeMode 6 absValueState (plusPtr currentPtr 1) afterPtr
                            else error "TODO: finish"
                else error "TODO: read tail in positive mode"
            else error "TODO: exhaust"
        readTailInNegativeMode !payloadBitOffset !absValueState currentPtr afterPtr =
          if absValueState > absValueNegativeBound
            then
              return $
                let message = "Value is smaller than " <> showAs minVal
                 in FailedStatus message path currentPtr startOffset
            else
              if currentPtr < afterPtr
                then do
                  byte <- peek currentPtr
                  if testBit byte 7
                    then error "TODO: loop"
                    else error "TODO: finish"
                else error "TODO: exhaust"
