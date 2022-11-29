module Coalmine.PtrKit.Reader where

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
      -- ^ Total offset amongst all consumed inputs.
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
      (Reader a)
      -- ^ Updated reader to feed to eventually acquire the result.
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
    Reader $ \path offset ptr ptr' ->
      runL path offset ptr ptr' >>= \case
        EmittingStatus resL ptrL offsetL ->
          case contR resL of Reader runR -> runR path offsetL ptrL ptr'
        ExhaustedStatus nextL ->
          return $ ExhaustedStatus $ nextL >>= contR

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
            Reader $ \path offset ->
              read nextPeeker path (offset + minusPtr ptr' ptr)

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
  (Integral a, Bits a) =>
  -- | Minimum value.
  a ->
  -- | Maximum value.
  a ->
  -- | Zero offset. Points to the most probable value.
  a ->
  Reader a
varLengthSignedInteger =
  error "TODO"
