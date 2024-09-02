module Coalmine.PtrKit.Reader where

import Coalmine.InternalPrelude hiding (Failure, Reader)
import Coalmine.PtrKit.Peeker qualified as Peeker
import Data.ByteString.Internal qualified as ByteStringInternal

-- | Result of processing one chunk of a streamed input.
data Status a
  = -- | Failed.
    FailedStatus
      -- | Message.
      Text
      -- | Contexts.
      [Text]
      -- | Total offset amongst all consumed inputs before the beginning of this value.
      Int
  | EmittingStatus
      -- | Result.
      a
      -- | Pointer to read the following data from.
      (Ptr Word8)
      -- | Total offset amongst all consumed inputs.
      Int
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

data Failure = Failure
  { -- | Message.
    message :: Text,
    -- | Contexts.
    contexts :: [Text],
    -- | Total offset amongst all consumed inputs before the beginning of this value.
    offset :: Int
  }
  deriving (Show, Eq)

readTotalByteString :: Reader a -> ByteString -> IO (Either Failure a)
readTotalByteString reader (ByteStringInternal.BS fp len) =
  withForeignPtr fp $ \ptr ->
    reader.run [] 0 ptr (plusPtr ptr len) <&> \case
      EmittingStatus res _ptrAfter totalOffset ->
        if totalOffset == len
          then Right res
          else Left $ Failure "Did not consume in whole" [] totalOffset
      _ -> error "TODO"

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
      -- Last data pointer. Initial pointer plus length.
      Ptr Word8 ->
      -- Result of processing one chunk.
      IO (Status a)
  }
  deriving (Functor)

instance Applicative Reader where
  pure a = Reader $ \_ totalOffset ptr _ptr' ->
    pure $ EmittingStatus a ptr totalOffset
  _left <*> _right =
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
            FailedStatus _ _ _ ->
              error "TODO"

liftPeeker :: Peeker.Peeker a -> Reader a
liftPeeker =
  Reader . read
  where
    read peeker path !offset ptr ptr' =
      peeker.run ptr ptr' <&> \case
        Peeker.FailedStatus message ptr'' ->
          FailedStatus message path (offset + minusPtr ptr'' ptr)
        Peeker.EmittingStatus result ptr'' _ ->
          EmittingStatus result ptr'' (offset + minusPtr ptr'' ptr)
        Peeker.ExhaustedStatus nextPeeker ->
          ExhaustedStatus
            $ read nextPeeker path (offset + minusPtr ptr' ptr)

inContext :: Text -> Reader a -> Reader a
inContext =
  error "TODO"

failure :: Text -> Reader a
failure =
  error "TODO"
