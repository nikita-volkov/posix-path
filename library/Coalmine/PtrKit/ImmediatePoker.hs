module Coalmine.PtrKit.ImmediatePoker
  ( ImmediatePoker,
    run,
    toByteString,
    failure,

    -- * Errors
    ByteStringErr (..),
  )
where

import Coalmine.InternalPrelude
import Data.ByteString.Internal qualified as ByteStringInternal

data ImmediatePoker =
  -- Should not be exported, since we want to keep the control
  -- over the ways of executing this abstraction.
  ImmediatePoker
  { -- | How many bytes it might occupy.
    --
    -- Determines how much to allocate.
    size :: Int,
    -- | Action writing to the pointer of the requested capacity,
    -- which is specified in the 'size' field.
    --
    -- Utilizes exceptions to transfer errors for composition efficiency. The
    -- idea is that we don\'t need to catch these exceptions when doing the
    -- internal composition, since the first exception should be propagated
    -- to the point of execution.
    --
    -- It does not however export these exceptions and executes
    -- into an exceptionless action via the exported API.
    run :: Ptr Word8 -> IO (Ptr Word8)
  }

instance Semigroup ImmediatePoker where
  left <> right =
    error "TODO"

instance Monoid ImmediatePoker where
  mempty = ImmediatePoker 0 pure

run ::
  ImmediatePoker ->
  -- | Action providing a pointer of the requested capacity.
  (Int -> IO (Ptr Word8)) ->
  -- | Action throwing 'ControlException'.
  IO (Ptr Word8)
run =
  error "TODO"

toByteString :: ImmediatePoker -> Either ByteStringErr ByteString
toByteString (ImmediatePoker maxSize run) =
  unsafeDupablePerformIO $ do
    fp <- mallocPlainForeignPtrBytes maxSize
    withForeignPtr fp $ \p ->
      catch
        ( run p <&> \pAfter ->
            Right (ByteStringInternal.BS fp (minusPtr pAfter p))
        )
        ( \(UserControlException pAfter reason) ->
            return . Left . ByteStringErr reason . ByteStringInternal.BS fp $
              minusPtr pAfter p
        )

failure :: Text -> ImmediatePoker
failure reason =
  ImmediatePoker 0 (\ptr -> throwIO (UserControlException ptr reason))

-- | Exception used as an internal control signal for efficiency.
--
-- Does not get exposed to the user.
data ControlException
  = UserControlException
      (Ptr Word8)
      -- ^ Pointer that was reached before the exception got thrown.
      --
      -- Helpful for defining location.
      Text
      -- ^ User-supplied error reason.
  deriving (Show)

instance Exception ControlException

data ByteStringErr = ByteStringErr
  { reason :: Text,
    -- | The so far constructed unfinished bytestring.
    byteString :: ByteString
  }
