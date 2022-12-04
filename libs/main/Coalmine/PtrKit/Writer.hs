module Coalmine.PtrKit.Writer
  ( Writer (..),

    -- * Elimination
    toByteString,
  )
where

import Coalmine.InternalPrelude hiding (Writer)
import Coalmine.PtrKit.PtrIO qualified as PtrIO
import Data.ByteString.Internal qualified as ByteStringInternal

data Writer =
  -- Should not be exported, since we want to keep the control
  -- over the ways of executing this abstraction.
  Writer
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
    poke :: Ptr Word8 -> IO (Ptr Word8)
  }

instance Semigroup Writer where
  left <> right =
    error "TODO"

instance Monoid Writer where
  mempty = Writer 0 pure

run ::
  Writer ->
  -- | Action providing a pointer of the requested capacity.
  (Int -> IO (Ptr Word8)) ->
  IO (Ptr Word8)
run (Writer size poke) alloc =
  error "TODO"

-- |
-- May allocate a bytestring that occupies redundant memory.
--
-- Use 'Data.ByteString.copy' to compact it, when that\'s important.
toByteString :: Writer -> ByteString
toByteString (Writer maxSize poke) =
  unsafeDupablePerformIO $ do
    fp <- mallocPlainForeignPtrBytes maxSize
    actualSize <- withForeignPtr fp $ \p ->
      poke p <&> \pAfter -> minusPtr pAfter p
    evaluate (ByteStringInternal.BS fp actualSize)
