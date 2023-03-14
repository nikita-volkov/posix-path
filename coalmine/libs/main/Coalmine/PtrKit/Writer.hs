module Coalmine.PtrKit.Writer
  ( Writer (..),

    -- * Elimination
    toByteString,
    run,

    -- * Construction
    boundedPrim,
    fixedPrim,
  )
where

import Coalmine.InternalPrelude hiding (Writer)
import Data.ByteString.Builder.Prim qualified as ByteStringBuilderPrim
import Data.ByteString.Builder.Prim.Internal qualified as ByteStringBuilderPrimInternal
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

-- * Construction

-- |
-- \(\mathcal{O}(1)\).
-- Lift a standard bytestring builder primitive that always
-- results in sequence of bytes that is no longer than a pre-determined
-- bound.
{-# INLINE boundedPrim #-}
boundedPrim :: ByteStringBuilderPrim.BoundedPrim a -> a -> Writer
boundedPrim builder =
  let !size = ByteStringBuilderPrimInternal.sizeBound builder
   in \val ->
        let poke = ByteStringBuilderPrimInternal.runB builder val
         in Writer size poke

-- |
-- \(\mathcal{O}(1)\).
-- Lift a standard bytestring builder primitive that always
-- results in a sequence of bytes of a pre-determined, fixed size.
{-# INLINE fixedPrim #-}
fixedPrim :: ByteStringBuilderPrim.FixedPrim a -> a -> Writer
fixedPrim builder =
  Writer size . poke
  where
    size = ByteStringBuilderPrimInternal.size builder
    poke val ptr =
      ByteStringBuilderPrimInternal.runF builder val ptr
        $> plusPtr ptr size
