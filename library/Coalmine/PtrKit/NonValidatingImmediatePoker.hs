module Coalmine.PtrKit.NonValidatingImmediatePoker
  ( NonValidatingImmediatePoker,

    -- * Elimination
    toByteString,

    -- * Construction and transformation
    varLengthUnsignedInteger,
  )
where

import Coalmine.InternalPrelude
import Coalmine.PtrKit.ImmediatePoker.PtrIO qualified as PtrIO
import Data.ByteString.Internal qualified as ByteStringInternal

data NonValidatingImmediatePoker =
  -- Should not be exported, since we want to keep the control
  -- over the ways of executing this abstraction.
  NonValidatingImmediatePoker
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

instance Semigroup NonValidatingImmediatePoker where
  left <> right =
    error "TODO"

instance Monoid NonValidatingImmediatePoker where
  mempty = NonValidatingImmediatePoker 0 pure

run ::
  NonValidatingImmediatePoker ->
  -- | Action providing a pointer of the requested capacity.
  (Int -> IO (Ptr Word8)) ->
  IO (Ptr Word8)
run (NonValidatingImmediatePoker size poke) alloc =
  error "TODO"

toByteString :: NonValidatingImmediatePoker -> ByteString
toByteString (NonValidatingImmediatePoker maxSize poke) =
  unsafeDupablePerformIO $ do
    fp <- mallocPlainForeignPtrBytes maxSize
    actualSize <- withForeignPtr fp $ \p ->
      poke p <&> \pAfter -> minusPtr pAfter p
    evaluate (ByteStringInternal.BS fp actualSize)

-- * Constructors

-- |
-- Variable length representation of unsigned integers.
--
-- Uses the 8th bit of each octet to specify, whether another octet is needed.
--
-- __Warning:__
-- It is your responsibility to ensure that the value is non-negative,
-- otherwise the encoder will fall into an infinite loop.
varLengthUnsignedInteger :: (Integral a, Bits a) => a -> NonValidatingImmediatePoker
varLengthUnsignedInteger =
  -- A two-phase implementation:
  -- 1. Aggregate the size and metadata required for poking.
  -- 2. Use the metadata to optimize the poking action.
  processValue 0 []
  where
    processValue !offset !byteRevList value =
      case nextValue of
        0 ->
          processMetadata offset (fromIntegral value) byteRevList
        _ ->
          processValue (succ offset) (byte : byteRevList) nextValue
          where
            !byte = setBit (fromIntegral value) 7
      where
        nextValue = unsafeShiftR value 7

    processMetadata lastOffset head tail =
      NonValidatingImmediatePoker size poke
      where
        size = succ lastOffset
        poke ptr =
          PtrIO.backPokeByteRevListWithHead lastPtr head tail
            $> plusPtr ptr size
          where
            lastPtr = plusPtr ptr lastOffset
