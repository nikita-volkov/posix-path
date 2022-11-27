module Coalmine.PtrKit.ImmediatePoker
  ( ImmediatePoker,

    -- * Elimination
    run,
    toByteString,

    -- * Construction and transformation
    inContext,
    failure,
    varLengthUnsignedInteger,

    -- * Errors
    ByteStringErr (..),
  )
where

import Coalmine.InternalPrelude
import Coalmine.PtrKit.PtrIO qualified as PtrIO
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
    poke ::
      Ptr Word8 ->
      -- Context for errors.
      [Text] ->
      IO (Ptr Word8)
  }

instance Semigroup ImmediatePoker where
  left <> right =
    error "TODO"

instance Monoid ImmediatePoker where
  mempty = ImmediatePoker 0 (\ptr _ -> return ptr)

run ::
  ImmediatePoker ->
  -- | Action providing a pointer of the requested capacity.
  (Int -> IO (Ptr Word8)) ->
  -- | Action throwing 'ControlException'.
  IO (Ptr Word8)
run =
  error "TODO"

toByteString :: ImmediatePoker -> Either ByteStringErr ByteString
toByteString (ImmediatePoker maxSize poke) =
  unsafeDupablePerformIO $ do
    fp <- mallocPlainForeignPtrBytes maxSize
    withForeignPtr fp $ \p ->
      catch
        ( poke p [] <&> \pAfter ->
            Right (ByteStringInternal.BS fp (minusPtr pAfter p))
        )
        ( \(ControlException pAfter context reason) ->
            return . Left . ByteStringErr reason context . ByteStringInternal.BS fp $
              minusPtr pAfter p
        )

-- * Constructors

inContext :: Text -> ImmediatePoker -> ImmediatePoker
inContext context (ImmediatePoker size poke) =
  ImmediatePoker size (\ptr contexts -> poke ptr (context : contexts))

failure :: Text -> ImmediatePoker
failure reason =
  ImmediatePoker 0 (\ptr context -> throwIO (ControlException ptr context reason))

-- |
-- Variable length representation of unsigned integers.
--
-- Uses the 8th bit of each octet to specify, whether another octet is needed.
varLengthUnsignedInteger :: (Integral a, Bits a) => a -> ImmediatePoker
varLengthUnsignedInteger =
  -- A two-phase implementation:
  -- 1. Aggregate the size and metadata required for poking.
  -- 2. Use the metadata to optimize the poking action.
  processValue
  where
    processValue value =
      if value < 0
        then inContext "varLengthUnsignedInteger" (failure "Negative value")
        else processValidValue 0 [] value

    processValidValue !offset !byteRevList value =
      case nextValue of
        0 ->
          processMetadata offset (fromIntegral value) byteRevList
        _ ->
          processValidValue (succ offset) (byte : byteRevList) nextValue
          where
            !byte = setBit (fromIntegral value) 7
      where
        nextValue = unsafeShiftR value 7

    processMetadata lastOffset head tail =
      ImmediatePoker size poke
      where
        size = succ lastOffset
        poke ptr _ =
          PtrIO.backPokeByteRevListWithHead lastPtr head tail
            $> plusPtr ptr size
          where
            lastPtr = plusPtr ptr lastOffset

-- * Errors

-- | Exception used as an internal control signal for efficiency.
--
-- Does not get exposed to the user.
data ControlException
  = ControlException
      (Ptr Word8)
      -- ^ Pointer that was reached before the exception got thrown.
      --
      -- Helpful for defining location.
      [Text]
      -- ^ Context.
      Text
      -- ^ User-supplied error reason.
  deriving (Show)

instance Exception ControlException

data ByteStringErr = ByteStringErr
  { reason :: Text,
    context :: [Text],
    -- | The so far constructed unfinished bytestring.
    byteString :: ByteString
  }
