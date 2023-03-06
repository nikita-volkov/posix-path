module Coalmine.InternalPrelude
  ( module Prelude,
    module Exports,

    -- * --
    traceMap,

    -- * --
    E8,

    -- * --
    BVec,
    UVec,
    SVec,

    -- * --
    renderAsYamlText,
    parseTextLeniently,

    -- * UTF8
    Data.Text.Encoding.encodeUtf8,
    decodeUtf8,

    -- * MonadError
    tryError,
    withError,
    handleError,
    mapError,
  )
where

import Acc as Exports (Acc)
import Acc.NeAcc as Exports (NeAcc)
import Attoparsec.Data as Exports (LenientParser (..))
import Control.Foldl as Exports (EndoM (..))
import Control.FromSum as Exports
import Data.Aeson as Exports (FromJSON (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson qualified
import Data.Attoparsec.Text qualified
import Data.ByteString.Lazy as Exports (LazyByteString)
import Data.Decimal as Exports (Decimal)
import Data.Default as Exports
import Data.DoubleWord as Exports (Word128 (..), Word160 (..), Word192 (..), Word224 (..), Word256 (..), Word96 (..))
import Data.Functor.Invariant as Exports
import Data.Group as Exports
import Data.Groupoid as Exports
import Data.Machine.Mealy as Exports
import Data.Machine.Moore as Exports
import Data.Semialign as Exports
import Data.Text.Encoding qualified
import Data.Tuple.All as Exports hiding (only)
import Data.Vector qualified
import Data.Vector.Generic as Exports (Vector)
import Data.Vector.Instances as Exports
import Data.Vector.Storable qualified
import Data.Vector.Unboxed as Exports (Unbox)
import Data.Vector.Unboxed qualified
import Data.Vector.Unboxed.Deriving as Exports (derivingUnbox)
import Data.Yaml qualified
import DeferredFolds.Unfoldr as Exports (Unfoldr (..))
import Deque.Strict as Exports (Deque)
import Foreign.C.Types as Exports
import Foreign.Marshal.Alloc as Exports
import GHC.Exts as Exports (IsList (..))
import GHC.ForeignPtr as Exports (mallocPlainForeignPtrBytes, unsafeWithForeignPtr)
import GHC.Utils.Misc as Exports (applyWhen, nTimes, nubSort)
import IsomorphismClass as Exports
import Language.Haskell.TH.Quote as Exports (QuasiQuoter (..))
import Language.Haskell.TH.Syntax as Exports (Lift, Q)
import Network.IP.Addr as Exports (IP (..), IP4 (..), IP6 (..), InetAddr (..), InetPort (..), NetAddr (..))
import System.IO as Exports
import Test.QuickCheck.Arbitrary as Exports (Arbitrary)
import Test.QuickCheck.Instances ()
import TextBuilderDev as Exports (IsomorphicToTextBuilder (..), TextBuilder, buildText)
import Witherable as Exports
import Prelude hiding (Vector, catMaybes, chosen, filter, mapMaybe, repeat, toList, uncons, unzip, zip, zipWith, (%))

-- * --

traceMap :: (a -> String) -> a -> a
traceMap f a = trace (f a) a

-- * --

data E8

instance HasResolution E8 where
  resolution _ = 100000000

-- * --

type BVec = Data.Vector.Vector

type UVec = Data.Vector.Unboxed.Vector

type SVec = Data.Vector.Storable.Vector

-- * --

parseTextLeniently :: (LenientParser a) => Text -> Either Text a
parseTextLeniently =
  first fromString
    . Data.Attoparsec.Text.parseOnly (lenientParser <* Data.Attoparsec.Text.endOfInput)

renderAsYamlText :: (Data.Aeson.ToJSON a) => a -> Text
renderAsYamlText =
  Data.Text.Encoding.decodeUtf8
    . Data.Yaml.encode
    . Data.Aeson.toJSON

-- * UTF8

decodeUtf8 = Data.Text.Encoding.decodeUtf8'

-- * MonadError

-- | 'MonadError' analogue to the 'Control.Exception.try' function.
tryError :: (MonadError e m) => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | 'MonadError' analogue to the 'withExceptT' function.
-- Modify the value (but not the type) of an error.  The type is
-- fixed because of the functional dependency @m -> e@.  If you need
-- to change the type of @e@ use 'mapError'.
withError :: (MonadError e m) => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) pure

-- | As 'handle' is flipped 'Control.Exception.catch', 'handleError'
-- is flipped 'catchError'.
handleError :: (MonadError e m) => (e -> m a) -> m a -> m a
handleError = flip catchError

-- | 'MonadError' analogue of the 'mapExceptT' function.  The
-- computation is unwrapped, a function is applied to the @Either@, and
-- the result is lifted into the second 'MonadError' instance.
mapError :: (MonadError e m, MonadError e' n) => (m (Either e a) -> n (Either e' b)) -> m a -> n b
mapError f action = f (tryError action) >>= liftEither
