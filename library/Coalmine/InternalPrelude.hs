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
  )
where

import Acc as Exports (Acc)
import Acc.NeAcc as Exports (NeAcc)
import Attoparsec.Data as Exports (LenientParser (..))
import Control.Foldl as Exports (EndoM (..))
import Control.FromSum as Exports
import Control.Monad.Morph as Exports
import Data.Aeson as Exports (FromJSON (..), ToJSON (..), ToJSONKey (..))
import qualified Data.Aeson
import qualified Data.Attoparsec.Text
import Data.Decimal as Exports (Decimal)
import Data.Default as Exports
import Data.DoubleWord as Exports (Word128 (..), Word160 (..), Word192 (..), Word224 (..), Word256 (..), Word96 (..))
import Data.Functor.Invariant as Exports
import Data.Group as Exports
import Data.Groupoid as Exports
import Data.Machine.Mealy as Exports
import Data.Machine.Moore as Exports
import Data.Semialign as Exports
import qualified Data.Text.Encoding
import Data.Time.Compat as Exports ()
import Data.Time.Format.ISO8601 as Exports (ISO8601 (..), iso8601ParseM, iso8601Show)
import Data.Tuple.All as Exports hiding (only)
import qualified Data.Vector
import Data.Vector.Generic as Exports (Vector)
import Data.Vector.Instances as Exports
import qualified Data.Vector.Storable
import Data.Vector.Unboxed as Exports (Unbox)
import qualified Data.Vector.Unboxed
import Data.Vector.Unboxed.Deriving as Exports (derivingUnbox)
import qualified Data.Yaml
import DeferredFolds.Unfoldr as Exports (Unfoldr (..))
import Deque.Strict as Exports (Deque)
import Foreign.C.Types as Exports
import GHC.Exts as Exports (IsList (..))
import GHC.Utils.Misc as Exports (applyWhen, nTimes)
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

parseTextLeniently :: LenientParser a => Text -> Either Text a
parseTextLeniently =
  first fromString
    . Data.Attoparsec.Text.parseOnly (lenientParser <* Data.Attoparsec.Text.endOfInput)

renderAsYamlText :: Data.Aeson.ToJSON a => a -> Text
renderAsYamlText =
  Data.Text.Encoding.decodeUtf8
    . Data.Yaml.encode
    . Data.Aeson.toJSON

-- * UTF8

decodeUtf8 = Data.Text.Encoding.decodeUtf8'
