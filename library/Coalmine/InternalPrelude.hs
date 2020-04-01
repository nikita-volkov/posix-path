module Coalmine.InternalPrelude
(
  module Prelude,
  module Exports,
  showAsText,
)
where

import Control.Foldl as Exports (Fold(..), FoldM(..))
import Data.DoubleWord as Exports (Word96(..), Word128(..), Word160(..), Word192(..), Word224(..), Word256(..))
import Data.String.ToString as Exports
import Data.Tuple.All as Exports
import Data.Vector.Generic as Exports (Vector)
import Data.Vector.Instances as Exports
import DeferredFolds.Unfoldr as Exports (Unfoldr(..))
import Deque.Strict as Exports (Deque)
import GHC.Exts as Exports (IsList(..))
import Network.IP.Addr as Exports (IP(..), IP4(..), IP6(..), NetAddr(..), InetAddr(..), InetPort(..))
import Prelude hiding (Vector, toList)
import Reduction as Exports (Reduction)
import Control.FromSum as Exports
import Data.Hashable.Time as Exports


showAsText :: Show a => a -> Text
showAsText = show >>> fromString
