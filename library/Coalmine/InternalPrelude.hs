module Coalmine.InternalPrelude
(
  module Prelude,
  module Exports,
  showAsText,
)
where

import Data.DoubleWord as Exports (Word96(..), Word128(..), Word160(..), Word192(..), Word224(..), Word256(..))
import Data.String.ToString as Exports
import Data.Tuple.All as Exports hiding (only)
import Data.Vector.Generic as Exports (Vector)
import Data.Vector.Instances as Exports
import Data.Vector.Unboxed as Exports (Unbox)
import DeferredFolds.Unfoldr as Exports (Unfoldr(..))
import Deque.Strict as Exports (Deque)
import GHC.Exts as Exports (IsList(..))
import Network.IP.Addr as Exports (IP(..), IP4(..), IP6(..), NetAddr(..), InetAddr(..), InetPort(..))
import Prelude hiding (Vector, toList, chosen, uncons, (%))
import Reduction as Exports (Reduction)
import Control.FromSum as Exports
import Data.Hashable.Time as Exports
import Data.Machine.Mealy as Exports
import Data.Machine.Moore as Exports
import Optics as Exports hiding (Curry)
import Data.Set.Optics as Exports
import Language.Haskell.TH.Quote as Exports (QuasiQuoter(..))
import Language.Haskell.TH.Syntax as Exports (Q, Lift)
import Acc as Exports (Acc)
import Acc.NeAcc as Exports (NeAcc)


showAsText :: Show a => a -> Text
showAsText = show >>> fromString
