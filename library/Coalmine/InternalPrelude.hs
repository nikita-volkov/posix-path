module Coalmine.InternalPrelude
  ( module Prelude,
    module Exports,
    showAsText,
  )
where

import Acc as Exports (Acc)
import Acc.NeAcc as Exports (NeAcc)
import Control.FromSum as Exports
import Data.DoubleWord as Exports (Word128 (..), Word160 (..), Word192 (..), Word224 (..), Word256 (..), Word96 (..))
import Data.Functor.Invariant as Exports
import Data.Group as Exports
import Data.Groupoid as Exports
import Data.Hashable.Time as Exports
import Data.Machine.Mealy as Exports
import Data.Machine.Moore as Exports
import Data.String.ToString as Exports
import Data.Text.Conversions as Exports
import Data.Tuple.All as Exports hiding (only)
import Data.Vector.Generic as Exports (Vector)
import Data.Vector.Instances as Exports
import Data.Vector.Unboxed as Exports (Unbox)
import Data.Vector.Unboxed.Deriving as Exports (derivingUnbox)
import DeferredFolds.Unfoldr as Exports (Unfoldr (..))
import Deque.Strict as Exports (Deque)
import GHC.Exts as Exports (IsList (..))
import Language.Haskell.TH.Quote as Exports (QuasiQuoter (..))
import Language.Haskell.TH.Syntax as Exports (Lift, Q)
import Network.IP.Addr as Exports (IP (..), IP4 (..), IP6 (..), InetAddr (..), InetPort (..), NetAddr (..))
import TextBuilder (TextBuilder, buildText)
import qualified TextBuilder
import Prelude hiding (Vector, chosen, toList, uncons, (%))

showAsText :: Show a => a -> Text
showAsText = show >>> fromString
