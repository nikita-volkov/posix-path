module Coalmine.Prelude
(
  module Prelude,
  module Exports,
  showAsText,
)
where

import Prelude
import Coalmine.Applicative as Exports
import Coalmine.Building as Exports
import Coalmine.Lens as Exports
import Coalmine.Parsing as Exports
import Coalmine.Types as Exports
import Reduction as Exports (Reduction)
import Data.DoubleWord as Exports (Word96(..), Word128(..), Word160(..), Word192(..), Word224(..), Word256(..))
import Deque.Strict as Exports (Deque)
import Control.Foldl as Exports (Fold(..), FoldM(..))
import Data.Tuple.All as Exports
import Data.String.ToString as Exports


showAsText :: Show a => a -> Text
showAsText = show >>> fromString
