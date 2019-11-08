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


showAsText :: Show a => a -> Text
showAsText = show >>> fromString
