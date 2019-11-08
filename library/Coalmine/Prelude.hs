module Coalmine.Prelude
(
  module Prelude,
  module Exports,
  showAsText,
)
where

import Prelude
import Coalmine.Types as Exports
import Coalmine.Building as Exports
import Coalmine.Parsing as Exports
import Coalmine.Lens as Exports


showAsText :: Show a => a -> Text
showAsText = show >>> fromString
