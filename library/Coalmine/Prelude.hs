module Coalmine.Prelude
  ( module Exports,
    renderAsYamlText,
  )
where

import Coalmine.Applicative as Exports
import Coalmine.Building as Exports
import Coalmine.HCurrying as Exports
import Coalmine.HashableExtras as Exports
import Coalmine.InternalPrelude as Exports
import Coalmine.MultilineTextBuilder as Exports (ToMultilineTextBuilder (..))
import Coalmine.Parsing as Exports
import Coalmine.Types as Exports
import qualified Data.Aeson
import qualified Data.Text.Encoding
import qualified Data.Yaml

renderAsYamlText :: Data.Aeson.ToJSON a => a -> Text
renderAsYamlText =
  Data.Text.Encoding.decodeUtf8
    . Data.Yaml.encode
    . Data.Aeson.toJSON
