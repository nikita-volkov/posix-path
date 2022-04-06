module Coalmine.Prelude
  ( module Exports,
    renderAsYamlText,
  )
where

import Coalmine.BaseExtras.Alternative as Exports
import Coalmine.BaseExtras.Applicative as Exports
import Coalmine.BaseExtras.Function as Exports
import Coalmine.Building as Exports
import Coalmine.CerealExtras.Instances as Exports ()
import Coalmine.DecimalExtras.Instances as Exports
import Coalmine.HCurrying as Exports
import Coalmine.HashableExtras as Exports
import Coalmine.InternalPrelude as Exports hiding (FilePath)
import Coalmine.Interval as Exports (Interval)
import Coalmine.MultilineTextBuilder as Exports (ToMultilineTextBuilder (..))
import Coalmine.Parsing as Exports
import Coalmine.SimplePaths as Exports (DirPath, FilePath)
import Coalmine.Types as Exports
import qualified Data.Aeson
import qualified Data.Text.Encoding
import qualified Data.Yaml

renderAsYamlText :: Data.Aeson.ToJSON a => a -> Text
renderAsYamlText =
  Data.Text.Encoding.decodeUtf8
    . Data.Yaml.encode
    . Data.Aeson.toJSON
