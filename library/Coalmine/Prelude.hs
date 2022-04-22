module Coalmine.Prelude
  ( module Exports,
    renderAsYamlText,
    parseTextLeniently,

    -- *
    MultilineTextBuilder,
  )
where

import Coalmine.BaseExtras.Alternative as Exports
import Coalmine.BaseExtras.Applicative as Exports
import Coalmine.BaseExtras.Function as Exports
import Coalmine.BaseExtras.MonadPlus as Exports
import Coalmine.Building as Exports
import Coalmine.CerealExtras.Instances as Exports ()
import Coalmine.DecimalExtras.Instances as Exports
import Coalmine.HCurrying as Exports
import Coalmine.HashableExtras as Exports
import Coalmine.InternalPrelude as Exports hiding (FilePath)
import Coalmine.Interval as Exports (Interval)
import Coalmine.MultilineTextBuilder as Exports (IsomorphicToMultilineTextBuilder (..))
import qualified Coalmine.MultilineTextBuilder as MultilineTextBuilder
import Coalmine.Name as Exports (FromNameInSpinalCase (..), FromNameInUpperCamelCase (..))
import Coalmine.Parsing as Exports
import Coalmine.Printing as Exports
import Coalmine.SimplePaths as Exports (DirPath, FilePath)
import Coalmine.StringIsomorphism as Exports
import Coalmine.TextIsomorphism as Exports
import Coalmine.TransformersExtras.Reader as Exports ()
import Coalmine.TransformersExtras.State as Exports ()
import qualified Data.Aeson
import qualified Data.Text.Encoding
import qualified Data.Yaml

renderAsYamlText :: Data.Aeson.ToJSON a => a -> Text
renderAsYamlText =
  Data.Text.Encoding.decodeUtf8
    . Data.Yaml.encode
    . Data.Aeson.toJSON

parseTextLeniently :: LenientParser a => Text -> Either Text a
parseTextLeniently =
  parse lenientParser

type MultilineTextBuilder = MultilineTextBuilder.Builder
