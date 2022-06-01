module Coalmine.Prelude
  ( module Exports,

    -- * --
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
import Coalmine.EvenSimplerPaths as Exports (Path)
import Coalmine.HCurrying as Exports
import Coalmine.HashableExtras as Exports
import Coalmine.InternalPrelude as Exports hiding (FilePath)
import Coalmine.Interval as Exports (Interval)
import Coalmine.IsomorphismClassInstances as Exports ()
import Coalmine.MtlExtras as Exports
import qualified Coalmine.MultilineTextBuilder as MultilineTextBuilder
import Coalmine.NameConversion as Exports
import Coalmine.Parsing as Exports
import Coalmine.Printing as Exports
import Coalmine.SimplePaths as Exports (DirPath, FilePath)
import Coalmine.TransformersExtras.Reader as Exports ()
import Coalmine.TransformersExtras.State as Exports ()

type MultilineTextBuilder = MultilineTextBuilder.Builder
