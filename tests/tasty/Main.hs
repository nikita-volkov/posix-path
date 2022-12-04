module Main where

import Coalmine.Prelude
import Coalmine.Tasty
import TestSuites.BaseExtras.List qualified
import TestSuites.CerealExtras.Compact qualified
import TestSuites.ConduitExtras qualified as ConduitExtras
import TestSuites.EvenSimplerPaths qualified as EvenSimplerPaths
import TestSuites.Inter qualified as Inter
import TestSuites.LocatedRendering qualified as LocatedRendering
import TestSuites.MegaparsecExtras qualified as MegaparsecExtras
import TestSuites.MultilineTextBuilder qualified as MultilineTextBuilder
import TestSuites.Name qualified as Name
import TestSuites.NumericVersion qualified as NumericVersion
import TestSuites.PtrKit qualified as PtrKit
import TestSuites.TimeExtrasConversions qualified as TimeExtrasConversions

main =
  defaultMain . testGroup "All" $
    [ testGroup "Inter" Inter.tests,
      testGroup "LocatedRendering" LocatedRendering.tests,
      testGroup "MegaparsecExtras" MegaparsecExtras.tests,
      testGroup "TimeExtrasConversions" TimeExtrasConversions.tests,
      testGroup "EvenSimplerPaths" EvenSimplerPaths.tests,
      testGroup "ConduitExtras" ConduitExtras.tests,
      testGroup "Name" Name.tests,
      testGroup "NumericVersion" NumericVersion.tests,
      testGroup "CerealExtras" $
        [ testGroup "Compact" TestSuites.CerealExtras.Compact.tests
        ],
      testGroup "BaseExtras" $
        [ testGroup "List" TestSuites.BaseExtras.List.tests
        ],
      testGroup "MultilineTextBuilder" MultilineTextBuilder.tests,
      testGroup "PtrKit" PtrKit.tests
    ]
