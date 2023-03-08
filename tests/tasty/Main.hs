module Main where

import Coalmine.Prelude
import Coalmine.Tasty
import Coalmine.TastyMonadic
import TestSuites.BaseExtras.List qualified
import TestSuites.CerealExtras.Compact qualified
import TestSuites.ConduitExtras qualified as ConduitExtras
import TestSuites.EvenSimplerPaths qualified as EvenSimplerPaths
import TestSuites.Inter qualified as Inter
import TestSuites.LeanPaths qualified as LeanPaths
import TestSuites.LocatedRendering qualified as LocatedRendering
import TestSuites.MegaparsecExtras qualified as MegaparsecExtras
import TestSuites.MultilineTextBuilder qualified as MultilineTextBuilder
import TestSuites.Name qualified as Name
import TestSuites.NumericVersion qualified as NumericVersion
import TestSuites.PtrKit qualified as PtrKit
import TestSuites.TimeExtrasConversions qualified as TimeExtrasConversions

main =
  declareTestGroupDefaultMain "All" do
    "Inter" ?:: Inter.tests
    "LocatedRendering" ?:: LocatedRendering.tests
    "MegaparsecExtras" ?:: MegaparsecExtras.tests
    "TimeExtrasConversions" ?:: TimeExtrasConversions.tests
    "EvenSimplerPaths" ?:: EvenSimplerPaths.tests
    "ConduitExtras" ?:: ConduitExtras.tests
    "Name" ?:: Name.tests
    "NumericVersion" ?:: NumericVersion.tests
    "CerealExtras" ?: do
      "Compact" ?:: TestSuites.CerealExtras.Compact.tests
    "BaseExtras" ?: do
      "List" ?:: TestSuites.BaseExtras.List.tests
    "MultilineTextBuilder" ?:: MultilineTextBuilder.tests
    "PtrKit" ?:: PtrKit.tests
    "LeanPaths" ?:: LeanPaths.tests

(?::) name =
  declareTestGroup name . traverse_ declareTestTree
