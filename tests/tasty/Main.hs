module Main where

import Coalmine.Prelude
import Coalmine.Tasty
import Coalmine.TastyMonadic
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
  declareTestGroupDefaultMain "All" do
    declareListTestGroup "Inter" Inter.tests
    declareListTestGroup "LocatedRendering" LocatedRendering.tests
    declareListTestGroup "MegaparsecExtras" MegaparsecExtras.tests
    declareListTestGroup "TimeExtrasConversions" TimeExtrasConversions.tests
    declareListTestGroup "EvenSimplerPaths" EvenSimplerPaths.tests
    declareListTestGroup "ConduitExtras" ConduitExtras.tests
    declareListTestGroup "Name" Name.tests
    declareListTestGroup "NumericVersion" NumericVersion.tests
    declareTestGroup "CerealExtras" do
      declareListTestGroup "Compact" TestSuites.CerealExtras.Compact.tests
    declareTestGroup "BaseExtras" do
      declareListTestGroup "List" TestSuites.BaseExtras.List.tests
    declareListTestGroup "MultilineTextBuilder" MultilineTextBuilder.tests
    declareListTestGroup "PtrKit" PtrKit.tests

declareListTestGroup name =
  declareTestGroup name . traverse_ declareTestTree
