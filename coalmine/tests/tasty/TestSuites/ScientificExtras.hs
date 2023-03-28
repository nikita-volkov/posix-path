module TestSuites.ScientificExtras where

import Coalmine.Prelude
import Coalmine.ScientificExtras qualified as ScientificExtras
import Coalmine.Tasty
import Coalmine.TastyMonadic

declare :: DeclareTestGroup ()
declare = do
  "scaleToDecimalsClipping" ?: do
    "Handles 0 resolution fine" ?! do
      assertEqual "" 33 $ ScientificExtras.scaleToDecimalsClipping 0 33
    "Normal" ?! do
      assertEqual "" 33 $ ScientificExtras.scaleToDecimalsClipping 2 0.33
      assertEqual "" 330 $ ScientificExtras.scaleToDecimalsClipping 3 0.33
      assertEqual "" 324987 $ ScientificExtras.scaleToDecimalsClipping 3 324.987
      assertEqual "" (-330) $ ScientificExtras.scaleToDecimalsClipping 3 (-0.33)
      assertEqual "" (-324987) $ ScientificExtras.scaleToDecimalsClipping 3 (-324.987)
    "Clips anything down" ?! do
      assertEqual "" 324987 $ ScientificExtras.scaleToDecimalsClipping 3 324.98768
      assertEqual "" 324987 $ ScientificExtras.scaleToDecimalsClipping 3 324.9876
      assertEqual "" 324987 $ ScientificExtras.scaleToDecimalsClipping 3 324.9874
      assertEqual "" (-324988) $ ScientificExtras.scaleToDecimalsClipping 3 (-324.98768)
      assertEqual "" (-324988) $ ScientificExtras.scaleToDecimalsClipping 3 (-324.9876)
      assertEqual "" (-324988) $ ScientificExtras.scaleToDecimalsClipping 3 (-324.9874)
  "scaleToDecimalsIfFits" ?: do
    "Normal" ?! do
      assertEqual "" (Just 33) $ ScientificExtras.scaleToDecimalsIfFits 2 0.33
      assertEqual "" (Just 330) $ ScientificExtras.scaleToDecimalsIfFits 3 0.33
      assertEqual "" (Just 324987) $ ScientificExtras.scaleToDecimalsIfFits 3 324.987
      assertEqual "" (Just (-330)) $ ScientificExtras.scaleToDecimalsIfFits 3 (-0.33)
      assertEqual "" (Just (-324987)) $ ScientificExtras.scaleToDecimalsIfFits 3 (-324.987)
    "Fails when appropriate" ?! do
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimalsIfFits 3 324.98768
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimalsIfFits 3 324.9876
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimalsIfFits 3 324.9874
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimalsIfFits 3 (-324.98768)
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimalsIfFits 3 (-324.9876)
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimalsIfFits 3 (-324.9874)
