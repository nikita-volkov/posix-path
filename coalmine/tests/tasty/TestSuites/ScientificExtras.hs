module TestSuites.ScientificExtras where

import Coalmine.Prelude
import Coalmine.ScientificExtras qualified as ScientificExtras
import Coalmine.Tasty
import Coalmine.TastyMonadic

declare :: DeclareTestGroup ()
declare = do
  "scaleToDecimalsClipping" ?: do
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
  "scaleToDecimals" ?: do
    "Normal" ?! do
      assertEqual "" (Just 33) $ ScientificExtras.scaleToDecimals 2 0.33
      assertEqual "" (Just 330) $ ScientificExtras.scaleToDecimals 3 0.33
      assertEqual "" (Just 324987) $ ScientificExtras.scaleToDecimals 3 324.987
      assertEqual "" (Just (-330)) $ ScientificExtras.scaleToDecimals 3 (-0.33)
      assertEqual "" (Just (-324987)) $ ScientificExtras.scaleToDecimals 3 (-324.987)
    "Clips anything down" ?! do
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimals 3 324.98768
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimals 3 324.9876
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimals 3 324.9874
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimals 3 (-324.98768)
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimals 3 (-324.9876)
      assertEqual "" Nothing $ ScientificExtras.scaleToDecimals 3 (-324.9874)
