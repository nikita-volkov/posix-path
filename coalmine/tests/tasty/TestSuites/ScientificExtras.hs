module TestSuites.ScientificExtras where

import Coalmine.ScientificExtras qualified as ScientificExtras
import Coalmine.Prelude
import Coalmine.Tasty

tests :: [TestTree]
tests =
  [ testCase "Samples" $ do
      assertEqual "" 33 $ ScientificExtras.scaleToDecimals 2 0.33
      assertEqual "" 330 $ ScientificExtras.scaleToDecimals 3 0.33
      assertEqual "" 324987 $ ScientificExtras.scaleToDecimals 3 324.987
      assertEqual "" (-330) $ ScientificExtras.scaleToDecimals 3 (-0.33)
      assertEqual "" (-324987) $ ScientificExtras.scaleToDecimals 3 (-324.987),
    testCase "Clips anything down" $ do
      assertEqual "" 324987 $ ScientificExtras.scaleToDecimals 3 324.98768
      assertEqual "" 324987 $ ScientificExtras.scaleToDecimals 3 324.9876
      assertEqual "" 324987 $ ScientificExtras.scaleToDecimals 3 324.9874
      assertEqual "" (-324988) $ ScientificExtras.scaleToDecimals 3 (-324.98768)
      assertEqual "" (-324988) $ ScientificExtras.scaleToDecimals 3 (-324.9876)
      assertEqual "" (-324988) $ ScientificExtras.scaleToDecimals 3 (-324.9874)
  ]
