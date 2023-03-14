module TestSuites.PtrKit where

import Coalmine.Comms.Readers qualified as Readers
import Coalmine.Comms.Writers qualified as Writers
import Coalmine.Prelude hiding (choose)
import Coalmine.PtrKitQuickCheck.Properties qualified as PtrKitProperties
import Coalmine.Tasty

tests :: [TestTree]
tests =
  []

varLengthSignedInteger :: TestTree
varLengthSignedInteger =
  testProperty "varLengthSignedInteger" $
    let gen = do
          min <- arbitrary @Int64
          max <- choose (min, maxBound)
          center <- choose (min, max)
          val <- choose (min, max)
          return (min, max, center, val)
     in forAll gen $ \(min, max, center, val) ->
          PtrKitProperties.writeReadRoundtrip
            Writers.varLengthSignedInteger
            (Readers.varLengthSignedInteger min max center)
            val
