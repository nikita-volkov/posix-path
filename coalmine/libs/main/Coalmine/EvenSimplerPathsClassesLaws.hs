module Coalmine.EvenSimplerPathsClassesLaws where

import Coalmine.EvenSimplerPathsClasses
import Coalmine.InternalPrelude
import Test.QuickCheck qualified as QuickCheck

generalizesToPathLawProperties :: (QuickCheck.Arbitrary a, GeneralizesToPath a, Eq a, Show a) => Proxy a -> [(String, QuickCheck.Property)]
generalizesToPathLawProperties proxy =
  [ ( "Partial isomorphism",
      QuickCheck.property $ \a ->
        Just (asProxyTypeOf a proxy)
          QuickCheck.=== specialize (generalize a)
    )
  ]
