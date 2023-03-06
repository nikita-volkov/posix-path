module Coalmine.EvenSimplerPathsClasses where

import Coalmine.EvenSimplerPaths
import Coalmine.InternalPrelude
import Test.QuickCheck qualified as QuickCheck

class GeneralizesToPath a where
  generalize :: a -> Path
  specialize :: Path -> Maybe a

generalizesToPathLawProperties :: (QuickCheck.Arbitrary a, GeneralizesToPath a, Eq a, Show a) => Proxy a -> [(Text, QuickCheck.Property)]
generalizesToPathLawProperties proxy =
  [ ( "Partial isomorphism",
      QuickCheck.property $ \a ->
        Just (asProxyTypeOf a proxy)
          QuickCheck.=== specialize (generalize a)
    )
  ]
