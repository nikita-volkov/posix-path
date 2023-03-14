module Coalmine.SyntaxModellingQuickCheck where

import Coalmine.InternalPrelude
import Coalmine.SyntaxModelling qualified as SyntaxModelling
import Test.QuickCheck

all ::
  (SyntaxModelling.Syntax a, Arbitrary a, Eq a, Show a) =>
  Proxy a ->
  [(String, Property)]
all proxy =
  [ ( "Partial isomorphism",
      property $ partialIsomorphism . flip asProxyTypeOf proxy
    )
  ]

partialIsomorphism ::
  (SyntaxModelling.Syntax a, Eq a, Show a) =>
  a ->
  Property
partialIsomorphism a =
  Right a
    === SyntaxModelling.fromTextInEither (SyntaxModelling.toText a)
