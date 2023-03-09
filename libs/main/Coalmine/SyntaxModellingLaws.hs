module Coalmine.SyntaxModellingLaws where

import Coalmine.InternalPrelude
import Coalmine.IsomorphismClasses
import Coalmine.SyntaxModelling qualified as SyntaxModelling
import Data.Attoparsec.Text qualified as Attoparsec
import Test.QuickCheck qualified as QuickCheck

properties ::
  (SyntaxModelling.Syntax a, QuickCheck.Arbitrary a, Eq a, Show a) =>
  Proxy a ->
  [(String, QuickCheck.Property)]
properties proxy =
  [ ( "Partial isomorphism",
      QuickCheck.property $ \a ->
        Right (asProxyTypeOf a proxy)
          QuickCheck.=== SyntaxModelling.fromTextInEither
            (SyntaxModelling.toText a)
    )
  ]
