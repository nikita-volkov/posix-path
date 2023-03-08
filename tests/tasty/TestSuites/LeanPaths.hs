module TestSuites.LeanPaths where

import Coalmine.LeanPaths.NormalizedPath
import Coalmine.Prelude hiding (Path)
import Coalmine.SyntaxModellingLaws qualified as SyntaxModellingLaws
import Coalmine.Tasty
import Coalmine.Tasty.TestTrees.Cereal

tests =
  [ testEncodeDecode @NormalizedPath Proxy,
    testProperties "Syntax" $ SyntaxModellingLaws.properties $ Proxy @NormalizedPath
  ]
