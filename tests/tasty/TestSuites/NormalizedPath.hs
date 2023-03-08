module TestSuites.NormalizedPath where

import Coalmine.NormalizedPath
import Coalmine.Prelude hiding (Path)
import Coalmine.SyntaxModellingLaws qualified as SyntaxModellingLaws
import Coalmine.Tasty
import Coalmine.Tasty.TestTrees.Cereal

tests =
  [ testEncodeDecode @Path Proxy,
    testProperties "Syntax" $ SyntaxModellingLaws.properties $ Proxy @Path
  ]
