module TestSuites.Name where

import Coalmine.Name
import Coalmine.Prelude
import Coalmine.Tasty
import Coalmine.Tasty.TestTrees.Cereal

tests :: [TestTree]
tests =
  [ testEncodeDecode @Name Proxy
  ]
