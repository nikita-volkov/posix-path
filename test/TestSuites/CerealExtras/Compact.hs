module TestSuites.CerealExtras.Compact where

import Coalmine.CerealExtras.Compact
import Coalmine.Prelude
import Coalmine.Tasty
import Coalmine.Tasty.TestTrees.Cereal

tests =
  [ testEncodeDecode @(Compact Text) Proxy
  ]
