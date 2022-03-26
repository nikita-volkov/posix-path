module TestSuites.ConduitExtras where

import qualified Coalmine.ConduitExtras as ConduitExtras
import Coalmine.Prelude
import Coalmine.Tasty
import Conduit

tests =
  [ testCase "Discretization" $
      let conduit =
            yieldMany input
              .| ConduitExtras.discretize 10 fst snd
              .| sinkList
          input =
            [ (3, 0),
              (13, 3),
              (23, 4),
              (24, 5),
              (54, 6),
              (63, 7)
            ]
          expectation =
            [0, 3, 5, 5, 5, 6]
          reality =
            runConduitPure conduit
       in assertEqual "" expectation reality
  ]
