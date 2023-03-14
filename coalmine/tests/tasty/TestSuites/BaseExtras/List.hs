module TestSuites.BaseExtras.List where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.Prelude
import Coalmine.Tasty
import Data.Set qualified as Set

tests :: [TestTree]
tests =
  [ testGroup "streamUniqueDuplicates" $
      [ testProperty "produces unique elements" $ \(list :: [Word]) ->
          let result = List.streamUniqueDuplicates list
           in nub result === result,
        testProperty "original - nubbed == duplicates" $ \(list :: [Word]) ->
          Set.fromList (list \\ nub list)
            == Set.fromList (List.streamUniqueDuplicates list)
      ]
  ]
