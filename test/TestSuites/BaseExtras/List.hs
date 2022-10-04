module TestSuites.BaseExtras.List where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.Prelude
import Coalmine.Tasty
import qualified Data.Set as Set

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
