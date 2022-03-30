module TestSuites.SimplePaths where

import Coalmine.Prelude
import Coalmine.SimplePaths
import Coalmine.Tasty

tests =
  [ testCase "FilePath parsing" $
      assertEqual "" (Right ".DS_Store") $
        parse (lenientParser @FilePath) ".DS_Store",
    testCase "DirPath parsing" $
      assertEqual "" (Right ".DS_Store") $
        parse (lenientParser @DirPath) ".DS_Store"
  ]
