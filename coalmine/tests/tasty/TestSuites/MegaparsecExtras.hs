module TestSuites.MegaparsecExtras where

import Coalmine.Inter
import Coalmine.Located qualified as Located
import Coalmine.MegaparsecExtras
import Coalmine.Prelude
import Coalmine.Tasty
import Text.Megaparsec.Char

tests :: [TestTree]
tests =
  [ testCase "locate" $
      let input =
            [i|
              a bcd e
            |]
          parser =
            char 'a' *> space *> locate (string "bcd") <* space <* char 'e'
       in case toTextRefiner parser input of
            Right res -> assertEqual "" (Located.Located 2 5 "bcd") res
            Left err -> assertFailure $ to err
  ]
