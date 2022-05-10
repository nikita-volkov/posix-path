module Coalmine.Tasty.HeadedMegaparsec where

import qualified Coalmine.HeadedMegaparsecExtras as P
import Coalmine.InternalPrelude hiding (FilePath)
import Coalmine.Printing
import Coalmine.SimplePaths (FilePath)
import HeadedMegaparsec
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Text.Megaparsec as Megaparsec

testParserToSucceed ::
  (Eq a, Show a, Megaparsec.VisualStream i, Megaparsec.TraversableStream i, Megaparsec.ShowErrorComponent e) =>
  TestName ->
  a ->
  HeadedParsec e i a ->
  i ->
  TestTree
testParserToSucceed name expectation parser input =
  testCase name $ case P.toRefiner parser input of
    Right res -> assertEqual "" expectation res
    Left err -> assertFailure . to $ err

testParserToFail ::
  (Eq a, Show a, Megaparsec.VisualStream i, Megaparsec.TraversableStream i, Megaparsec.ShowErrorComponent e) =>
  TestName ->
  Text ->
  HeadedParsec e i a ->
  i ->
  TestTree
testParserToFail name expectation parser input =
  testCase name $ case P.toRefiner parser input of
    Right res -> assertFailure . show $ res
    Left err -> assertEqual "" expectation err

testFileParserToSucceed ::
  (Eq a, Show a) =>
  TestName ->
  FilePath ->
  HeadedParsec Void Text a ->
  a ->
  TestTree
testFileParserToSucceed name path parser expectation =
  testCase name $
    P.toFileReader parser path >>= \case
      Right res -> assertEqual "" expectation res
      Left err -> assertFailure . to $ err
