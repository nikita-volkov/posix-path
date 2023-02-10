-- |
-- Monadic definition extensional DSL for tasty.
-- The value is purely stylistic:
-- this DSL lets you avoid the management of commas in lists.
--
-- You can import this module into the same namespace as other Tasty
-- modules or unqualified. It is designed not to cause conflicts.
module Coalmine.TastyMonadic
  ( declareTestGroupDefaultMain,
    declareTestGroupTestTree,
    DeclareTestGroup,
    declareTestTree,
    declareTestGroup,
    declareTestCase,
    declareTestProperty,
  )
where

import Coalmine.InternalPrelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

declareTestGroupDefaultMain :: String -> DeclareTestGroup () -> IO ()
declareTestGroupDefaultMain name declare =
  defaultMain $ declareTestGroupTestTree name declare

declareTestGroupTestTree :: String -> DeclareTestGroup () -> TestTree
declareTestGroupTestTree name (DeclareTestGroup run) =
  testGroup name . reverse . snd . run $ []

newtype DeclareTestGroup a
  = DeclareTestGroup ([TestTree] -> (a, [TestTree]))
  deriving
    (Functor, Applicative, Monad)
    via (State [TestTree])

declareTestTree :: TestTree -> DeclareTestGroup ()
declareTestTree tree =
  DeclareTestGroup $ \testList ->
    ((), tree : testList)

declareTestGroup :: String -> DeclareTestGroup () -> DeclareTestGroup ()
declareTestGroup name declare =
  declareTestTree $ declareTestGroupTestTree name declare

declareTestCase :: String -> IO () -> DeclareTestGroup ()
declareTestCase name body =
  declareTestTree $ testCase name body

declareTestProperty :: Testable prop => String -> prop -> DeclareTestGroup ()
declareTestProperty name body =
  declareTestTree $ testProperty name body
