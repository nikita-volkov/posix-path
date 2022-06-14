module Coalmine.EvenSimplerPaths.QuickCheckGens where

import qualified Coalmine.EvenSimplerPaths.Charsets as Charsets
import Coalmine.InternalPrelude hiding (filter)
import Coalmine.QuickCheckExtras.Gens
import qualified StructureKit.Charset as Charset
import Test.QuickCheck

fileName :: Gen Text
fileName = do
  size <- chooseInt (0, 100)
  fromString <$> vectorOf size char
  where
    char = filter pred arbitrary
      where
        pred = not . Charset.toCharPredicate Charsets.notFileName

extension :: Gen Text
extension = do
  size <- chooseInt (1, 10)
  fromString <$> vectorOf size char
  where
    char = filter pred arbitrary
      where
        pred = not . Charset.toCharPredicate Charsets.notFileName

extensions :: Gen [Text]
extensions = do
  amount <- chooseInt (0, 10)
  vectorOf amount extension
