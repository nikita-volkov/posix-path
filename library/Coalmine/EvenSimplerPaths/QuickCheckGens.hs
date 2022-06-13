module Coalmine.EvenSimplerPaths.QuickCheckGens where

import qualified Coalmine.EvenSimplerPaths.Charsets as Charsets
import Coalmine.InternalPrelude hiding (filter)
import Coalmine.QuickCheckExtras.Gens
import qualified StructureKit.Charset as Charset
import Test.QuickCheck

fileName :: Gen Text
fileName =
  fromString <$> listOf char
  where
    char = filter pred arbitrary
      where
        pred = not . Charset.toCharPredicate Charsets.notFileName

extension :: Gen Text
extension =
  fromString <$> listOf1 char
  where
    char = filter pred arbitrary
      where
        pred = not . Charset.toCharPredicate Charsets.notFileName
