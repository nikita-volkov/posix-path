module Coalmine.BaseExtras.IO where

import Coalmine.InternalPrelude
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.Yaml as Yaml

-- |
-- Assume that the result of executing an explicitly failing action is right.
-- Otherwise fail forming a message using the provided label.
runStage :: Show e => String -> IO (Either e a) -> IO a
runStage stage io =
  io >>= evalRight stage

evalRight :: Show e => String -> Either e a -> IO a
evalRight stage =
  either (fail . showErr) return
  where
    showErr details = stage <> ": " <> show details

printYaml :: ToJSON a => a -> IO ()
printYaml = ByteStringChar8.putStrLn . Yaml.encode
