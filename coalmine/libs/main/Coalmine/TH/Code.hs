module Coalmine.TH.Code where

import Coalmine.InternalPrelude hiding (lift)
import Language.Haskell.TH.Syntax

readStatically :: (Read a, Lift a) => String -> Code Q a
readStatically literal = Code $ do
  literal <- case readMaybe literal of
    Just literal -> return literal
    Nothing -> fail $ "Unreadable literal: " <> literal
  examineCode $ liftTyped literal
