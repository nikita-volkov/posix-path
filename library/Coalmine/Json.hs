module Coalmine.Json where

import Coalmine.InternalPrelude

-- |
-- JSON literal.
--
-- A more correct way to represent it than the one in the \"aeson\" lib.
data Json
  = ObjectJson !(BVec (Text, Json))
  | ArrayJson !(BVec Json)
  | StringJson !Text
  | NumberJson !Scientific
  | TrueJson
  | FalseJson
  | NullJson
