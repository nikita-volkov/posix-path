module Coalmine.Comms.Schema where

import Coalmine.InternalPrelude

-- |
-- Structure of the data.
-- All the metainformation necessary to be able to:
-- - generate code (with names of things),
-- - validate the compatibility on the consumer/receiver ends,
-- - serialize to complete representation,
-- - serialize to compact representation (lacking names).
--
-- A tree of named elements.
data Schema
  = ProductSchema [(Text, Schema)]
  | SumSchema [(Text, Schema)]
  | SeqSchema
      { minLength :: Int,
        maxLength :: Int,
        element :: Schema
      }
  | IntegerSchema
      { minValue :: Integer,
        maxValue :: Integer
      }
