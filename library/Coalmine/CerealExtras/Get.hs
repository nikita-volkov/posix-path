module Coalmine.CerealExtras.Get where

import Coalmine.InternalPrelude hiding (get, map, put)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Serialize.Get
import qualified Data.Serialize.LEB128 as Leb128
import qualified Data.Vector as BVec
import qualified Data.Vector.Generic as GVec

intInSLeb128 :: Get Int
intInSLeb128 = Leb128.getSLEB128

word64InLeb128 :: Get Word64
word64InLeb128 = Leb128.getLEB128 @Word64

size :: Get Int
size = word64InLeb128 <&> fromIntegral

list :: Get a -> Get [a]
list element =
  size >>= \size -> replicateM size element

vec :: GVec.Vector v a => Get a -> Get (v a)
vec element =
  size >>= \size -> GVec.replicateM size element

-- |
-- Most generic map construction helper.
map :: ([(k, v)] -> map) -> Get k -> Get v -> Get map
map fromDistinctAscList key val =
  list ((,) <$> key <*> val) <&> fromDistinctAscList

ordMap :: Ord k => Get k -> Get v -> Get (Map k v)
ordMap = map Map.fromDistinctAscList

intMap :: Get Int -> Get v -> Get (IntMap.IntMap v)
intMap = map IntMap.fromDistinctAscList

failWithException :: Exception e => e -> Get any
failWithException = fail . displayException
