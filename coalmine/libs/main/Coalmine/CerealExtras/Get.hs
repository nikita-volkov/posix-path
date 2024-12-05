module Coalmine.CerealExtras.Get where

import Coalmine.InternalPrelude hiding (get, map, put)
import Data.ByteString qualified as ByteString
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Serialize.Get
import Data.Serialize.LEB128 qualified as Leb128
import Data.Text.Encoding qualified as TextEncoding
import Data.Vector.Generic qualified as GVec

runCompletely :: Get a -> ByteString -> Either Text a
runCompletely get input =
  case runGetPartial get input of
    Done res rem ->
      if ByteString.null rem
        then Right res
        else Left "Too much input"
    Fail err _ -> Left (from err)
    Partial _ -> Left "Not enough input"

intInSLeb128 :: Get Int
intInSLeb128 = Leb128.getSLEB128

word64InLeb128 :: Get Word64
word64InLeb128 = Leb128.getLEB128 @Word64

size :: Get Int
size = word64InLeb128 <&> fromIntegral

list :: Get a -> Get [a]
list element =
  size >>= \size -> replicateM size element

vec :: (GVec.Vector v a) => Get a -> Get (v a)
vec element =
  size >>= \size -> GVec.replicateM size element

-- |
-- Most generic map construction helper.
map :: ([(k, v)] -> map) -> Get k -> Get v -> Get map
map fromDistinctAscList key val =
  list ((,) <$> key <*> val) <&> fromDistinctAscList

ordMap :: Get k -> Get v -> Get (Map k v)
ordMap = map Map.fromDistinctAscList

intMap :: Get Int -> Get v -> Get (IntMap.IntMap v)
intMap = map IntMap.fromDistinctAscList

failWithException :: (Exception e) => e -> Get any
failWithException = fail . displayException

secureCompactVec ::
  (GVec.Vector v a) =>
  -- | Max size.
  Int ->
  Get a ->
  Get (v a)
secureCompactVec maxSize element = do
  sizeVal <- size
  if sizeVal > maxSize
    then fail "Size is too large"
    else GVec.replicateM sizeVal element

secureCompactByteString ::
  -- | Max size.
  Int ->
  Get ByteString
secureCompactByteString maxSize = do
  sizeVal <- size
  if sizeVal > maxSize
    then fail "Size is too large"
    else getBytes sizeVal

secureCompactText ::
  -- | Max UTF8-encoded size.
  Int ->
  Get Text
secureCompactText maxSize = do
  byteString <- secureCompactByteString maxSize
  case TextEncoding.decodeUtf8' byteString of
    Right res -> return res
    Left exc -> failWithException exc
