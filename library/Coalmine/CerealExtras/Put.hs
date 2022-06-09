module Coalmine.CerealExtras.Put where

import Coalmine.InternalPrelude hiding (get, map, put)
import qualified Data.ByteString as ByteString
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Serialize.LEB128 (putLEB128)
import Data.Serialize.Put
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as BVec
import qualified Data.Vector.Generic as GVec

-- * Helpers

size :: Int -> Put
size = putLEB128 @Word64 . fromIntegral

sized ::
  (a -> Int) ->
  (a -> Put) ->
  (a -> Put)
sized toSize toPut a =
  size (toSize a) <> toPut a

-- |
-- General sequence with compact encoding of the size metadata.
sizedSequence ::
  -- | Implementation of 'foldMap'.
  (forall x. Monoid x => (a -> x) -> seq -> x) ->
  -- | Size accessor.
  (seq -> Int) ->
  -- | Element putter.
  (a -> Put) ->
  seq ->
  Put
sizedSequence foldMap measureSize putElement =
  sized measureSize (foldMap putElement)

-- * Specifics

vec :: GVec.Vector v a => (a -> Put) -> v a -> Put
vec =
  sizedSequence GVec.foldMap GVec.length

map ::
  (map -> Int) ->
  ((k -> v -> Put) -> map -> Put) ->
  (k -> Put) ->
  (v -> Put) ->
  map ->
  Put
map getSize foldMapWithKey putKey putVal map =
  size (getSize map) <> foldMapWithKey (\k v -> putKey k <> putVal v) map

ordMap :: (k -> Put) -> (v -> Put) -> Map k v -> Put
ordMap = map Map.size Map.foldMapWithKey

intMap :: (Int -> Put) -> (v -> Put) -> IntMap v -> Put
intMap = map IntMap.size IntMap.foldMapWithKey

compactByteString :: ByteString -> Put
compactByteString a = do
  putLEB128 @Word64 $ fromIntegral $ ByteString.length a
  putByteString a

compactText :: Text -> Put
compactText =
  compactByteString . TextEncoding.encodeUtf8
