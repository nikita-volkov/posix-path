-- |
-- Type wrapper determining the serialization.
--
-- Useful for definition of storage format.
--
-- For safety it is advised to import only this module in domain model definition modules.
-- Thus ensuring that only optimised serialisable primitives are used.
module Coalmine.CerealExtras.Compact where

import qualified Coalmine.CerealExtras.Get as CerealExtrasGet
import qualified Coalmine.CerealExtras.Put as CerealExtrasPut
import Coalmine.InternalPrelude hiding (get, put)
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize (..))
import qualified Data.Serialize as Cereal
import qualified Data.Serialize.LEB128 as Leb128
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as BVec
import qualified Data.Vector.Unboxed as UVec

newtype Compact a = Compact {unwrap :: a}
  deriving (Show, Eq, Ord, Integral, Num, Real, Enum, FromJSON, ToJSON, ToJSONKey, Arbitrary)

instance Serialize (Compact Int) where
  put (Compact a) = Leb128.putSLEB128 a
  get = Leb128.getSLEB128 @Int & coerce

instance Serialize (Compact Word) where
  put (Compact a) = Leb128.putLEB128 a
  get = Leb128.getLEB128 @Word & coerce

instance Serialize (Compact Integer) where
  put (Compact a) = Leb128.putSLEB128 a
  get = Leb128.getSLEB128 @Integer & coerce

instance Serialize (Compact Natural) where
  put (Compact a) = Leb128.putLEB128 a
  get = Leb128.getLEB128 @Natural & coerce

instance Serialize (Compact ByteString) where
  put (Compact a) = do
    Leb128.putLEB128 @Word64 $ fromIntegral $ ByteString.length a
    Cereal.putByteString a
  get = do
    length <- Leb128.getLEB128 @Word64
    byteString <- Cereal.getBytes $ fromIntegral length
    return $ Compact byteString

instance Serialize (Compact Text) where
  put = put . TextEncoding.encodeUtf8 . unwrap
  get = do
    byteString <- get
    case TextEncoding.decodeUtf8' byteString of
      Right res -> return $ Compact res
      Left exc -> CerealExtrasGet.failWithException exc

instance (Serialize k, Serialize v, Ord k) => Serialize (Compact (Map k v)) where
  put (Compact map) = CerealExtrasPut.ordMap put put map
  get = CerealExtrasGet.ordMap get get <&> Compact

instance Serialize a => Serialize (Compact (BVec.Vector a)) where
  put (Compact vec) = CerealExtrasPut.vec put vec
  get = CerealExtrasGet.vec get <&> Compact

instance (Serialize a, UVec.Unbox a) => Serialize (Compact (UVec.Vector a)) where
  put (Compact vec) = CerealExtrasPut.vec put vec
  get = CerealExtrasGet.vec get <&> Compact

instance (Serialize a) => Serialize (Compact [a]) where
  put (Compact list) = do
    put $ Compact $ length list
    forM_ list put
  get = do
    Compact length <- get
    Compact <$> replicateM length get
