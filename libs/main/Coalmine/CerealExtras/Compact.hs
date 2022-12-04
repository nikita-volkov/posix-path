-- |
-- Type wrapper determining the serialization.
--
-- Useful for definition of storage format.
--
-- For safety it is advised to import only this module in domain model definition modules.
-- Thus ensuring that only optimised serialisable primitives are used.
module Coalmine.CerealExtras.Compact where

import Coalmine.CerealExtras.Get qualified as CerealExtrasGet
import Coalmine.CerealExtras.Put qualified as CerealExtrasPut
import Coalmine.InternalPrelude hiding (get, put)
import Data.ByteString qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize (..))
import Data.Serialize qualified as Cereal
import Data.Serialize.LEB128 qualified as Leb128
import Data.Text.Encoding qualified as TextEncoding
import Data.Vector qualified as BVec
import Data.Vector.Unboxed qualified as UVec

unwrap :: Compact a -> a
unwrap = (.value)

newtype Compact a = Compact {value :: a}
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
  put = put . TextEncoding.encodeUtf8 . (.value)
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
