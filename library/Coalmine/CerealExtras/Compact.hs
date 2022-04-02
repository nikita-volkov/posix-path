-- |
-- Type wrapper determining the serialization.
--
-- Useful for definition of storage format.
--
-- For safety it is advised to import only this module in domain model definition modules.
-- Thus ensuring that only optimised serialisable primitives are used.
module Coalmine.CerealExtras.Compact where

import qualified Coalmine.CerealExtras.Get as Get
import qualified Coalmine.CerealExtras.Put as Put
import Coalmine.Prelude hiding (get, put)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize (..))
import qualified Data.Serialize as Cereal
import qualified Data.Serialize.LEB128 as Leb128
import qualified Data.Vector as BVec
import qualified Data.Vector.Unboxed as UVec

newtype Compact a = Compact a
  deriving (Show, Eq, Ord, Integral, Num, Real, Enum, FromJSON, ToJSON, ToJSONKey)

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

instance Serialize (Compact Text) where
  put (Compact text) = error "TODO"
  get = error "TODO"

instance (Serialize k, Serialize v, Ord k) => Serialize (Compact (Map k v)) where
  put (Compact map) = Put.ordMap put put map
  get = Get.ordMap get get <&> Compact

instance Serialize a => Serialize (Compact (BVec.Vector a)) where
  put (Compact vec) = Put.vec put vec
  get = Get.vec get <&> Compact

instance (Serialize a, UVec.Unbox a) => Serialize (Compact (UVec.Vector a)) where
  put (Compact vec) = Put.vec put vec
  get = Get.vec get <&> Compact
