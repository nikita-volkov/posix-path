module Coalmine.CerealExtras.Instances where

import Cereal.UnorderedContainers.Serialize ()
import Coalmine.InternalPrelude hiding (get, put)
import Data.Scientific qualified as Scientific
import Data.Serialize

instance Serialize Scientific where
  put x = do
    put (Scientific.coefficient x)
    put (Scientific.base10Exponent x)
  get = Scientific.scientific <$> get <*> get

instance Serialize UniversalTime where
  put = put . getModJulianDate
  get = ModJulianDate <$> get

instance Serialize DiffTime where
  put = put . diffTimeToPicoseconds
  get = picosecondsToDiffTime <$> get

instance Serialize UTCTime where
  put UTCTime {..} = put utctDay >> put utctDayTime
  get = UTCTime <$> get <*> get

instance Serialize NominalDiffTime where
  put = put . toRational
  get = fromRational <$> get

instance Serialize Day where
  put = put . toModifiedJulianDay
  get = ModifiedJulianDay <$> get

instance Serialize (Fixed e) where
  put (MkFixed n) = put n
  get = MkFixed <$> get
