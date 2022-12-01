module Coalmine.PtrKitQuickCheck.Properties where

import Coalmine.InternalPrelude hiding (Reader, Writer)
import Coalmine.PtrKit.Reader (Reader)
import Coalmine.PtrKit.Reader qualified as Reader
import Coalmine.PtrKit.Writer (Writer)
import Coalmine.PtrKit.Writer qualified as Writer
import Test.QuickCheck

writeReadRoundtrip :: (Show a, Eq a) => (a -> Writer) -> Reader a -> a -> Property
writeReadRoundtrip writer reader value =
  (===) (Right value) $
    unsafePerformIO $ Reader.readTotalByteString reader $ Writer.toByteString $ writer value
