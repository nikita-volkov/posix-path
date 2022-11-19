module Coalmine.Pakk.Encoding where

import Coalmine.InternalPrelude

data Encoding = Encoding
  { size :: Int,
    write :: Write
  }

instance Semigroup Encoding where
  Encoding leftSize leftWrite <> Encoding rightSize rightWrite =
    Encoding (leftSize + rightSize) (leftWrite <> rightWrite)

instance Monoid Encoding where
  mempty = Encoding 0 mempty

newtype Write = Write {run :: Ptr Word8 -> Int -> IO WriteIteration}

instance Semigroup Write where
  left <> right =
    Write $ \ptr cap ->
      left.run ptr cap >>= \case
        FinishedWriteIteration ptr cap ->
          right.run ptr cap
        ExhaustedWriteIteration nextLeftWrite ->
          return $ ExhaustedWriteIteration $ nextLeftWrite <> right

instance Monoid Write where
  mempty = Write $ \ptr cap ->
    pure $ FinishedWriteIteration ptr cap

data WriteIteration
  = FinishedWriteIteration
      (Ptr Word8)
      -- ^ Pointer after the written data.
      Int
      -- ^ Capacity of that pointer.
  | -- | Need a pointer to continue writing to.
    ExhaustedWriteIteration
      Write
      -- ^ Next encoding to execute.
