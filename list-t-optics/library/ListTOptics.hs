{-# OPTIONS_GHC -Wno-orphans #-}
module ListTOptics where

import ListT
import Optics
import Prelude hiding (tail)
import Control.Monad

instance (Zoom m n a b) => Zoom (ListT m) (ListT n) a b where
  zoom optic (ListT eliminate) =
    ListT $
      zoom optic eliminate >>= \case
        Nothing -> return Nothing
        Just (head, tail) -> return (Just (head, zoom optic tail))
  zoomMaybe optic (ListT eliminate) =
    ListT $
      zoomMaybe optic eliminate >>= \case
        Nothing -> return Nothing
        Just (Just (head, tail)) -> return (Just (Just head, zoomMaybe optic tail))
        Just Nothing -> return (Just (Nothing, mzero))
  zoomMany optic (ListT eliminate) =
    ListT $
        zoomMany optic eliminate >>= \case
          Nothing -> return Nothing
          Just (head, tail) -> return (Just (head, zoomMany optic tail))
