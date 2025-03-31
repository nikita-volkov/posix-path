module PosixPath.BaseExtras.MonadPlus where

import Control.Monad
import Prelude

reverseMany :: (MonadPlus m) => m a -> m [a]
reverseMany element = go []
  where
    go !acc =
      mplus
        (element >>= go . (: acc))
        (return acc)

reverseSepBy :: (MonadPlus m) => m a -> m b -> m [a]
reverseSepBy element sep =
  mplus
    (reverseSepBy1 element sep)
    (pure [])

reverseSepBy1 :: (MonadPlus m) => m a -> m b -> m [a]
reverseSepBy1 element sep =
  element >>= go . pure
  where
    go !acc =
      mplus
        (sep >> element >>= go . (: acc))
        (return acc)
