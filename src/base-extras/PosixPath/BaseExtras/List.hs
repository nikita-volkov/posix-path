module PosixPath.BaseExtras.List where

import Prelude

dropPedantically :: Int -> [a] -> Either Int [a]
dropPedantically amount =
  if amount > 0
    then \case
      _head : tail -> dropPedantically (pred amount) tail
      _ -> Left amount
    else
      if amount == 0
        then Right
        else const (Left amount)

traverseHeadWithDefault :: (Functor f) => a -> (a -> f a) -> [a] -> f [a]
traverseHeadWithDefault def f = \case
  head : tail -> (: tail) <$> f head
  _ -> pure <$> f def

headOr :: a -> [a] -> a
headOr def = \case
  head : _ -> head
  _ -> def
