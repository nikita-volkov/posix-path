module Coalmine.ContainersExtras.Seq where

import Coalmine.InternalPrelude hiding (empty, insert)
import Data.Sequence

mapHead :: (a -> a) -> Seq a -> Seq a
mapHead f seq = case viewl seq of
  EmptyL -> mempty
  head :< tail -> f head <| tail

mapTailTip :: (a -> a) -> Seq a -> Seq a
mapTailTip f seq = case viewr seq of
  EmptyR -> mempty
  init :> tailTip -> init |> f tailTip
