module Coalmine.Collector where

import Coalmine.Prelude hiding (init)
import Control.Foldl (Fold (..))

-- *

-- |
-- A data-structure that knows how to consume an input
-- and be finalized into some other representation.
--
-- Can also be thought of as a unification over append-only builders.
--
-- An accumulator of a fold.
--
-- A two-action state machine that can either update itself
-- or exit with a result.
class Collector c where
  type Input c
  type Output c
  feed :: Input c -> c -> c
  burn :: c -> Output c

-- *

collect :: (Collector c, Foldable f) => c -> f (Input c) -> Output c
collect collector =
  burn . foldl' (flip feed) collector

-- *

feedMany :: (Collector c, Foldable f) => f (Input c) -> c -> c
feedMany foldable collector =
  foldl' (flip feed) collector foldable

-- *

toFold :: Collector c => c -> Fold (Input c) (Output c)
toFold init =
  Fold (flip feed) init burn
