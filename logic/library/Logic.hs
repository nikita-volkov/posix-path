module Logic where

import Coalmine.Prelude

newtype Logic fx res
  = Logic (forall m. (Monad m) => fx m -> m res)

instance Functor (Logic fx) where
  fmap f (Logic run) =
    Logic (liftM f . run)

instance Applicative (Logic fx) where
  pure a =
    Logic (\_ -> pure a)
  Logic runL <*> Logic runR =
    Logic (\fx -> runL fx <*> runR fx)

instance Monad (Logic fx) where
  return = pure
  Logic runL >>= k =
    Logic
      ( \fx -> do
          param <- runL fx
          case k param of
            Logic runR -> runR fx
      )

run :: (Monad m) => Logic fx res -> fx m -> m res
run (Logic run) = run
