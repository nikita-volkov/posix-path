module Logic where

import Coalmine.Prelude

newtype Logic ctx res
  = Logic (forall m. (Monad m) => ctx m -> m res)

instance Functor (Logic ctx) where
  fmap f (Logic run) =
    Logic (liftM f . run)

instance Applicative (Logic ctx) where
  pure a =
    Logic (\_ -> pure a)
  Logic runL <*> Logic runR =
    Logic (\ctx -> runL ctx <*> runR ctx)

instance Monad (Logic ctx) where
  return = pure
  Logic runL >>= k =
    Logic
      ( \ctx -> do
          param <- runL ctx
          case k param of
            Logic runR -> runR ctx
      )

run :: (Monad m) => Logic ctx res -> ctx m -> m res
run (Logic run) = run

runOnIdentity :: Logic ctx res -> ctx Identity -> res
runOnIdentity (Logic run) = runIdentity . run
