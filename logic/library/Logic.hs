module Logic where

import Coalmine.Prelude

newtype Logic ops res
  = Logic (forall m. (Monad m) => ops m -> m res)

instance Functor (Logic ops) where
  fmap f (Logic run) =
    Logic (liftM f . run)

instance Applicative (Logic ops) where
  pure a =
    Logic (\_ -> pure a)
  Logic runL <*> Logic runR =
    Logic (\ops -> runL ops <*> runR ops)

instance Monad (Logic ops) where
  return = pure
  Logic runL >>= k =
    Logic
      ( \ops -> do
          param <- runL ops
          case k param of
            Logic runR -> runR ops
      )

run :: (Monad m) => Logic ops res -> ops m -> m res
run (Logic run) = run

runOnIdentity :: Logic ops res -> ops Identity -> res
runOnIdentity (Logic run) = runIdentity . run

liftOp1 :: (forall m. ops m -> a -> m res) -> a -> Logic ops res
liftOp1 lifter a =
  Logic \ops -> lifter ops a

liftOp2 :: (forall m. ops m -> a -> b -> m res) -> a -> b -> Logic ops res
liftOp2 lifter a b =
  Logic \ops -> lifter ops a b

liftOp3 :: (forall m. ops m -> a -> b -> c -> m res) -> a -> b -> c -> Logic ops res
liftOp3 lifter a b c =
  Logic \ops -> lifter ops a b c
