module Logic.Transformer where

import Coalmine.Prelude

newtype LogicT ops m res
  = LogicT (ops m -> m res)

instance (Functor m) => Functor (LogicT ops m) where
  fmap f (LogicT run) =
    LogicT (fmap f . run)

instance (Applicative m) => Applicative (LogicT ops m) where
  pure a =
    LogicT (\_ -> pure a)
  LogicT runL <*> LogicT runR =
    LogicT (\ops -> runL ops <*> runR ops)

instance (Monad m) => Monad (LogicT ops m) where
  return = pure
  LogicT runL >>= k =
    LogicT
      ( \ops -> do
          param <- runL ops
          case k param of
            LogicT runR -> runR ops
      )

instance MonadTrans (LogicT ops) where
  lift = LogicT . const

run :: LogicT ops m res -> ops m -> m res
run (LogicT run) = run

runOnIdentity :: LogicT ops Identity res -> ops Identity -> res
runOnIdentity (LogicT run) = runIdentity . run

liftOp1 :: (ops m -> a -> m res) -> a -> LogicT ops m res
liftOp1 lifter a =
  LogicT \ops -> lifter ops a

liftOp2 :: (ops m -> a -> b -> m res) -> a -> b -> LogicT ops m res
liftOp2 lifter a b =
  LogicT \ops -> lifter ops a b

liftOp3 :: (ops m -> a -> b -> c -> m res) -> a -> b -> c -> LogicT ops m res
liftOp3 lifter a b c =
  LogicT \ops -> lifter ops a b c
