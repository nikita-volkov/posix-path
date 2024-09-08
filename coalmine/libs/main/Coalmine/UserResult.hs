module Coalmine.UserResult
  ( UserResult,
    run,
    inContext,
  )
where

import Coalmine.InternalPrelude
import Coalmine.UserErr (UserErr (..))
import Coalmine.UserErr qualified as UserErr

-- |
-- Consider it a 'Maybe' with extended information about the error.
--
-- Use 'pure' to lift a valid value and 'throwError' to fail.
newtype UserResult a
  = UserResult (Either UserErr a)
  deriving (Show, Functor, Applicative, Monad, MonadError UserErr)

-- | Eliminate this layer of abstraction.
run :: UserResult a -> Either UserErr a
run (UserResult run) = run

mapDef :: (Either UserErr a -> Either UserErr b) -> UserResult a -> UserResult b
mapDef mapper (UserResult either) =
  UserResult (mapper either)

inContext :: Text -> UserResult a -> UserResult a
inContext context =
  mapDef $ first (UserErr.addContext context)
