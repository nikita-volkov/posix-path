module Coalmine.UserErrPrelude
  ( UserErr.UserErr (..),
    UserErr.ToUserErr (..),
    throwUserErr,
    rethrowUserErrAddingContext,
    rethrowUserErrAddingContexts,
  )
where

import Coalmine.InternalPrelude
import Coalmine.UserErr (UserErr)
import Coalmine.UserErr qualified as UserErr

throwUserErr :: (MonadError UserErr m) => Text -> Text -> [Text] -> m a
throwUserErr reason suggestion contexts =
  throwError $ UserErr.UserErr reason suggestion contexts

rethrowUserErrAddingContext :: (MonadError UserErr m) => Text -> m a -> m a
rethrowUserErrAddingContext =
  UserErr.addContextInMonadError

rethrowUserErrAddingContexts :: (MonadError UserErr m) => [Text] -> m a -> m a
rethrowUserErrAddingContexts =
  UserErr.addContextsInMonadError
