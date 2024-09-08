module Coalmine.UserErrPrelude
  ( UserErr.UserErr (..),
    throwUserErr,
    rethrowUserErrAddingContext,
    rethrowUserErrAddingContexts,
    rethrowUserErrSettingInput,
  )
where

import Coalmine.InternalPrelude
import Coalmine.UserErr (UserErr)
import Coalmine.UserErr qualified as UserErr

throwUserErr ::
  (MonadError UserErr m) =>
  -- | Reason.
  Text ->
  -- | Suggestion.
  Text ->
  m a
throwUserErr =
  UserErr.throwInMonadError

rethrowUserErrAddingContext :: (MonadError UserErr m) => Text -> m a -> m a
rethrowUserErrAddingContext =
  UserErr.addContextInMonadError

rethrowUserErrAddingContexts :: (MonadError UserErr m) => [Text] -> m a -> m a
rethrowUserErrAddingContexts =
  UserErr.addContextsInMonadError

rethrowUserErrSettingInput :: (MonadError UserErr m) => Json -> m a -> m a
rethrowUserErrSettingInput =
  UserErr.setInputInMonadError
