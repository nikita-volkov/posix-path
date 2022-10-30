module Coalmine.UserErrPrelude
  ( UserErr.UserErr (..),
    UserErr.ToUserErr (..),
    throwUserErr,
    rethrowUserErrInContext,
  )
where

import Coalmine.InternalPrelude
import Coalmine.Name (Name)
import Coalmine.UserErr (UserErr)
import qualified Coalmine.UserErr as UserErr

throwUserErr :: MonadError UserErr m => Text -> Text -> [Name] -> m a
throwUserErr reason suggestion contexts =
  throwError $ UserErr.UserErr reason suggestion contexts

rethrowUserErrInContext :: MonadError UserErr m => Name -> m a -> m a
rethrowUserErrInContext =
  UserErr.addContextInMonadError
