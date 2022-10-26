module Coalmine.UserErrPrelude
  ( UserErr.UserErr (..),
    UserErr.ToUserErr (..),
    throwUserErr,
  )
where

import Coalmine.InternalPrelude
import Coalmine.Name (Name)
import qualified Coalmine.UserErr as UserErr

throwUserErr :: MonadError UserErr.UserErr m => Text -> Text -> [Name] -> m a
throwUserErr reason suggestion contexts =
  throwError $ UserErr.UserErr reason suggestion contexts
