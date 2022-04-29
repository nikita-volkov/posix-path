module Coalmine.MtlExtras where

import Coalmine.InternalPrelude

modifyFailing :: (MonadState s m, MonadError e m) => (s -> Either e s) -> m ()
modifyFailing _fn = do
  _state <- get
  case _fn _state of
    Left _err -> throwError _err
    Right _state -> put _state
