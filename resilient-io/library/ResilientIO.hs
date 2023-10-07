module ResilientIO where

import Prelude

-- |
-- Execute resilient IO handling all notifications
-- with the provided action.
--
-- All unprocessed exceptions will get propagated.
resiliently ::
  ResilientIO n a ->
  -- | Handler of notifications.
  (n -> IO ()) ->
  IO a
resiliently =
  error "TODO"

newtype ResilientIO n a = ResilientIO
  { io :: (n -> IO ()) -> IO a
  }
  deriving (Functor)
  deriving (Applicative, Monad, Alternative, MonadPlus, MonadIO) via (ReaderT (n -> IO ()) IO)

notify :: n -> ResilientIO n ()
notify notification =
  ResilientIO (\emit -> emit notification)
