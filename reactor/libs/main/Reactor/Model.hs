module Reactor.Model where

import Prelude

class IsReactorModel model where
  data Cfg model
  data Event model
  data Cmd model
  configure :: Cfg model -> model
  update :: Event model -> model -> model
  command :: Event model -> model -> Cmd model

statefulProcess ::
  (IsReactorModel model) =>
  -- | Model configuration.
  Cfg model ->
  -- | Producer of events.
  IO (Maybe (Event model)) ->
  -- | Consumer of commands.
  (Cmd model -> IO ()) ->
  IO ()
statefulProcess cfg recv emit =
  go (configure cfg)
  where
    go !model = do
      recv >>= \case
        Nothing -> pure ()
        Just event -> do
          emit (command event model)
          go (update event model)
