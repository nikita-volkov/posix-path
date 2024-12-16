module Reactor.Model where

import Control.Concurrent.Async
import Prelude

class IsReactorModel model where
  data Cfg model
  data Event model
  data Cmd model
  configure :: Cfg model -> model
  step :: Event model -> model -> model
  emit :: Event model -> model -> [Cmd model]

statefulProcess ::
  (IsReactorModel model) =>
  -- | Initial model configuration.
  Cfg model ->
  -- | Generator of events.
  --
  -- Empty list means exit.
  IO [Event model] ->
  -- | Interpreter of commands.
  (Cmd model -> IO ()) ->
  -- | A process which runs until an empty list is produced by the generator.
  IO ()
statefulProcess cfg recv send =
  go (configure cfg)
  where
    go !model = do
      recv >>= \case
        [] -> pure ()
        events -> do
          let (revCommands, newModel) =
                foldl'
                  ( \(!commands, !model) event ->
                      ( foldl' (flip (:)) commands (emit event model),
                        step event model
                      )
                  )
                  ([], model)
                  events
          mapConcurrently_ send revCommands
          go newModel
