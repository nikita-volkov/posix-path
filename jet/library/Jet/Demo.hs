module Jet.Demo where

import Coalmine.Prelude hiding (listen, tell)
import Jet

data UserEvent

data WorkerEvent
  = ProgressedWorkerEvent
      Rational
      -- ^ By.
      Bool
      -- ^ To finish.
  | FinishedWorkerEvent
  | FailedWorkerEvent

data View
  = MenuView [Text]
  | ProgressView Rational
  | ErrorView Text

-- |
-- Real world action of the application.
data Action
  = RenameModuleAction Text Text

data Cmd
  = ProcessUserEventCmd UserEvent
  | ProcessWorkerEventCmd WorkerEvent

data Event
  = UiEvent View
  | ActionChosenEvent Action

dispatch ::
  Reactor () UserEvent ->
  Reactor Action WorkerEvent ->
  Reactor View () ->
  Reactor Cmd Event ->
  Dispatch ()
dispatch userActions worker drawer logic =
  asum
    [ do
        userEvent <- listen userActions
        tell logic (ProcessUserEventCmd userEvent),
      do
        workerEvent <- listen worker
        tell logic (ProcessWorkerEventCmd workerEvent),
      do
        event <- listen logic
        case event of
          ActionChosenEvent action -> tell worker action
          UiEvent view -> tell drawer view
    ]
