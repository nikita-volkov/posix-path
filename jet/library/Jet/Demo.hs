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

-- | Integrate reactors into a process.
run :: StateMachine Cmd Event -> IO ()
run machine = do
  (workerSink, workerSource) <- startWorker
  userActionsSource <- startUserActions
  uiDrawerSink <- startUiDrawer
  let source =
        asum
          [ ProcessWorkerEventCmd <$> workerSource,
            ProcessUserEventCmd <$> userActionsSource
          ]
      sink =
        mconcat
          [ (error "TODO: contrafilter") uiDrawerSink,
            (error "TODO: contrafilter") workerSink
          ]
  dispatch source sink machine

startWorker :: IO (Sink Action, Source WorkerEvent)
startWorker =
  error "TODO"

startUserActions :: IO (Source UserEvent)
startUserActions =
  error "TODO"

startUiDrawer :: IO (Sink View)
startUiDrawer =
  error "TODO"
