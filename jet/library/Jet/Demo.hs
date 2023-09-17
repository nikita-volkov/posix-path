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
  = ViewReplacedEvent View
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
          [ contrafilter
              (\case ViewReplacedEvent view -> Just view; _ -> Nothing)
              uiDrawerSink,
            contrafilter
              (\case ActionChosenEvent action -> Just action; _ -> Nothing)
              workerSink
          ]
  dispatch source sink machine

startWorker :: IO (Sink Action, Source WorkerEvent)
startWorker = do
  error "TODO"

startUserActions :: IO (Source UserEvent)
startUserActions =
  error "TODO"

startUiDrawer :: IO (Sink View)
startUiDrawer =
  error "TODO"
