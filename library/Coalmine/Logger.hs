module Coalmine.Logger
  ( -- * Logger
    Logger,

    -- ** Management
    start,
    stop,

    -- ** Operations
    getVerbosity,
    setVerbosity,
    modifyVerbosity,
    write,

    -- * Writer interface
    Writer,
    startHandleWriter,
  )
where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.InternalPrelude hiding (Writer)
import Coalmine.StmExtras.TQueue qualified as TQueue
import Data.Text.IO qualified as TextIO

data Logger = Logger
  { taskQueue :: TQueue Task,
    currentVerbosityVar :: TVar Int
  }

start :: Int -> Writer -> IO Logger
start initialVerbosity writer = do
  taskQueue <- newTQueueIO
  forkIO $
    let go !verbosity = do
          tasks <- atomically $ flushTQueue taskQueue
          playResult <- playTasks verbosity tasks
          writer.flush
          case playResult of
            Nothing -> return ()
            Just newVerbosity -> go newVerbosity
     in go initialVerbosity
  currentVerbosityVar <- newTVarIO initialVerbosity
  return $ Logger taskQueue currentVerbosityVar
  where
    playTasks verbosity = \case
      task : remainder -> case task of
        StopTask ->
          return Nothing
        SetVerbosityTask newVerbosity ->
          playTasks newVerbosity remainder
        WriteTask taskVerbosity message ->
          if taskVerbosity > verbosity
            then playTasks verbosity remainder
            else do
              writer.write message
              playTasks verbosity remainder
      [] -> return (Just verbosity)

startHandle :: Int -> Handle -> IO Logger
startHandle verbosity handle = do
  writer <- startHandleWriter verbosity handle
  start verbosity writer

stop :: Logger -> IO ()
stop logger =
  atomically $ writeTQueue logger.taskQueue StopTask

-- * Ops

getVerbosity :: Logger -> IO Int
getVerbosity logger =
  atomically $ readTVar logger.currentVerbosityVar

setVerbosity :: Logger -> Int -> IO ()
setVerbosity logger verbosity = atomically $ do
  writeTVar logger.currentVerbosityVar verbosity
  writeTQueue logger.taskQueue (SetVerbosityTask verbosity)

-- | Update the verbosity producing the old one.
modifyVerbosity :: Logger -> (Int -> Int) -> IO Int
modifyVerbosity logger modifier = atomically $ do
  verbosity <- readTVar logger.currentVerbosityVar
  writeTVar logger.currentVerbosityVar $! modifier verbosity
  return verbosity

write :: Logger -> Int -> Text -> IO ()
write logger verbosity message =
  atomically $ writeTQueue logger.taskQueue (WriteTask verbosity message)

-- * Task

data Task
  = SetVerbosityTask Int
  | WriteTask Int Text
  | StopTask

taskVerbosity :: Task -> Maybe Int
taskVerbosity = \case
  SetVerbosityTask a -> Just a
  _ -> Nothing

-- * Writer

data Writer = Writer
  { write :: Text -> IO (),
    flush :: IO ()
  }

startHandleWriter :: Int -> Handle -> IO Writer
startHandleWriter verbosity handle = do
  hSetBuffering handle (BlockBuffering Nothing)
  return $
    Writer
      { write = TextIO.hPutStrLn handle,
        flush = hFlush handle
      }
