module Coalmine.Printer
  ( -- * Printer
    Printer,

    -- ** Management
    start,
    stop,

    -- ** Operations
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

newtype Printer = Printer
  { taskQueue :: TQueue Task
  }

start :: Writer -> IO Printer
start writer = do
  taskQueue <- newTQueueIO
  forkIO $
    let go = do
          tasks <- atomically $ flushTQueue taskQueue
          playResult <- playTasks tasks
          writer.flush
          case playResult of
            False -> return ()
            True -> go
     in go
  return $ Printer taskQueue
  where
    playTasks = \case
      task : remainder -> case task of
        StopTask -> return False
        WriteTask message -> do
          writer.write message
          playTasks remainder
      [] -> return True

startHandle :: Handle -> IO Printer
startHandle handle = startHandleWriter handle >>= start

stop :: Printer -> IO ()
stop logger =
  atomically $ writeTQueue logger.taskQueue StopTask

-- * Ops

write :: Printer -> Text -> IO ()
write logger message =
  atomically $ writeTQueue logger.taskQueue (WriteTask message)

-- * Task

data Task
  = WriteTask Text
  | StopTask

-- * Writer

data Writer = Writer
  { write :: Text -> IO (),
    flush :: IO ()
  }

startHandleWriter :: Handle -> IO Writer
startHandleWriter handle = do
  hSetBuffering handle (BlockBuffering Nothing)
  return $
    Writer
      { write = TextIO.hPutStrLn handle,
        flush = hFlush handle
      }
