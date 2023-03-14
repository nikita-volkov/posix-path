module Coalmine.Printer
  ( -- * Printer
    Printer,

    -- ** Management
    startWithWriter,
    startWithHandle,
    stop,

    -- ** Operations
    write,
  )
where

import Coalmine.InternalPrelude hiding (Writer)
import Data.Text.IO qualified as TextIO

-- | Asynchronous printer.
newtype Printer = Printer
  { taskQueue :: TQueue Task
  }

startWithWriter :: (Text -> IO ()) -> IO Printer
startWithWriter write = do
  taskQueue <- newTQueueIO
  forkIO $
    let go = do
          tasks <- atomically $ flushTQueue taskQueue
          case processTasks mempty tasks of
            (output, continue) -> do
              write (to output)
              if continue then go else return ()
     in go
  return $ Printer taskQueue
  where
    processTasks :: TextBuilder -> [Task] -> (TextBuilder, Bool)
    processTasks !output = \case
      task : remainder -> case task of
        StopTask -> (output, False)
        WriteTask taskText -> processTasks (output <> to taskText) remainder
      [] -> (output, True)

startWithHandle :: Handle -> IO Printer
startWithHandle handle = do
  hSetBuffering handle NoBuffering
  startWithWriter (TextIO.hPutStr handle)

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
