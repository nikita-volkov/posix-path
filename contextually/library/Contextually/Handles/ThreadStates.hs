module Contextually.Handles.ThreadStates where

import StmContainers.Map qualified as Map
import Prelude hiding (Handle)

newtype Handle = Handle
  { threadStates :: Map.Map ThreadId [Text]
  }

acquire :: IO Handle
acquire = error "TODO"

enterContext :: Handle -> ThreadId -> Text -> IO ()
enterContext = error "TODO"

exitContext :: Handle -> ThreadId -> IO ()
exitContext handle =
  error "TODO"
