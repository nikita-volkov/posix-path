module Contextually where

import Data.Text qualified as Text
import Prelude

data ContextualizedException = ContextualizedException
  { contexts :: [Text],
    cause :: SomeException
  }
  deriving (Show)

instance Exception ContextualizedException where
  toException = SomeException
  fromException (SomeException exception) = cast exception
  displayException e =
    toList (Text.intercalate "/" e.contexts) <> ": " <> displayException e.cause

contextually :: Text -> IO a -> IO a
contextually context action =
  catch action \someException ->
    case fromException someException of
      Just ContextualizedException {..} -> throwIO (ContextualizedException (context : contexts) cause)
      Nothing -> throwIO (ContextualizedException [context] someException)
