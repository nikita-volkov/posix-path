-- | Report about an error.
module Coalmine.UserErr where

import Coalmine.BaseExtras.List qualified as ListExtras
import Coalmine.Inter
import Coalmine.InternalPrelude hiding (init)
import Coalmine.Printing
import Data.Text qualified as Text

-- |
-- Contextual error report intended for the app user.
--
-- Allows to easily nest the error in contexts.
data UserErr = UserErr
  { reason :: Text,
    suggestion :: Text,
    -- | Sort of a path of the error.
    --
    -- Serves as the unique identifier of the error type.
    contexts :: [Text],
    -- | Structured view on the input if there is any.
    input :: Maybe Json
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable UserErr

instance BroadPrinting UserErr where
  toBroadBuilder e =
    ListExtras.intercalate "\n\n"
      . catMaybes
      $ [ if Text.null e.reason
            then Nothing
            else Just $ to e.reason,
          if null e.contexts
            then Nothing
            else
              let compiledContext =
                    ListExtras.mapIntercalate
                      toBroadBuilder
                      "/"
                      e.contexts
               in Just
                    [j|
                    Context:
                      ${compiledContext}
                  |],
          if Text.null e.suggestion
            then Nothing
            else
              Just
                [j|
                Suggestion:
                  ${e.suggestion}
              |]
        ]

-- * Rendering

renderAsPlainText :: UserErr -> Text
renderAsPlainText =
  from . toBroadBuilder

-- * Construction

init ::
  -- | Reason.
  Text ->
  -- | Suggestion
  Text ->
  UserErr
init reason suggestion =
  UserErr reason suggestion [] Nothing

-- * Mapping

addContext :: Text -> UserErr -> UserErr
addContext context appException =
  appException {contexts = context : appException.contexts}

addContextInEither :: Text -> Either UserErr a -> Either UserErr a
addContextInEither context =
  first $ addContext context

addContextInMonadError :: (MonadError UserErr m) => Text -> m a -> m a
addContextInMonadError context =
  handleError $ throwError . addContext context

addContexts :: [Text] -> UserErr -> UserErr
addContexts contexts appException =
  appException {contexts = contexts <> appException.contexts}

addContextsInMonadError :: (MonadError UserErr m) => [Text] -> m a -> m a
addContextsInMonadError contexts =
  handleError $ throwError . addContexts contexts

setInput :: Json -> UserErr -> UserErr
setInput newInput UserErr {..} =
  UserErr
    { input = Just newInput,
      ..
    }

setInputInMonadError :: (MonadError UserErr m) => Json -> m a -> m a
setInputInMonadError input =
  handleError $ throwError . setInput input

-- * Ops

throwInMonadError ::
  (MonadError UserErr m) =>
  -- | Reason.
  Text ->
  -- | Suggestion.
  Text ->
  m a
throwInMonadError reason suggestion =
  throwError $ init reason suggestion

nestInMonadError :: (MonadError UserErr m) => Text -> Either UserErr a -> m a
nestInMonadError context = \case
  Right a -> return a
  Left err -> throwError $ addContext context err

-- * Conversion class

class ToUserErr a where
  toUserErr :: a -> UserErr
