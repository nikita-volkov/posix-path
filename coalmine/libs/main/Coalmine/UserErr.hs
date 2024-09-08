-- | Report about an error.
module Coalmine.UserErr where

import Coalmine.BaseExtras.List qualified as ListExtras
import Coalmine.Inter
import Coalmine.InternalPrelude
import Coalmine.Printing
import Data.Text qualified as Text

-- |
-- Contextual error report intended for the app user.
--
-- Allows to easily nest the error in contexts.
data UserErr = UserErr
  { reason :: Text,
    suggestion :: Text,
    contexts :: [Text]
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
  -- | Context.
  Text ->
  -- | Reason.
  Text ->
  -- | Suggestion
  Text ->
  UserErr
init context reason suggestion =
  UserErr reason suggestion [context]

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

-- * Ops

throwInMonadError :: (MonadError UserErr m) => Text -> Text -> [Text] -> m a
throwInMonadError reason suggestion contexts =
  throwError $ UserErr reason suggestion contexts

nestInMonadError :: (MonadError UserErr m) => Text -> Either UserErr a -> m a
nestInMonadError context = \case
  Right a -> return a
  Left err -> throwError $ addContext context err

-- * Conversion class

class ToUserErr a where
  toUserErr :: a -> UserErr
