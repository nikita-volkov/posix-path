module Coalmine.UserErr where

import qualified Coalmine.BaseExtras.List as ListExtras
import Coalmine.Inter
import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as Printer
import Coalmine.Name (Name)
import Coalmine.Printing
import qualified Data.Text as Text

-- |
-- Contextual error report intended for the app user.
--
-- Allows to easily nest the error in contexts.
data UserErr = UserErr
  { reason :: Text,
    suggestion :: Text,
    contexts :: [Name]
  }
  deriving (Show)

instance BroadPrinting UserErr where
  toBroadBuilder e =
    ListExtras.intercalate "\n\n" . catMaybes $
      [ if Text.null e.reason
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
                      $compiledContext
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

-- * Mapping

addContext :: Name -> UserErr -> UserErr
addContext context appException =
  appException {contexts = context : appException.contexts}

addContextInEither :: Name -> Either UserErr a -> Either UserErr a
addContextInEither context =
  first $ addContext context

addContextInMonadError :: MonadError UserErr m => Name -> m a -> m a
addContextInMonadError context =
  handleError $ throwError . addContext context
