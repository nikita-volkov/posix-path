-- | General error model and API for applications.
--
-- Categorizes the errors by the way they should be handled.
-- It appears to be that for most apps the following categories
-- are enough:
--
-- * User-facing errors. In case of CLI apps those should be
-- rendered as messages in stderr.
--
-- * Logged warnings. Those should be rendered to application
-- warning log, which serves for debugging.
--
-- * Fatal errors. Application should stop.
module Coalmine.AppException where

import Coalmine.Inter
import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as Renderer
import qualified Data.Text as Text

data AppException = AppException
  { userReason :: Text,
    suggestion :: Text,
    contexts :: [Text],
    adminMsg :: Text,
    restoreEnv :: Bool
  }
  deriving (Show)

instance Exception AppException

-- * Rendering

renderForUserAsPlainText :: AppException -> Text
renderForUserAsPlainText e =
  to
    [j|
      ${e.userReason}

      Context:
        $compiledContext

      Suggestion:
        ${e.suggestion}
    |]
  where
    compiledContext =
      Text.intercalate "/" e.contexts

renderForAdminAsPlainText :: AppException -> Text
renderForAdminAsPlainText =
  error "TODO"

-- * Execution

-- |
-- Wrap an 'AppException'-throwing IO code,
-- handling those exceptions for a CLI app.
--
-- * 'userReason', 'suggestion' and 'contexts'
-- will be rendered to 'stderr'.
-- * 'adminMsg' will be rendered to 'stderr'.
-- * No attempt to restore the app will be performed and
-- such an exception will be rethrown.
handleForCli :: IO a -> IO a
handleForCli io =
  catch io $ \(appException :: AppException) ->
    error "TODO"

-- * Mapping

addContext :: Text -> AppException -> AppException
addContext context appException =
  appException {contexts = context : appException.contexts}
