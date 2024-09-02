-- |
-- Error builder DSL.
--
-- TODO: Add rendering to colored terminal.
module Coalmine.ErrBuilder
  ( Err,
    err,
    Context,
    context,
  )
where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.Inter
import Coalmine.InternalPrelude
import Coalmine.MultilineTextBuilder (MultilineTextBuilder)
import Coalmine.Printing

-- * Err

data Err = Err
  { reason :: Text,
    suggestion :: Text,
    contexts :: [Context]
  }

instance BroadPrinting Err where
  toBroadBuilder err =
    [j|
      ${err.reason}

      Suggestion:
        ${err.suggestion}

      Context:
        ${renderedContexts}
    |]
    where
      renderedContexts =
        List.mapIntercalate (.splice) "\n" err.contexts

err :: Text -> Text -> [Context] -> Err
err = Err

-- * Context

newtype Context = Context {splice :: MultilineTextBuilder}

context :: Text -> Text -> Context
context category details =
  Context [j|${category}: ${details}|]
