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

import qualified Coalmine.BaseExtras.List as List
import Coalmine.Inter
import Coalmine.InternalPrelude
import Coalmine.MultilineTextBuilder (MultilineTextBuilder)
import Coalmine.Printing

-- * Err

data Err = Err
  { errReason :: Text,
    errSuggestion :: Text,
    errContexts :: [Context]
  }

instance BroadPrinting Err where
  toBroadBuilder Err {..} =
    [j|
      $errReason

      Suggestion:
        $errSuggestion

      Context:
        $renderedContexts
    |]
    where
      renderedContexts =
        List.mapIntercalate contextSplice "\n" errContexts

err :: Text -> Text -> [Context] -> Err
err = Err

-- * Context

newtype Context = Context {contextSplice :: MultilineTextBuilder}

context :: Text -> Text -> Context
context category details =
  Context [j|$category: $details|]
