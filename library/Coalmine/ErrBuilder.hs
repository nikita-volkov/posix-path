-- |
-- Error builder DSL.
module Coalmine.ErrBuilder
  ( err,
    Context,
    context,
  )
where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.Inter as Exports
import Coalmine.InternalPrelude
import Coalmine.MultilineTextBuilder (MultilineTextBuilder)

err :: MultilineTextBuilder -> MultilineTextBuilder -> [Context] -> Text
err reason suggestion contexts =
  to
    [j|
      $reason

      Suggestion:
        $suggestion

      Context:
        $renderedContexts
    |]
  where
    renderedContexts =
      List.mapIntercalate contextSplice "\n" contexts

-- * Context

newtype Context = Context {contextSplice :: MultilineTextBuilder}

context :: Text -> Text -> Context
context category details =
  Context [j|$category: $details|]
