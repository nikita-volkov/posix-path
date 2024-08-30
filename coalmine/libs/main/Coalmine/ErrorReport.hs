-- |
-- Description of an error intended for rendering into various formats,
-- but not for handling the various cases for logic.
-- For that purpose you should model the errors via ADTs.
-- But in the end parts of those ADTs will still remain unhandled.
-- Those are the ones that you have no better handling for than just delegating it to the user.
-- This datastructure is intended for those cases.
--
-- It provides a way for you to output structured error details that can be rendered into various human-readable formats.
-- The most evident formats are YAML and JSON.
module Coalmine.ErrorReport where

import Coalmine.BaseExtras.Monoid
import Coalmine.InternalPrelude

-- | Chain of levels diving deeper into the causes.
type ErrorReport = [Cause]

data Cause = Cause
  { -- | Label. A static message describing what happened.
    --
    -- It should not be a dynamically constructed string.
    -- Use the @details@ field for that.
    message :: Text,
    -- | Map describing various facts.
    details :: [(Text, Json)]
  }

class ConvertsToErrorReport a where
  toErrorReport :: a -> ErrorReport

instance ConvertsToErrorReport SomeException where
  toErrorReport someException =
    Cause
      { message = "SomeException",
        details = []
      }
      & pure

toJson :: ErrorReport -> Text
toJson =
  error "TODO"

toJsonTree :: ErrorReport -> Json
toJsonTree =
  ArrayJson
    . fmap
      ( \level ->
          [ level.message
              & toJSON
              & Just
              & fmap ("label",),
            level.details
              & (fmap . first) (fromString . toList)
              & filtered (not . null) Just
              & fmap fromList
              & fmap ObjectJson
              & fmap ("details",)
          ]
            & catMaybes
            & fromList
            & ObjectJson
      )
    . fromList

toYaml :: ErrorReport -> Text
toYaml = renderAsYamlText . toJsonTree
