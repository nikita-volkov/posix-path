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

import Coalmine.InternalPrelude

type ErrorReport = [ErrorReportLevel]

data ErrorReportLevel = ErrorReportLevel
  { -- | Static message describing what happened.
    --
    -- It should not be a dynamically constructed string.
    -- Use the @details@ field for that.
    message :: Text,
    -- | Map describing various facts.
    details :: [(Text, Json)]
  }

class ConvertsToErrorReport a where
  toErrorReport :: a -> ErrorReport

toJson :: ErrorReport -> Text
toJson =
  error "TODO"

toJsonTree :: ErrorReport -> Json
toJsonTree =
  ArrayJson
    . fmap
      ( \level ->
          [ ( "message",
              level.message
                & toJSON
            ),
            ( "details",
              level.details
                & (fmap . first) (fromString . toList)
                & fromList
                & ObjectJson
            )
          ]
            & fromList
            & ObjectJson
      )
    . fromList

toYaml :: ErrorReport -> Text
toYaml = renderAsYamlText . toJsonTree
