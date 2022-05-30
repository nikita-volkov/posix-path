module Main where

import Coalmine.Prelude
import qualified Coalmine.Terse as T

-- * --

main =
  error "TODO"

-- * --

api ::
  T.SecurityPolicy sess ->
  (QuizConfig -> StateT sess IO PostQuizesJsonResponse) ->
  [T.Route]
api securityPolicy postQuizesHandler =
  [ T.specificSegmentRoute "quizes" $
      [ T.securePostRoute
          securityPolicy
          [ T.byJsonContent (T.schemaDecoder quizConfigSchema)
              & fmap (fmap T.generalizeResponse . postQuizesHandler)
          ]
      ]
  ]

quizConfigSchema :: T.Schema QuizConfig
quizConfigSchema =
  T.objectSchema $
    QuizConfig
      <$> lmap
        quizConfigTitle
        (T.requiredSchemaField "title" T.stringSchema)
      <*> lmap
        quizConfigQuestions
        ( T.requiredSchemaField
            "questions"
            ( T.validatedSchema
                [ T.minItemsArrayValidator 1,
                  T.maxItemsArrayValidator 10
                ]
                (T.arraySchema questionConfigSchema)
            )
        )

questionConfigSchema :: T.Schema QuestionConfig
questionConfigSchema =
  error "TODO"

data QuizConfig = QuizConfig
  { quizConfigTitle :: !Text,
    quizConfigQuestions :: ![QuestionConfig]
  }

data QuestionConfig

data PostQuizesJsonResponse
  = JsonPostQuizesJsonResponse PostQuizesJsonResponseJson

instance T.SpecificResponse PostQuizesJsonResponse where
  generalizeResponse = \case
    JsonPostQuizesJsonResponse a ->
      T.jsonResponse
        201
        "Created"
        ( T.objectJson
            [ T.requiredJsonField
                "id"
                (T.schemaJson T.uuidSchema (postQuizesJsonResponseJsonId a))
            ]
        )

data PostQuizesJsonResponseJson = PostQuizesJsonResponseJson
  { postQuizesJsonResponseJsonId :: !UUID
  }
