module Main where

import Coalmine.Prelude
import Coalmine.Terse
import qualified TerseDemo.Model as M

-- * --

main =
  error "TODO"

-- * --

api ::
  SecurityPolicy sess ->
  (M.QuizConfig -> StateT sess IO M.PostQuizesJsonResponse) ->
  [Route]
api securityPolicy postQuizesHandler =
  [ specificSegmentRoute "quizes" $
      [ securePostRoute
          securityPolicy
          [ let renderResponse = \case
                  M.CreatedPostQuizesJsonResponse a ->
                    jsonResponse
                      201
                      "Created"
                      ( objectJson
                          [ requiredJsonField
                              "id"
                              (schemaJson uuidSchema (M.postQuizesJsonResponseCreatedId a))
                          ]
                      )
             in byJsonContent (schemaDecoder quizConfigSchema)
                  & fmap (fmap renderResponse . postQuizesHandler)
          ]
      ]
  ]

quizConfigSchema :: Schema M.QuizConfig
quizConfigSchema =
  objectSchema $
    M.QuizConfig
      <$> lmap
        M.quizConfigTitle
        (requiredSchemaField "title" stringSchema)
      <*> lmap
        M.quizConfigQuestions
        ( requiredSchemaField
            "questions"
            ( validatedSchema
                [ minItemsArrayValidator 1,
                  maxItemsArrayValidator 10
                ]
                (arraySchema questionConfigSchema)
            )
        )

questionConfigSchema :: Schema M.QuestionConfig
questionConfigSchema =
  error "TODO"
