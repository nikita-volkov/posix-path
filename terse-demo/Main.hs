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
  (M.TokensPostRequestBody -> IO M.TokensPostResponse) ->
  [Route]
api securityPolicy postQuizesHandler tokensPostHandler =
  [ specificSegmentRoute "quizes" $
      [ securePostRoute
          securityPolicy
          [ let renderResponse = \case
                  M.CreatedPostQuizesJsonResponse a ->
                    response 201 "Created" $
                      [ jsonResponseContent $
                          objectJson
                            [ requiredJsonField
                                "id"
                                (schemaJson uuidSchema (M.postQuizesJsonResponseCreatedId a))
                            ]
                      ]
             in jsonRequestBody (schemaDecoder quizConfigSchema)
                  & fmap (fmap renderResponse . postQuizesHandler)
          ]
      ],
    specificSegmentRoute "tokens" $
      [ insecurePostRoute
          [ fmap M.JsonTokensPostRequestBody . jsonRequestBody . schemaDecoder . objectSchema $
              M.TokensPostRequestBodyJson
                <$> lmap
                  M.tokensPostRequestBodyJsonEmail
                  (requiredSchemaField "email" emailSchema)
                <*> lmap
                  M.tokensPostRequestBodyJsonPassword
                  (requiredSchemaField "password" passwordSchema)
          ]
          ( let renderResponse = \case
                  M.Status200TokensPostResponse token ->
                    response 200 "Authenticated" $
                      [ jsonResponseContent $
                          schemaJson stringSchema token
                      ]
                  M.Status401TokensPostResponse ->
                    response 401 "Unauthorized" []
             in fmap renderResponse . tokensPostHandler
          )
      ]
  ]

-- * Schemas

quizConfigSchema :: Schema M.QuizConfig
quizConfigSchema =
  objectSchema $
    M.QuizConfig
      <$> lmap
        M.quizConfigTitle
        ( requiredSchemaField "title" stringSchema
        )
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

emailSchema :: Schema Text
emailSchema =
  error "TODO"

passwordSchema :: Schema Text
passwordSchema =
  error "TODO"
