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
  (M.QuizesPostRequestBody -> StateT sess IO M.QuizesPostResponse) ->
  (M.TokensPostRequestBody -> IO M.TokensPostResponse) ->
  [Route]
api securityPolicy postQuizesHandler tokensPostHandler =
  [ specificSegmentRoute "quizes" $
      [ securePostRoute
          securityPolicy
          [ fmap M.JsonQuizesPostRequestBody . jsonRequestBody . schemaDecoder $
              quizConfigSchema
          ]
          ( let renderResponse = \case
                  M.Status201QuizesPostResponse a ->
                    response 201 "Created" $
                      [ jsonResponseContent $
                          objectJson
                            [ requiredJsonField
                                "id"
                                (schemaJson uuidSchema (M.quizesPostResponseStatus201JsonId a))
                            ]
                      ]
             in fmap renderResponse . postQuizesHandler
          )
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
