module TerseDemo.Model where

import Coalmine.Prelude

data QuizConfig = QuizConfig
  { quizConfigTitle :: !Text,
    quizConfigQuestions :: ![QuestionConfig]
  }

data QuestionConfig

data PostQuizesJsonResponse
  = CreatedPostQuizesJsonResponse !PostQuizesJsonResponseCreated

data PostQuizesJsonResponseCreated = PostQuizesJsonResponseCreated
  { postQuizesJsonResponseCreatedId :: !UUID
  }

data QuizesQuizIdGetResponse
  = NotPublishedAndNotOwnedQuizesQuizIdGetResponse
  | NotFoundQuizesQuizIdGetResponse
  | QuizConfigQuizesQuizIdGetResponse
      !QuizConfig

data TokensPostResponse
  = Status200TokensPostResponse !Text
  | Status401TokensPostResponse

data TokensPostRequestBody
  = JsonTokensPostRequestBody !TokensPostRequestBodyJson

data TokensPostRequestBodyJson = TokensPostRequestBodyJson
  { tokensPostRequestBodyJsonEmail :: !Text,
    tokensPostRequestBodyJsonPassword :: !Text
  }
