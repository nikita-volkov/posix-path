module TerseDemo.Model where

import Coalmine.Prelude

-- * --

data QuizConfig = QuizConfig
  { quizConfigTitle :: !Text,
    quizConfigQuestions :: ![QuestionConfig]
  }

data QuestionConfig

-- * --

data QuizesPostRequestBody
  = JsonQuizesPostRequestBody !QuizConfig

data QuizesPostResponse
  = Status201QuizesPostResponse QuizesPostResponseStatus201Json

data QuizesPostResponseStatus201Json = QuizesPostResponseStatus201Json
  { quizesPostResponseStatus201JsonId :: !UUID
  }

-- * --

data QuizesQuizIdGetResponse
  = -- | Quiz config.
    Status200QuizesQuizIdGetResponse
      QuizConfig
      -- ^ JSON content.
  | -- | Not published and not owned.
    Status403QuizesQuizIdGetResponse
  | -- | Not found.
    Status404QuizesQuizIdGetResponse

-- * --

data TokensPostRequestBody
  = JsonTokensPostRequestBody !TokensPostRequestBodyJson

data TokensPostRequestBodyJson = TokensPostRequestBodyJson
  { tokensPostRequestBodyJsonEmail :: !Text,
    tokensPostRequestBodyJsonPassword :: !Text
  }

data TokensPostResponse
  = -- | Authenticated.
    Status200TokensPostResponse
      Text
      -- ^ JSON content.
  | -- | Unauthorized.
    Status401TokensPostResponse
