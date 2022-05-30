module TerseDemo.Model where

import Coalmine.Prelude

data QuizConfig = QuizConfig
  { quizConfigTitle :: !Text,
    quizConfigQuestions :: ![QuestionConfig]
  }

data QuestionConfig

data PostQuizesJsonResponse
  = CreatedPostQuizesJsonResponse PostQuizesJsonResponseCreated

data PostQuizesJsonResponseCreated = PostQuizesJsonResponseCreated
  { postQuizesJsonResponseCreatedId :: !UUID
  }
