module Coalmine.ArgsParser
  ( -- * Top-level execution
    consume,
    readAndConsumeArgsHappily,

    -- * Args consumer
    Consumer,
    parse,

    -- * Arg parser
    Parser,
    minMaxInt,
    enum,
  )
where

import Coalmine.InternalPrelude
import qualified Data.Attoparsec.Text as Attoparsec

-- * Execution

data Err
  = ConsumptionErr
      Int
      -- ^ Index that we're at.
      ConsumptionErr
  | TooManyErr

consume :: Consumer a -> String -> Either Err a
consume =
  error "TODO"

-- | Read CLI args dying with a rendered error in case of failure.
--
-- Useful for CLI apps.
readAndConsumeArgsHappily :: Consumer a -> IO a
readAndConsumeArgsHappily =
  error "TODO"

-- * Args consumer

newtype Consumer a = Consumer (Int -> [String] -> (Int, Either ConsumptionErr a))

data ConsumptionErr
  = -- | No more args available to fetch from.
    ExhaustedConsumptionErr
  | -- | The arg exists, but we've failed parsing it.
    ParsingConsumptionErr
      ParsingErr
      -- ^ Error details.

parse :: Parser a -> Consumer a
parse (Parser parseArg) = Consumer $ \offset -> \case
  [] -> (offset, Left ExhaustedConsumptionErr)
  h : t -> case parseArg h of
    Right res -> let !nextOffset = succ offset in (nextOffset, Right res)
    Left err -> (offset, Left . ParsingConsumptionErr $ err)

-- * Arg parser

newtype Parser a = Parser (String -> Either ParsingErr a)
  deriving (Functor)

data ParsingErr
  = InvalidIntParsingErr
  | SmallerIntParsingErr Int
  | LargerIntParsingErr Int
  | MissingEnumParsingErr

minMaxInt :: Int -> Int -> Parser Int
minMaxInt min max =
  Parser $ \input -> case readMaybe input of
    Just int ->
      if int < min
        then Left $ SmallerIntParsingErr min
        else
          if int > max
            then Left $ LargerIntParsingErr max
            else Right int
    Nothing -> Left InvalidIntParsingErr

enum :: [(Text, a)] -> Parser a
enum =
  error "TODO"
