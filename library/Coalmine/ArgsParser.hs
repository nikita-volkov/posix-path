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

newtype Consumer a = Consumer (Int -> [String] -> Either (Int, ConsumptionErr) (Int, [String], a))

instance Functor Consumer where
  fmap f (Consumer run) =
    Consumer $ \i args -> fmap (fmap f) (run i args)

instance Applicative Consumer where
  pure a = Consumer $ \i args -> Right (i, args, a)
  (<*>) = ap

instance Monad Consumer where
  return = pure
  Consumer runL >>= k =
    Consumer $ \i args ->
      case runL i args of
        Right (i, args, res) -> case k res of
          Consumer runR -> runR i args
        Left err -> Left err

data ConsumptionErr
  = -- | No more args available to fetch from.
    ExhaustedConsumptionErr
  | -- | The arg exists, but we've failed parsing it.
    ParsingConsumptionErr
      ParsingErr
      -- ^ Error details.
      String
      -- ^ Input.

parse :: Parser a -> Consumer a
parse (Parser parseArg) = Consumer $ \offset -> \case
  [] -> Left (offset, ExhaustedConsumptionErr)
  h : t -> case parseArg h of
    Left err -> Left (offset, ParsingConsumptionErr err h)
    Right res -> let !nextOffset = succ offset in Right (nextOffset, t, res)

-- * Arg parser

newtype Parser a = Parser (String -> Either ParsingErr a)
  deriving (Functor)

data ParsingErr
  = InvalidIntParsingErr
  | SmallerIntParsingErr Int
  | LargerIntParsingErr Int
  | MissingEnumParsingErr [Text]

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
