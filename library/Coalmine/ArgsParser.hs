module Coalmine.ArgsParser
  ( -- * Top-level execution
    getAndConsumeArgsHappily,

    -- * Args consumer
    Consumer,
    parse,
    int,
    enum,
    text,
  )
where

import Coalmine.Inter
import Coalmine.InternalPrelude
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text

-- * Execution

data Err
  = ConsumptionErr
      Int
      -- ^ Index that we're at.
      ConsumptionErr
  | TooManyArgsErr Int
  deriving (Show)

-- | Read CLI args dying with a printed error in case of failure.
--
-- Useful for CLI apps.
getAndConsumeArgsHappily :: Consumer a -> IO a
getAndConsumeArgsHappily consumer =
  getArgs >>= \args -> case consume consumer args of
    Right res -> return res
    Left err -> die . to . renderErr $ err

consume :: Consumer a -> [String] -> Either Err a
consume (Consumer run) inputs =
  case run 0 inputs of
    Right (_, argsTail, res) -> case argsTail of
      [] -> Right res
      _ -> Left $ TooManyArgsErr (length argsTail)
    Left (pos, err) -> Left $ ConsumptionErr pos err

renderErr :: Err -> Text
renderErr = \case
  TooManyArgsErr n ->
    [i|Got $n unexpected arguments|]
  ConsumptionErr index err -> case err of
    ExhaustedConsumptionErr ->
      [i|Not enough arguments. Consumed $index|]
    ParsingConsumptionErr err input -> case err of
      InvalidIntParsingErr ->
        fromReason "Not a valid int"
      SmallerIntParsingErr n ->
        fromReason [i|Int is smaller than $n|]
      LargerIntParsingErr n ->
        fromReason [i|Int is larger than $n|]
      MissingEnumParsingErr options ->
        fromReason [i|No such enum option. Expecting one of $optionsString|]
        where
          optionsString = show options
      TextParsingErr err -> case err of
        TooShortTextErr n -> fromReason [i|Shorter than $n|]
        TooLongTextErr n -> fromReason [i|Longer than $n|]
      where
        fromReason :: TextBuilder -> Text
        fromReason reason =
          [i|Failed to parse arg "$input" at index $index: $reason|]

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
  deriving (Show)

data ParsingErr
  = InvalidIntParsingErr
  | SmallerIntParsingErr Int
  | LargerIntParsingErr Int
  | MissingEnumParsingErr [Text]
  | TextParsingErr TextErr
  deriving (Show)

data TextErr
  = TooShortTextErr Int
  | TooLongTextErr Int
  deriving (Show)

parse :: (String -> Either ParsingErr a) -> Consumer a
parse parseArg = Consumer $ \offset -> \case
  [] -> Left (offset, ExhaustedConsumptionErr)
  h : t -> case parseArg h of
    Left err -> Left (offset, ParsingConsumptionErr err h)
    Right res -> let !nextOffset = succ offset in Right (nextOffset, t, res)

int :: Int -> Int -> Consumer Int
int min max = parse $ \input -> case readMaybe input of
  Just int ->
    if int < min
      then Left $ SmallerIntParsingErr min
      else
        if int > max
          then Left $ LargerIntParsingErr max
          else Right int
  Nothing -> Left InvalidIntParsingErr

enum :: [(Text, a)] -> Consumer a
enum list = parse $ \input ->
  case lookup (fromString input) list of
    Just res -> Right res
    Nothing -> Left $ MissingEnumParsingErr $ fmap fst list

text :: Int -> Int -> Consumer Text
text minLength maxLength = parse $ \input ->
  case fromString input of
    text -> case Text.length text of
      length ->
        if length < minLength
          then Left $ TextParsingErr $ TooShortTextErr minLength
          else
            if length > maxLength
              then Left $ TextParsingErr $ TooLongTextErr maxLength
              else Right text
