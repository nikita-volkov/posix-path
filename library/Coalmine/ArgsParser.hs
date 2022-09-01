module Coalmine.ArgsParser
  ( ArgsParser,
    arg,

    -- * Arg
    ArgParser,
    minMaxIntArg,
  )
where

import Coalmine.InternalPrelude
import qualified Data.Attoparsec.Text as Attoparsec

-- * Args

newtype ArgsParser a = ArgsParser (Int -> [String] -> Either Err a)

data Err
  = ConsumptionErr
      Int
      -- ^ Index that we're at.
      ConsumptionErr
  | TooManyErr

data ConsumptionErr
  = -- | No more args available to fetch from.
    ExhaustedConsumptionErr
  | -- | The arg exists, but we've failed parsing it.
    ParsingConsumptionErr
      ParsingErr
      -- ^ Error details.

data ParsingErr
  = InvalidIntParsingErr
  | SmallerIntParsingErr Int
  | LargerIntParsingErr Int

arg :: ArgParser a -> ArgsParser a
arg (ArgParser parseArg) = ArgsParser $ \offset -> \case
  [] -> Left $ ConsumptionErr offset ExhaustedConsumptionErr
  head : tail -> error "TODO"

-- * Arg

newtype ArgParser a = ArgParser (String -> Either ParsingErr a)
  deriving (Functor)

minMaxIntArg :: Int -> Int -> ArgParser Int
minMaxIntArg min max =
  ArgParser $ \input -> case readMaybe input of
    Just int ->
      if int < min
        then Left $ SmallerIntParsingErr min
        else
          if int > max
            then Left $ LargerIntParsingErr max
            else Right int
