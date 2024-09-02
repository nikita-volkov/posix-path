module Coalmine.RestEasy.BodyConsumers where

import AesonValueParser qualified
import Coalmine.Inter
import Coalmine.InternalPrelude
import Data.Aeson qualified as Aeson
import Data.Aeson.Parser qualified as AesonParser
import Data.Attoparsec.ByteString qualified as AttoparsecByteString
import Data.ByteString qualified as ByteString
import Data.Serialize qualified as Cereal

type BodyConsumer a =
  IO ByteString -> IO (Either Text a)

refine :: (a -> Either Text b) -> BodyConsumer a -> BodyConsumer b
refine refiner consumer fetch =
  consumer fetch <&> \case
    Right res -> refiner res
    Left err -> Left err

aesonValueParser :: AesonValueParser.Value a -> BodyConsumer a
aesonValueParser parser =
  refine refiner aeson
  where
    refiner = AesonValueParser.runWithTextError parser

aeson :: BodyConsumer Aeson.Value
aeson =
  attoparsecByteStringParser AesonParser.json

attoparsecByteStringParser :: AttoparsecByteString.Parser a -> BodyConsumer a
attoparsecByteStringParser parser fetch =
  fetch >>= AttoparsecByteString.parseWith fetch parser >>= \case
    AttoparsecByteString.Done _rmdr res ->
      return $ Right res
    AttoparsecByteString.Fail _rmdr contexts msg ->
      return $ Left $ to [j|$contextsBdr: $msg|]
      where
        contextsBdr = foldMap (mappend "/") contexts
    AttoparsecByteString.Partial _cont ->
      return $ Left "Not enough input"

cereal :: Cereal.Get a -> BodyConsumer a
cereal get fetch =
  go $ Cereal.runGetPartial get
  where
    go decode = do
      chunk <- fetch
      if ByteString.null chunk
        then return $ Left "Not enough data"
        else case decode chunk of
          Cereal.Done res _ -> return $ Right res
          Cereal.Fail err _ -> return $ Left $ to err
          Cereal.Partial decodeNext -> go decodeNext
