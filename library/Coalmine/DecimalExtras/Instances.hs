module Coalmine.DecimalExtras.Instances where

import Coalmine.InternalPrelude
import Data.Attoparsec.Text qualified as P
import Data.Decimal qualified as D
import Data.Serialize qualified as Cereal
import Data.Text qualified as Text

instance LenientParser D.Decimal where
  lenientParser = do
    pos <- P.char '-' $> False <|> P.char '+' $> True <|> pure True
    intText <- decimalText
    placesText <- (P.char '.' *> decimalText) <|> pure mempty
    let placesTextLen = Text.length placesText
     in if placesTextLen > 255
          then fail $ "More than 255 decimal places: " <> show placesTextLen <> ". Input: " <> show placesText
          else
            let places = fromIntegral placesTextLen
                mantissa = foldIntTextTo (foldIntTextTo 0 intText) placesText
                signedMantissa = if pos then mantissa else negate mantissa
             in return $ D.Decimal places signedMantissa
    where
      decimalText =
        P.takeWhile1 isDecimal
        where
          isDecimal a = a >= '0' && a <= '9'
      foldIntTextTo acc text =
        Text.foldl' step acc text
        where
          step a b = a * 10 + fromIntegral (ord b - 48)

instance ToJSON D.Decimal where
  toJSON = toJSON . show

instance ToJSONKey D.Decimal where
  toJSONKey = contramap show toJSONKey

instance Cereal.Serialize D.Decimal where
  put (D.Decimal a b) = Cereal.put a >> Cereal.put b
  get = D.Decimal <$> Cereal.get <*> Cereal.get
