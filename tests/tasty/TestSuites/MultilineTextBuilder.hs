module TestSuites.MultilineTextBuilder where

import Coalmine.MultilineTextBuilder
import Coalmine.Prelude hiding (choose)
import Coalmine.Tasty
import Data.Text qualified as Text

tests =
  [ testProperty "Concatting lifted texts equals concatting originals" $
      \(texts :: [Text]) ->
        mconcat texts === to (foldMap (to @Builder) texts),
    testProperty "Indentation behaves the same as for text" $
      \(blocks :: [(Indentation, Text)]) ->
        let expected =
              foldMap compileBlock blocks
              where
                compileBlock (Indentation indentation, text) =
                  indentTextLines indentation text
            actual =
              to @Text $ foldMap compileBlock blocks
              where
                compileBlock (Indentation indentation, text) =
                  indent indentation $ to text
         in expected === actual
  ]

newtype Indentation = Indentation Int
  deriving (Eq, Show)

instance Arbitrary Indentation where
  arbitrary =
    Indentation <$> choose (0, 99)

indentTextLines :: Int -> Text -> Text
indentTextLines indentation =
  prefixTextLines $ Text.replicate indentation " "

prefixTextLines :: Text -> Text -> Text
prefixTextLines prefix =
  Text.replace "\n" ("\n" <> prefix)
