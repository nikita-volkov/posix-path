module PosixPath.Structures.Component
  ( Component (..),
    attoparsecParserOf,
    toTextBuilder,
  )
where

import Data.Attoparsec.Text qualified as Attoparsec
import PosixPath.Structures.Name qualified as Name
import TextBuilder qualified
import Prelude

data Component
  = NameComponent Name.Name
  | DotComponent
  | DotDotComponent

attoparsecParserOf :: Attoparsec.Parser Component
attoparsecParserOf = do
  name <- Name.attoparsecParserOf
  if Name.null name
    then do
      mplus
        ( do
            _ <- Attoparsec.char '.'
            mplus
              (Attoparsec.char '.' $> DotDotComponent)
              (pure DotComponent)
        )
        (pure (NameComponent name))
    else pure (NameComponent name)

toTextBuilder :: Component -> TextBuilder.TextBuilder
toTextBuilder = \case
  NameComponent name -> Name.toTextBuilder name
  DotComponent -> "."
  DotDotComponent -> ".."
