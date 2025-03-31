module PosixPathStructures.Component
  ( Component (..),
  )
where

import Coalmine.Prelude
import Coalmine.SyntaxModelling qualified as Syntax
import Data.Attoparsec.Text qualified as Attoparsec
import PosixPathStructures.Name qualified as Name

data Component
  = NameComponent Name.Name
  | DotComponent
  | DotDotComponent

instance Syntax.Syntax Component where
  attoparsecParser = do
    name <- Syntax.attoparsecParser
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
  toTextBuilder = \case
    NameComponent name -> Syntax.toTextBuilder name
    DotComponent -> "."
    DotDotComponent -> ".."
