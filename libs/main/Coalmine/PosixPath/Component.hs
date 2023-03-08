module Coalmine.PosixPath.Component
  ( Component (..),
  )
where

import Coalmine.BaseExtras.MonadPlus
import Coalmine.InternalPrelude
import Coalmine.PosixPath.Name qualified as Name
import Coalmine.SyntaxModelling qualified as Syntax
import Data.Attoparsec.Text qualified as Attoparsec

data Component
  = NameComponent Name.Name
  | DotComponent
  | DotDotComponent

instance Syntax.Syntax Component where
  attoparsec = do
    name <- Syntax.attoparsec
    if Name.null name
      then do
        mplus
          ( do
              Attoparsec.char '.'
              mplus
                (Attoparsec.char '.' $> DotDotComponent)
                (pure DotComponent)
          )
          (pure (NameComponent name))
      else pure (NameComponent name)
  textBuilder = \case
    NameComponent name -> Syntax.textBuilder name
    DotComponent -> "."
    DotDotComponent -> ".."
