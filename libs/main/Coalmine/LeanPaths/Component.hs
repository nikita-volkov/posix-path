module Coalmine.LeanPaths.Component
  ( Component,
  )
where

import Coalmine.InternalPrelude
import Coalmine.LeanPaths.Name qualified as Name
import Coalmine.SyntaxModelling qualified as Syntax

data Component
  = NameComponent Name.Name
  | DotComponent
  | DotDotComponent
