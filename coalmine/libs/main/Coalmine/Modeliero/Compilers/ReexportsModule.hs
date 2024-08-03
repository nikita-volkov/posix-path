{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Coalmine.Modeliero.Compilers.ReexportsModule where

import Coalmine.HaskellCodegenKit.Namespace (Namespace)
import Coalmine.HaskellCodegenKit.Namespace qualified as Namespace
import Coalmine.HaskellCodegenKit.Package
import Coalmine.InternalPrelude
import Coalmine.Modeliero.Codegens.Structure (Structure)
import Coalmine.Modeliero.Codegens.Structure qualified as Structure
import Coalmine.Slug (Slug)

data Params = Params
  { namespace :: Namespace,
    reexportedModules :: [Namespace]
  }

data Result = Result
  { module_ :: Module
  }

compile :: Params -> Result
compile =
  error "TODO"
