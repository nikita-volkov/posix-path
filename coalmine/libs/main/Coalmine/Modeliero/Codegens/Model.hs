{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Coalmine.Modeliero.Codegens.Model where

import Coalmine.HaskellCodegenKit.Namespace (Namespace)
import Coalmine.HaskellCodegenKit.Namespace qualified as Namespace
import Coalmine.HaskellCodegenKit.Package
import Coalmine.InternalPrelude
import Coalmine.Modeliero.Codegens.Structure (Structure)
import Coalmine.Modeliero.Codegens.Structure qualified as Structure
import Coalmine.Slug (Slug)

data Model = Model
  { rootNamespace :: Namespace,
    features :: Features,
    structures :: [Structure]
  }

data Features = Features
  { arbitrary :: Bool,
    aeson :: Bool,
    literal :: Bool,
    anonymization :: Bool,
    lens :: Bool,
    optics :: Bool
  }

compileModelModules :: Model -> Modules
compileModelModules model =
  Modules
    { public =
        mconcat
          [ [ compileFacadeModule model
            ],
            maybeToList (compileAnonymizationClassModule model)
          ],
      private =
        fmap (Structure.compileStructureModule model.rootNamespace) model.structures
    }

compileFacadeModule :: Model -> Module
compileFacadeModule =
  error "TODO"

compileAnonymizationClassModule :: Model -> Maybe Module
compileAnonymizationClassModule =
  error "TODO"
