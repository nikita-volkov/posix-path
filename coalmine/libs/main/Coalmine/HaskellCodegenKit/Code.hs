{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Coalmine.HaskellCodegenKit.Code where

import Coalmine.HaskellCodegenKit.Package
import Coalmine.InternalPrelude hiding (writeFile)

data Code = Code
  { importRequests :: HashMap Import Text,
    printer :: (Import -> Text) -> TextBuilder
  }

data Import = Import
  { -- | Possible external dependency.
    -- Affects the Cabal-file.
    --
    -- Nothing means that the module is from this package.
    dependency :: Maybe Dependency,
    name :: Text
  }

import_ ::
  Import ->
  -- | Code with the import request registered and
  -- either an empty splice or a qualification prefix ending with dot.
  Code
import_ =
  error "TODO"
