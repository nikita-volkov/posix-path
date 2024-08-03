{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Coalmine.HaskellCodegenKit.Code where

import Coalmine.HaskellCodegenKit.Package
import Coalmine.InternalPrelude hiding (writeFile)
import Data.Text qualified as Text

compileCodeModule :: [Text] -> Code -> Module
compileCodeModule name code =
  Module
    { name,
      dependencies = compileCodeDependencies code,
      content = compileCodeContent (Text.intercalate "." name) code
    }

compileCodeDependencies :: Code -> [Dependency]
compileCodeDependencies =
  error "TODO"

compileCodeContent :: Text -> Code -> Text
compileCodeContent =
  error "TODO"

data Code = Code
  { importRequests :: HashMap Import Text,
    -- | Function on prefix namespace and import resolver.
    printer :: [Text] -> (Import -> Text) -> TextBuilder
  }

data Import = Import
  { -- | Possible external dependency.
    -- Affects the Cabal-file.
    --
    -- Nothing means that the imported module is from the same package.
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
