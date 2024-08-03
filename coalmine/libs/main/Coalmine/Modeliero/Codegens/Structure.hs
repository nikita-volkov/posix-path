{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Coalmine.Modeliero.Codegens.Structure where

import Coalmine.HaskellCodegenKit.Namespace (Namespace)
import Coalmine.HaskellCodegenKit.Namespace qualified as Namespace
import Coalmine.HaskellCodegenKit.Package
import Coalmine.InternalPrelude
import Coalmine.Slug (Slug)

data Structure = Structure
  { name :: Slug,
    definition :: StructureDefinition
  }

data StructureDefinition
  = ProductStructureDefinition ProductDefinition
  | RefinedStructureDefinition RefinedType

data RefinedType
  = IntegerRefinedType IntegerRestrictions
  | FloatRefinedType FloatRestrictions
  | StringRefinedType StringRestrictions
  | VectorRefinedType VectorRestrictions

data IntegerRestrictions = IntegerRestrictions
  { min :: Maybe Integer,
    max :: Maybe Integer
  }

data FloatRestrictions = FloatRestrictions
  { min :: Maybe Float,
    max :: Maybe Float
  }

data StringRestrictions = StringRestrictions
  { regex :: Maybe Text,
    minLength :: Maybe Int,
    maxLength :: Maybe Int
  }

data VectorRestrictions = VectorRestrictions
  { element :: Either RefinedType Type,
    minLength :: Maybe Int,
    maxLength :: Maybe Int
  }

data ProductDefinition = ProductDefinition
  { fields :: [ProductField]
  }

data ProductField = ProductField
  { name :: Text,
    type_ :: Type,
    anonymize :: Bool
  }

data Type
  = ModelType Text
  | MaybeType Type
  | VectorType Type
  | MapType Type Type
  | SetType Type
  | IntType
  | FloatType
  | ScientificType
  | TextType
  | UuidType
  | EmailType
  | UrlType
  | PosixPathType
  | IpType

compileStructureModuleName :: Namespace -> Structure -> Text
compileStructureModuleName =
  error "TODO"

compileStructureModule :: Namespace -> Structure -> Module
compileStructureModule =
  error "TODO"
