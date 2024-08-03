module Coalmine.Modeliero.Codegens.Model where

import Coalmine.HaskellCodegenKit.Package
import Coalmine.InternalPrelude

compileModelModules :: Model -> [Module]
compileModelModules =
  error "TODO"

data Model = Model
  { features :: Features,
    structures :: [Structure]
  }

data Features = Features
  { aeson :: Bool,
    literal :: Bool,
    anonymization :: Bool
  }

data Structure = Structure
  { name :: Text,
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
