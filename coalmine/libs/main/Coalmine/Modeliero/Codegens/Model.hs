module Coalmine.Modeliero.Codegens.Model where

import Coalmine.HaskellCodegenKit.Package
import Coalmine.InternalPrelude

data Model = Model
  { features :: Features,
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

compileModelModules ::
  -- | Prefix namespace.
  [Text] ->
  Model ->
  Modules
compileModelModules namespace model =
  Modules
    { public =
        mconcat
          [ [ compileFacadeModule namespace model
            ],
            maybeToList (compileClassesModule namespace model),
            maybeToList (compileAnonymizationClassModule namespace model)
          ],
      private =
        fmap (compileStructureModule namespace) model.structures
    }

compileFacadeModule :: [Text] -> Model -> Module
compileFacadeModule =
  error "TODO"

compileClassesModule :: [Text] -> Model -> Maybe Module
compileClassesModule =
  error "TODO"

compileAnonymizationClassModule :: [Text] -> Model -> Maybe Module
compileAnonymizationClassModule =
  error "TODO"

compileStructureModule :: [Text] -> Structure -> Module
compileStructureModule =
  error "TODO"
