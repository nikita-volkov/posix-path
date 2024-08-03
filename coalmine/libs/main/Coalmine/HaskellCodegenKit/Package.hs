module Coalmine.HaskellCodegenKit.Package where

import Coalmine.InternalPrelude hiding (writeFile)

-- * Writes

writeFile :: FilePath -> File -> IO ()
writeFile =
  error "TODO"

writeModule :: FilePath -> Module -> IO ()
writeModule workdir =
  writeFile workdir . compileModuleFile

writePackage :: FilePath -> Package -> IO ()
writePackage =
  error "TODO"

-- * Text

-- | Contents of the module.
printModule :: Module -> Text
printModule =
  error "TODO"

data File = File
  { path :: FilePath,
    content :: Text
  }

-- | Compile package to files.
compilePackageFiles :: Package -> [File]
compilePackageFiles =
  error "TODO"

-- | Compile module to a file.
compileModuleFile :: Module -> File
compileModuleFile =
  error "TODO"

compilePackageCabalFile :: Package -> File
compilePackageCabalFile =
  error "TODO"

data Package = Package
  { name :: Text,
    synopsis :: Text,
    publicModules :: [Module],
    privateModules :: [Module]
  }

data Module = Module
  { name :: [Text],
    dependencies :: [Dependency],
    content :: Text
  }

data Dependency
