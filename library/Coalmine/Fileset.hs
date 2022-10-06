module Coalmine.Fileset
  ( -- * --
    Fileset,

    -- ** Execution
    write,
    print,

    -- ** --
    file,
    inDir,
  )
where

import Coalmine.EvenSimplerPaths (Path)
import qualified Coalmine.EvenSimplerPaths as Paths
import Coalmine.InternalPrelude hiding (print)
import qualified Coalmine.MultilineTextBuilder as B
import Coalmine.Printing
import qualified Data.Text.IO as TextIO

-- * --

newtype Fileset = Fileset [(Path, Text)]
  deriving (Semigroup, Monoid)

instance IsList Fileset where
  type Item Fileset = (Path, Text)
  fromList = Fileset
  toList = coerce

instance BroadPrinting Fileset where
  toBroadBuilder (Fileset package) =
    B.intercalate "\n\n" $ fmap renderFile package
    where
      renderFile (path, contents) =
        mconcat
          [ toBroadBuilder path,
            ":",
            B.indent 2 $ mappend "\n" $ to contents
          ]

-- * Execution

-- | Write to file system.
write :: Fileset -> IO ()
write (Fileset files) =
  traverse_ (uncurry writeFileCreatingDirs) files
  where
    writeFileCreatingDirs path contents = do
      Paths.createDirsTo path
      TextIO.writeFile (printCompactAsString path) contents

-- | Output to 'stdout'.
print :: Fileset -> IO ()
print = printBroadToStdOut

-- * --

file :: Path -> Text -> Fileset
file path content =
  Fileset [(path, content)]

-- |
-- Prepend a directory path to all contents of this package.
inDir :: Path -> Fileset -> Fileset
inDir path (Fileset contents) =
  Fileset $ fmap (first (mappend path)) contents
