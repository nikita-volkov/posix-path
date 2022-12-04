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
import Coalmine.EvenSimplerPaths qualified as Paths
import Coalmine.InternalPrelude hiding (print)
import Coalmine.MultilineTextBuilder qualified as B
import Coalmine.Printing
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TextIO

-- * --

newtype Fileset = Fileset {map :: Map Path Text}
  deriving (Semigroup, Monoid)

instance IsList Fileset where
  type Item Fileset = (Path, Text)
  fromList = Fileset . fromList
  toList = toList . (.map)

instance BroadPrinting Fileset where
  toBroadBuilder =
    B.intercalate "\n\n" . fmap renderFile . toList
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
write =
  traverse_ (uncurry writeFileCreatingDirs) . toList
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
  Fileset $ Map.singleton path content

-- |
-- Prepend a directory path to all contents of this package.
inDir :: Path -> Fileset -> Fileset
inDir path =
  Fileset . Map.mapKeys (mappend path) . (.map)
