module PosixPath.Structures.Name.Charsets where

import Prelude

-- https://stackoverflow.com/a/35352640/485115
notFileName :: String
notFileName = "./\\:\NUL*?\"<>|"
