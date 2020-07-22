module Fragnix.Paths where

import System.FilePath ((</>))

environmentPath :: FilePath
environmentPath = "env"

fragnixBasePath :: FilePath
fragnixBasePath = "fragnix"

builtinEnvironmentPath :: FilePath
builtinEnvironmentPath = fragnixBasePath </> "builtin_environment"

slicesPath :: FilePath
slicesPath = fragnixBasePath </> "slices"

cbitsPath :: FilePath
cbitsPath = fragnixBasePath </> "cbits"

includePath :: FilePath
includePath = fragnixBasePath </> "include"

preprocessedPath :: FilePath
preprocessedPath = fragnixBasePath </> "temp" </> "preprocessed"

-- | Directory for generated modules
compilationunitsPath :: FilePath
compilationunitsPath = fragnixBasePath </> "temp" </> "compilationunits"

buildPath :: FilePath
buildPath = fragnixBasePath </> "temp" </> "build"

declarationsPath :: FilePath
declarationsPath = fragnixBasePath </> "temp" </> "declarations/declarations.json"
