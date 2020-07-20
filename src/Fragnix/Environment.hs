module Fragnix.Environment
 ( loadEnvironment
 , persistEnvironment
 , persistBuiltinEnvironment
 ) where

import Language.Haskell.Names (Symbol)
import Language.Haskell.Names.Environment (
    Environment, readSymbols)
import Language.Haskell.Exts (
    ModuleName(ModuleName),prettyPrint)

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS (readFile, writeFile, pack)
import Data.Char (ord)
import qualified Data.Map as Map (
    fromList,toList,union)
import System.FilePath (
    (</>))
import System.Directory (
    createDirectoryIfMissing,doesFileExist,getDirectoryContents)
import Control.Monad (
    filterM,forM,forM_)


loadEnvironment' :: FilePath -> IO Environment
loadEnvironment' path = do
    createDirectoryIfMissing True path
    filenames <- getDirectoryContents path
    let pathmodulnames = map (\filename -> (ModuleName () filename,path </> filename)) filenames
    existingPathModulNames <- filterM (doesFileExist . snd) pathmodulnames
    fmap Map.fromList (forM existingPathModulNames (\(modulname,modulpath) -> do
        symbols <- readSymbols modulpath
        return (modulname,symbols)))

loadEnvironment :: IO Environment
loadEnvironment = do
  builtinEnvironment <- loadEnvironment' builtinEnvironmentPath
  userEnvironment <- loadEnvironment' environmentPath
  return (Map.union builtinEnvironment userEnvironment)

-- Replaces "writeSymbols" from haskell-names. Uses encodePretty instead.
writeSymbols :: FilePath -> [Symbol] -> IO ()
writeSymbols path symbols =
  BS.writeFile path $
    encodePretty symbols `mappend` BS.pack [fromIntegral $ ord '\n']

persistEnvironment :: Environment -> IO ()
persistEnvironment environment = do
    createDirectoryIfMissing True environmentPath
    forM_ (Map.toList environment) (\(modulname,symbols) -> do
        let modulpath = environmentPath </> prettyPrint modulname
        writeSymbols modulpath symbols)

environmentPath :: FilePath
environmentPath = "fragnix" </> "environment"

builtinEnvironmentPath :: FilePath
builtinEnvironmentPath = "fragnix" </> "builtin_environment"

persistBuiltinEnvironment :: Environment -> IO ()
persistBuiltinEnvironment environment = do
    createDirectoryIfMissing True builtinEnvironmentPath
    forM_ (Map.toList environment) (\(modulname,symbols) -> do
        let modulpath = builtinEnvironmentPath </> prettyPrint modulname
        writeSymbols modulpath symbols)
