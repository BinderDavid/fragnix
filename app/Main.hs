module Main where

import Build (build)
import CreateEnv (createEnv)
import Paths_fragnix
import Data.Version (showVersion)

import Options.Applicative (
  ParserInfo, Parser, execParser,
  subparser, command, info, infoOption, long, help,
  progDesc, header, metavar, helper, (<**>),
  many, argument, str, auto, fullDesc)


data Command = Build [FilePath] | CreateEnv

commandParserInfo :: ParserInfo Command
commandParserInfo =
  info (commandParser  <**> helper <**> versionOption) (fullDesc <> header "fragnix - fragment-based code distribution")

commandParser :: Parser Command
commandParser = subparser (mconcat
                         [ command "build" (info (buildParser <**> helper) (progDesc "Build the given list of modules."))
                         , command "create-env" (info (createEnvParser <**> helper) (progDesc "Create the builtin environment."))
                         ])
  where
    buildParser :: Parser Command
    buildParser = Build <$> many (argument str (metavar "TARGET"))

    createEnvParser :: Parser Command
    createEnvParser = pure CreateEnv

versionOption :: Parser (a -> a)
versionOption = infoOption versionText (long "version" <> help "Show version")
  where
    versionText = "Version " <> showVersion version

main :: IO ()
main = do
   command <- execParser commandParserInfo
   case command of
     Build modulePaths -> build modulePaths
     CreateEnv -> createEnv