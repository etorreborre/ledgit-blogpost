module Ledgit where

import           Ledgit.Data
import           Ledgit.App
import           Options.Applicative
import           Protolude           hiding (option)
import           Data.Registry

-- TODO process all the files in a given directory
runApplication :: IO ()
runApplication = do
  cliOptions <- execParser defineCliOptions
  let registry = newRegistry cliOptions
  let app = make @(App IO) registry
  runApp app

defineCliOptions :: ParserInfo CliOptions
defineCliOptions =
   info (cliOptionsParser <**> helper) $
   header "ledgit - massage ledger files"
   <> progDesc "Transform a CSV ledger file into a suitable YNAB file"

cliOptionsParser :: Parser CliOptions
cliOptionsParser = CliOptions
   <$> strArgument
       ( metavar "INPUT FILE"
      <> help "Input CSV file" )
   <*> option auto
       ( long "output-file"
      <> short 'o'
      <> value Nothing
      <> help "Output CSV file" )
