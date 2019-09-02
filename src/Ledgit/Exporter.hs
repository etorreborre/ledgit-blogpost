{-# LANGUAGE RecordWildCards #-}

module Ledgit.Exporter where

import           Data.ByteString.Streaming
import           Data.Text                 as T (splitOn)
import           Ledgit.Data
import           Protolude                 hiding (ByteString)
import           Streaming
import           Streaming.Cassava         hiding (header)
import qualified Streaming.Prelude         as S
import           Streaming.With

data Exporter m = Exporter {
  exportCsv :: Stream (Of LedgerLine) m () -> m ()
}

newExporter :: forall m . (MonadIO m, MonadMask m) => CliOptions -> Exporter m
newExporter cliOptions = Exporter {..} where

  exportCsv :: Stream (Of LedgerLine) m () -> m ()
  exportCsv lines = writeBinaryFile (toS $ makeOutputFile cliOptions) $
    processLines lines

processLines :: (MonadIO m) => Stream (Of LedgerLine) m () -> ByteString m ()
processLines lines =
  lines &
  S.mapM setTodaysDateIfMissing &
  encodeByNameWith defaultEncodeOptions ynabHeader

-- | Create the output file path if none is given by appending -ynab to the
--   input file path
makeOutputFile :: CliOptions -> Text
makeOutputFile cliOptions =
  case outputFile cliOptions of
    Nothing ->
      case T.splitOn "." (inputFile cliOptions) of
        [name, extension] ->
          name <> "-ynab" <> "." <> extension
        _ ->
          panic (inputFile cliOptions <> " must have a .csv extension")

    Just f ->
      f
