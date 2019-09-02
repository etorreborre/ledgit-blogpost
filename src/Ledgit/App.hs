{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Ledgit.App where

import           Data.Registry
import           Ledgit.Data
import           Ledgit.Exporter
import           Ledgit.Importer
import           Protolude

data App m = App {
  runApp :: m ()
}

newApp :: Importer m -> Exporter m -> App m
newApp Importer {..} Exporter {..} = App {..} where
  runApp = importCsv exportCsv

newRegistry :: CliOptions -> Registry _ _
newRegistry cliOptions =
     fun (newImporter @IO)
  <: fun (newExporter @IO)
  <: fun (newApp @IO)
  <: val cliOptions
