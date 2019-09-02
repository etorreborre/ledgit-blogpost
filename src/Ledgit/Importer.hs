{-# LANGUAGE RecordWildCards #-}

module Ledgit.Importer where

import           Control.Monad.Morph
import qualified Data.ByteString.Streaming as S
import           Ledgit.Data
import           Protolude                 hiding (ByteString)
import           Streaming
import           Streaming.Cassava         hiding (header)
import qualified Streaming.Prelude         as P
import           Streaming.With

data Importer m = Importer {
  importCsv :: (Stream (Of LedgerLine) m () -> m ()) -> m ()
}

newImporter :: forall m . (MonadIO m, MonadMask m) => CliOptions -> Importer m
newImporter cliOptions = Importer {..} where

  importCsv :: (Stream (Of LedgerLine) m () -> m ()) -> m ()
  importCsv f = withBinaryFileContents (toS filePath) $ \contents ->
    f (P.map toLedgerLine $ hoist rethrow $ decodeByNameWith (makeDecodeOptions filePath) $ S.filter filterBom $ contents)

  filePath :: Text
  filePath = inputFile cliOptions

filterBom :: Word8 -> Bool
filterBom w = all (\c -> w /= (fromIntegral (ord c))) ['\239', '\187', '\191']

rethrow :: (Exception e, MonadIO m) => ExceptT e m a -> m a
rethrow ma = do
   r <- runExceptT ma
   case r of
     Left e  -> throwIO e
     Right a -> pure a
