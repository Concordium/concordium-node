-- |This tools provides functionality for exporting a node database for use with the out-of-band
-- catch up mechanism.  It also provides functionality for checking that such an exported set of
-- blocks is correctly serialized.
module Main where

import Control.Monad
import Data.Functor.Identity
import Data.Serialize (runGet)
import Data.Time
import Options.Applicative
import System.Exit

import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.ImportExport
import qualified Concordium.KonsensusV1.TreeState.Types as SkovV1
import qualified Concordium.KonsensusV1.Types as KonsensusV1
import Concordium.Logger
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Parameters

import DatabaseExporter.CommandLineParser

-- |Check an exported block file.
-- Note that for 'ConsensusV1' we only check the blocks since
-- that is the only thing being exported.
checkDatabase :: FilePath -> IO ()
checkDatabase filepath = do
    logm External LLInfo $ "Checking database: " ++ filepath
    t <- getCurrentTime
    res <- runLoggerT (importBlocksV3 filepath 0 (handleImport t)) logm
    case res of
        Left _ -> putStrLn "Error." >> exitFailure
        Right _ -> putStrLn "Done."
  where
    logm _ lvl s = putStrLn $ show lvl ++ ": " ++ s
    handleImport :: MonadLogger m => UTCTime -> ImportData -> m (ImportResult a ())
    handleImport t (ImportBlock pv gi bs) = case promoteProtocolVersion pv of
        SomeProtocolVersion spv -> case consensusVersionFor spv of
            ConsensusV0 -> case deserializeExactVersionedPendingBlock spv bs t of
                Left _ -> return $ Left ImportSerializationFail
                Right pb -> do
                    logEvent External LLInfo $ "GenesisIndex: " ++ show gi ++ " block: " ++ show (pbHash pb) ++ " slot: " ++ show (blockSlot pb)
                    return $ Right ()
            ConsensusV1 -> case SkovV1.deserializeExactVersionedPendingBlock spv bs t of
                Left _ -> return $ Left ImportSerializationFail
                Right pb -> do
                    logEvent External LLInfo $ "GenesisIndex: " ++ show gi ++ " block: " ++ show ((blockHash . getHash) pb) ++ " round: " ++ show (KonsensusV1.blockRound pb)
                    return $ Right ()
    handleImport _ (ImportFinalizationRecord _ gi bs) =
        case runGet getExactVersionedFinalizationRecord bs of
            Left _ -> return $ Left ImportSerializationFail
            Right fr -> do
                logEvent External LLInfo $ "GenesisIndex: " ++ show gi ++ " finrec for: " ++ show (finalizationBlockPointer fr)
                return $ Right ()

-- |Export a block database, or check and exported block file, depending on the command line.
main :: IO ()
main = do
    Identity conf <- execParser opts
    case conf of
        Check file -> checkDatabase file
        Export db filepath n | n > 0 -> do
            exportError <- exportDatabaseV3 db filepath n
            when exportError $ exitWith $ ExitFailure 1
        Export{} -> do
            putStrLn "Chunk size should be larger than 0"
            exitWith $ ExitFailure 2
  where
    opts =
        info
            (config <**> helper)
            ( fullDesc
                <> progDesc "Export the database of a consensus node"
            )
