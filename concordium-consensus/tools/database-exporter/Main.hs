-- |This tools provides functionality for exporting a node database for use with the out-of-band
-- catch up mechanism.  It also provides functionality for checking that such an exported set of
-- blocks is correctly serialized.
module Main where

import Data.Functor.Identity
import Data.Serialize (runGet)
import Data.Time
import Options.Applicative
import System.Exit

import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.ImportExport
import Concordium.Logger
import Concordium.Types

import DatabaseExporter.CommandLineParser

-- |Check an exported block file.
checkDatabase :: FilePath -> IO ()
checkDatabase filepath = do
    t <- getCurrentTime
    res <- runLoggerT (importBlocksV3 filepath 0 (handleImport t)) logm
    case res of
        Left _ -> putStrLn "Error." >> exitFailure
        Right _ -> putStrLn "Done."
  where
    logm _ lvl s = putStrLn $ show lvl ++ ": " ++ s
    handleImport t (ImportBlock pv _ bs) = case promoteProtocolVersion pv of
        SomeProtocolVersion spv -> case deserializeExactVersionedPendingBlock spv bs t of
            Left _ -> return $ Left ImportSerializationFail
            _ -> return $ Right ()
    handleImport _ (ImportFinalizationRecord _ _ bs) =
        case runGet getExactVersionedFinalizationRecord bs of
            Left _ -> return $ Left ImportSerializationFail
            _ -> return $ Right ()

-- |Export a block database, or check and exported block file, depending on the command line.
main :: IO ()
main = do
    Identity conf <- execParser opts
    case conf of
        Check file -> checkDatabase file
        Export db filepath -> exportDatabaseV3 db filepath
  where
    opts =
        info
            (config <**> helper)
            ( fullDesc
                <> progDesc "Export the database of a consensus node"
            )
