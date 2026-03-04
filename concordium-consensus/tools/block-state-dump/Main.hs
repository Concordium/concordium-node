{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This tool provides functionality for exporting a node database for use with the out-of-band
--  catch up mechanism.  It also provides functionality for checking that such an exported set of
--  blocks is correctly serialized.
module Main where

import Concordium.Logger
import Concordium.Types
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity
import qualified Data.Serialize as S
import Data.Time
import qualified Options.Applicative as Options

import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import qualified Concordium.KonsensusV1.TreeState.LowLevel as TreeState
import qualified Concordium.KonsensusV1.TreeState.LowLevel.LMDB as TreeState
import Concordium.Types.HashableTo
import Concordium.Types.Parameters

import qualified BlockStateDump.Config as Config
import Control.Monad.Reader

-- | Dump block state from the node database.
dumpState ::
    forall pv.
    (IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    -- Path to tree state LMDB database, e.g. xyz/database-v4/treestate-0
    FilePath ->
    -- Path to block state file, e.g. xyz/database-v4/blockstate-0.dat
    FilePath ->
    -- Start block height
    BlockHeight ->
    -- End block height
    BlockHeight ->
    LogIO ()
dumpState pv treeStateDbPath blockStatePath blockStart blockEnd = do
    when (blockEnd < blockStart) $ throwUserError "Block end before block start"
    logEvent External LLInfo $ "Dumping block state from: " ++ blockStatePath ++ " on " ++ show (demoteProtocolVersion pv)
    logEvent External LLInfo $ "Using tree state: " ++ treeStateDbPath

    -- (treeStateDb :: TreeState.DatabaseHandlers pv) <- liftIO $ TreeState.openDatabase treeStateDbPath
    (treeStateDb :: TreeState.DatabaseHandlers pv) <- liftIO $ TreeState.openDatabase treeStateDbPath
    TreeState.checkDatabaseVersion treeStateDb

    forM [blockStart .. blockEnd] $ \blockHeight ->
        runTreeState treeStateDb $ do
            blockMaybe <- TreeState.lookupBlockByHeight blockHeight
            block <-
                maybe
                    (throwUserError "block not found")
                    return
                    blockMaybe
            let statePointer = TreeState.stbStatePointer block

            liftIO $ print $ "Block height: " ++ show blockHeight ++ ", pointer: " ++ show statePointer

            return (blockHeight, statePointer)

    return ()

-- | Dump part of blobstore specified in command
main :: IO ()
main = do
    Identity conf <- Options.execParser opts
    let commandAction = case conf of
            Config.DumpState{..} ->
                case promoteProtocolVersion cProtocolVersion of
                    SomeProtocolVersion spv ->
                        dumpState spv cTreeStateDbPath cBlockStatePath cStartBlockHeight cEndBlockHeight
    runLoggerT commandAction logm
  where
    opts =
        Options.info
            (Config.config Options.<**> Options.helper)
            ( Options.fullDesc
                <> Options.progDesc "Dump blobstore content"
            )
    logm _ lvl s = putStrLn $ show lvl ++ ": " ++ s

throwUserError :: (MonadIO m) => String -> m a
throwUserError = liftIO . ioError . userError

runTreeState ::
    (MonadIO m) =>
    TreeState.DatabaseHandlers pv ->
    TreeState.DiskLLDBM pv (ReaderT (TreeState.DatabaseHandlers pv) m) a ->
    m a
runTreeState treeStateDb = flip runReaderT treeStateDb . TreeState.runDiskLLDBM