{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation of dumping block state
module BlockStateDump.DumpState (
    dumpState,
) where

import Concordium.Logger
import Concordium.Types
import Control.Monad
import Control.Monad.Reader

import qualified Concordium.GlobalState.AccountMap.LMDB as AccountMap
import qualified Concordium.GlobalState.Persistent.Account as Account
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.KonsensusV1.TreeState.LowLevel as TreeState
import qualified Concordium.KonsensusV1.TreeState.LowLevel.LMDB as TreeState

import BlockStateDump.Util
import qualified Concordium.GlobalState.BlockState as BS
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified Text.Pretty.Simple as Pretty

-- | Dump block state from the node database.
dumpState ::
    forall pv.
    (IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    -- Path to tree state LMDB database, e.g. xyz/database-v4/treestate-0
    FilePath ->
    -- Path to account map LMDB database, e.g. xyz/database-v4/accountmap
    FilePath ->
    -- Path to block state file, e.g. xyz/database-v4/blockstate-0.dat
    FilePath ->
    -- Out dir
    FilePath ->
    -- Start block height
    BlockHeight ->
    -- End block height
    BlockHeight ->
    LogIO ()
dumpState spv treeStateDbPath accountMapDbPath blockStatePath outDir blockStart blockEnd = do
    when (blockEnd < blockStart) $ throwUserError "Block end before block start"
    logEvent External LLInfo $ "Dumping block state from: " ++ blockStatePath ++ " on " ++ show (demoteProtocolVersion spv)
    logEvent External LLInfo $ "Using tree state: " ++ treeStateDbPath

    (treeStateDb :: TreeState.DatabaseHandlers pv) <- liftIO $ TreeState.openDatabase treeStateDbPath
    TreeState.checkDatabaseVersion treeStateDb

    blocks <- forM [blockStart .. blockEnd] $ \blockHeight ->
        runTreeState treeStateDb $ do
            blockMaybe <- TreeState.lookupBlockByHeight blockHeight
            block <-
                maybe
                    (throwUserError "block not found")
                    return
                    blockMaybe
            let statePointer = TreeState.stbStatePointer block

            liftIO $ print $ "Block height: " ++ show blockHeight ++ ", pointer: " ++ show statePointer

            return
                BlockEntry
                    { beBlockHeight = blockHeight,
                      beBlock = block
                    }

    pbscAccountMap <- liftIO $ AccountMap.openDatabase accountMapDbPath

    pbscBlobStore <- liftIO $ Blob.loadBlobStore blockStatePath
    pbscAccountCache <- liftIO $ Account.newAccountCache 1000
    pbscModuleCache <- liftIO $ Modules.newModuleCache 1000
    let (pbsc :: BS.PersistentBlockStateContext pv) = BS.PersistentBlockStateContext{..}

    outputFiles <- liftIO $ openOutputFiles outDir

    runBSO pbsc $ forM_ blocks $ \block@BlockEntry{..} -> do
        bs <- BS.loadBlockState Nothing (TreeState.stbStatePointer beBlock)
        bsp <- BS.loadPBS (BS.hpbsPointers bs)
        dumpBlockState outputFiles block bsp

    liftIO $ closeOutputFiles outputFiles

    return ()

openOutputFiles :: FilePath -> IO OutputFiles
openOutputFiles outDir = do
    Dir.createDirectoryIfMissing True outDir
    ofStateGraph <- IO.openFile (outDir FP.</> "graph.dot") IO.WriteMode
    ofBlocks <- IO.openFile (outDir FP.</> "blocks.txt") IO.WriteMode
    ofState <- IO.openFile (outDir FP.</> "state.txt") IO.WriteMode

    return OutputFiles{..}

closeOutputFiles :: OutputFiles -> IO ()
closeOutputFiles OutputFiles{..} = do
    IO.hClose ofStateGraph
    IO.hClose ofBlocks
    IO.hClose ofState
    return ()

data OutputFiles = OutputFiles
    { ofStateGraph :: IO.Handle,
      ofBlocks :: IO.Handle,
      ofState :: IO.Handle
    }

dumpBlockState ::
    (BS.BlockStateOperations m) =>
    OutputFiles ->
    BlockEntry pv ->
    BS.BlockStatePointers pv ->
    m ()
dumpBlockState output BlockEntry{..} BS.BlockStatePointers{..} = do
    liftBSOIO $ Pretty.pHPrint (ofBlocks output) beBlock
    liftBSOIO $ IO.hPutStrLn (ofBlocks output) ""

    return ()

liftBSOIO ::
    (BS.BlockStateOperations m) =>
    IO a ->
    m a
liftBSOIO m = do
    BS.liftBlobStore $ liftIO m

data BlockEntry pv = BlockEntry
    { beBlockHeight :: BlockHeight,
      beBlock :: TreeState.StoredBlock pv
    }

runTreeState ::
    (MonadIO m) =>
    TreeState.DatabaseHandlers pv ->
    TreeState.DiskLLDBM pv (ReaderT (TreeState.DatabaseHandlers pv) m) a ->
    m a
runTreeState treeStateDb = flip runReaderT treeStateDb . TreeState.runDiskLLDBM

runBSO ::
    BS.PersistentBlockStateContext pv ->
    BS.PersistentBlockStateMonad pv (BS.PersistentBlockStateContext pv) (ReaderT (BS.PersistentBlockStateContext pv) LogIO) a ->
    LogIO a
runBSO pbsc = flip runReaderT pbsc . BS.runPersistentBlockStateMonad