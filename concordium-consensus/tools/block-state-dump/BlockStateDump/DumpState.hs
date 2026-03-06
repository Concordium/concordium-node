{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Implementation of dumping block state
module BlockStateDump.DumpState (
    dumpState,
) where

import Control.Monad
import Control.Monad.Reader
import qualified Data.IORef as IO
import qualified Data.Text.Lazy as Text
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified Text.Pretty.Simple as Pretty

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Logger
import Concordium.Types
import qualified Concordium.Types.HashableTo as Hash

import qualified Concordium.GlobalState.AccountMap.LMDB as AccountMap
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.Account as Account
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.KonsensusV1.TreeState.LowLevel as TreeState
import qualified Concordium.KonsensusV1.TreeState.LowLevel.LMDB as TreeState
import qualified Concordium.KonsensusV1.TreeState.Types as TreeState

import qualified BlockStateDump.DumpState.ProtocolLevelTokens as PLT
import BlockStateDump.Shared
import Data.Coerce

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

            -- let statePointer = TreeState.stbStatePointer block
            -- liftIO $ print $ "Block height: " ++ show blockHeight ++ ", pointer: " ++ show statePointer

            return
                BlockEntry
                    { beBlock = block
                    }

    pbscAccountMap <- liftIO $ AccountMap.openDatabase accountMapDbPath

    pbscBlobStore <- liftIO $ Blob.loadBlobStore blockStatePath
    pbscAccountCache <- liftIO $ Account.newAccountCache 1000
    pbscModuleCache <- liftIO $ Modules.newModuleCache 1000
    let (pbsc :: BS.PersistentBlockStateContext pv) = BS.PersistentBlockStateContext{..}

    outputFiles <- liftIO $ openOutputFiles outDir
    liftIO $ IO.hPutStrLn (ofStateGraph outputFiles) "digraph G {"

    runBSO pbsc $ forM_ blocks $ \block@BlockEntry{..} -> do
        bs <- BS.loadBlockState Nothing (TreeState.stbStatePointer beBlock)
        dumpBlockState outputFiles block bs

    liftIO $ IO.hPutStrLn (ofStateGraph outputFiles) "}"
    liftIO $ closeOutputFiles outputFiles

    return ()

-- class DumpBuilder where
--     buildNode:: String -> Hash.Hash -> IO NodeId
--     buildEdge:: NodeId -> NodeId -> Blob.BlobRef a -> IO ()
--     buildStateData::(Show a) => Blob.BlobRef a -> Hash.Hash -> a -> IO ()

-- data GraphDumpBuilder = GraphDumpBuilder {
--     gdbNextNodeId:: Word64,
--     gdbFiles::OutputFiles
-- }

-- instance DumpBuilder GraphDumpBuilder where
--     buildNode label hash =
--     buildEdge:: NodeId -> NodeId -> Blob.BlobRef a -> IO ()
--     buildStateData::(Show a) => Blob.BlobRef a -> Hash.Hash

-- data StateNode = StateNode {
--     snLabel:: String,
--     snHash:: Hash.Hash
-- }

-- data StateEdge = forall a. StateEdge {
--     seBlobRef:: Blob.BlobRef a
-- }

-- liftBSOIO ::
--     (BS.BlockStateOperations m) =>
--     IO a ->
--     m a
-- liftBSOIO m = do
--     BS.liftBlobStore $ liftIO m

data BlockEntry pv = BlockEntry
    { beBlock :: TreeState.StoredBlock pv
    }

dumpBlockState ::
    forall pv m.
    (BS.SupportsPersistentState pv m) =>
    OutputFiles ->
    BlockEntry (MPV m) ->
    BS.HashedPersistentBlockState (MPV m) ->
    m ()
dumpBlockState output BlockEntry{..} bs = do
    liftBSOIO $ Pretty.pHPrint (ofBlocks output) beBlock
    liftBSOIO $ IO.hPutStrLn (ofBlocks output) ""

    let BlockHash blockHash = Hash.getHash $ TreeState.stbBlock beBlock
    let blockHeight = TreeState.bmHeight $ TreeState.stbInfo beBlock
    blockNode <- liftBSOIO $ buildCompNode output ("block[" ++ show blockHeight ++ "]") blockHash
    stateNodeMaybe <- liftBSOIO $ buildBlobRefNodeWithParentEdge output "state" blockNode (TreeState.stbStatePointer beBlock) (v0StateHash $ BS.hpbsHash bs)
    let stateNode = maybe (error "state node should always be created") id stateNodeMaybe

    bsp <- BS.loadPBS (BS.hpbsPointers bs)
    dumpBlockStatePointers stateNode bsp
  where
    dumpBlockStatePointers :: NodeId -> BS.BlockStatePointers pv -> m ()
    dumpBlockStatePointers parentNode BS.BlockStatePointers{..} = do
        PLT.dumpProtocolLevelTokens output parentNode bspProtocolLevelTokens
        return ()
