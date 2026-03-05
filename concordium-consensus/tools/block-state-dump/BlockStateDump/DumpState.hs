{-# LANGUAGE ExistentialQuantification #-}
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

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.GlobalState.AccountMap.LMDB as AccountMap
import qualified Concordium.GlobalState.Persistent.Account as Account
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.KonsensusV1.TreeState.LowLevel as TreeState
import qualified Concordium.KonsensusV1.TreeState.LowLevel.LMDB as TreeState

import BlockStateDump.Util
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.KonsensusV1.TreeState.Types as TreeState
import qualified Concordium.Types.HashableTo as Hash
import qualified Data.IORef as IO
import qualified Data.Text.Lazy as Text
import Data.Word
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

openOutputFiles :: FilePath -> IO OutputFiles
openOutputFiles outDir = do
    Dir.createDirectoryIfMissing True outDir
    ofStateGraph <- IO.openFile (outDir FP.</> "graph.dot") IO.WriteMode
    ofBlocks <- IO.openFile (outDir FP.</> "blocks.txt") IO.WriteMode
    ofState <- IO.openFile (outDir FP.</> "state.txt") IO.WriteMode
    ofNextNodeId <- IO.newIORef 0

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
      ofState :: IO.Handle,
      ofNextNodeId :: IO.IORef Word64
    }

dumpBlockState ::
    -- (BS.BlockStateOperations m) =>
    (BS.SupportsPersistentState pv m) =>
    OutputFiles ->
    BlockEntry (MPV m) ->
    BS.HashedPersistentBlockState (MPV m) ->
    m ()
dumpBlockState output BlockEntry{..} bs = do
    liftBSOIO $ Pretty.pHPrint (ofBlocks output) beBlock
    liftBSOIO $ IO.hPutStrLn (ofBlocks output) ""

    BS.BlockStatePointers{..} <- BS.loadPBS (BS.hpbsPointers bs)

    let BlockHash blockHash = Hash.getHash $ TreeState.stbBlock beBlock
    blockNode <- liftBSOIO $ buildNode output ("block " ++ show (TreeState.bmHeight $ TreeState.stbInfo beBlock)) blockHash
    stateNode <- liftBSOIO $ buildNode output "state" (v0StateHash $ BS.hpbsHash bs)
    liftBSOIO $ buildEdge output blockNode stateNode (TreeState.stbStatePointer beBlock)

    return ()

-- todo ar write to be monadic (inner and outer)?

-- class DumpBuilder where
--     buildNode:: String -> Hash.Hash -> IO NodeId
--     buildEdge:: NodeId -> NodeId -> Blob.BlobRef a -> IO ()
--     buildStateData::(Show a) => Blob.BlobRef a -> Hash.Hash -> a -> IO ()

buildNode :: OutputFiles -> String -> Hash.Hash -> IO NodeId
buildNode output label hash = do
    nodeId@(NodeId nodeIdWord) <- NodeId <$> IO.readIORef (ofNextNodeId output)
    IO.writeIORef (ofNextNodeId output) (nodeIdWord + 1)
    IO.hPutStrLn (ofStateGraph output) $
        "    "
            ++ show nodeId
            ++ " [label=\""
            ++ label
            ++ "/"
            ++ show hash
            ++ "\" ];"
    return nodeId

buildEdge :: OutputFiles -> NodeId -> NodeId -> Blob.BlobRef a -> IO ()
buildEdge output source target blobRef = do
    IO.hPutStrLn (ofStateGraph output) $
        "    "
            ++ show source
            ++ " -> "
            ++ show target
            ++ " [label=\""
            ++ show blobRef
            ++ "\"];"
    return ()

buildStateData :: (Show a) => OutputFiles -> Blob.BlobRef a -> Hash.Hash -> a -> IO ()
buildStateData output blobRef hash stateData = do
    IO.hPutStrLn (ofStateGraph output) $
        show blobRef
            ++ "/"
            ++ show hash
            ++ ":\n"
            ++ Text.unpack (Pretty.pShow stateData)
    return ()

newtype NodeId = NodeId Word64

instance Show NodeId where
    show (NodeId w) = show w

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

liftBSOIO ::
    (BS.SupportsPersistentState pv m) =>
    IO a ->
    m a
liftBSOIO m = do
    liftIO m

data BlockEntry pv = BlockEntry
    { beBlock :: TreeState.StoredBlock pv
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
