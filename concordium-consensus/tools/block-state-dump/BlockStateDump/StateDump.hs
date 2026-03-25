{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Implementation of dumping block state
module BlockStateDump.StateDump (
    dumpState,
) where

import Control.Monad
import Control.Monad.Reader
import Data.List (intercalate)
import qualified System.IO as IO
import qualified Text.Pretty.Simple as Pretty

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

import BlockStateDump.Shared
import qualified BlockStateDump.StateDump.Accounts as AccountsDump
import qualified BlockStateDump.StateDump.ProtocolLevelTokens as PLTDump
import qualified Control.Monad.RWS as RWST

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
    -- Block heights to dump state for
    [BlockHeight] ->
    LogIO ()
dumpState spv treeStateDbPath accountMapDbPath blockStatePath outDir blockHeights = do
    logEvent External LLInfo $ "Dumping block state from: " ++ blockStatePath ++ " on " ++ show (demoteProtocolVersion spv)
    logEvent External LLInfo $ "Using tree state: " ++ treeStateDbPath

    (treeStateDb :: TreeState.DatabaseHandlers pv) <- liftIO $ TreeState.openDatabase treeStateDbPath
    TreeState.checkDatabaseVersion treeStateDb
        >>= flip unless (throwUserError "tree data database does not have right protocol version")

    blocks <- forM blockHeights $ \blockHeight ->
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

    (outputFilePaths, builderState) <- liftIO $ createBuilderState outDir

    _ <-
        runBSO pbsc $
            RWST.runRWST
                ( do
                    writeGraphStateRaw "digraph G {"
                    writeGraphStateRaw "    ordering=\"out\""
                    -- writeGraphStateRaw "    root [style=invis]"

                    blockNodes <- forM blocks $ \block@BlockEntry{..} -> do
                        bs <- lift $ BS.loadBlockState Nothing (TreeState.stbStatePointer beBlock)
                        lift $ BS.cacheBlockState bs
                        dumpBlockState block bs

                    writeGraphStateRaw $ "    { rank=source; " ++ intercalate "" (flip fmap blockNodes $ \nodeId -> show nodeId ++ ";") ++ " }"

                    writeGraphStateRaw "}"
                    closeOutputFiles
                )
                outputFilePaths
                builderState

    return ()

data BlockEntry pv = BlockEntry
    { beBlock :: TreeState.StoredBlock pv
    }

dumpBlockState ::
    forall pv m.
    (BS.SupportsPersistentState pv m) =>
    BlockEntry (MPV m) ->
    BS.HashedPersistentBlockState (MPV m) ->
    StateDumpMonad m NodeId
dumpBlockState BlockEntry{..} bs = do
    StateDumpBuilderState{..} <- RWST.get
    liftBSDIO $ Pretty.pHPrintNoColor sdbsBlocks beBlock
    liftBSDIO $ IO.hPutStrLn sdbsBlocks ""

    let BlockHash blockHash = Hash.getHash $ TreeState.stbBlock beBlock
    let blockHeight = TreeState.bmHeight $ TreeState.stbInfo beBlock
    blockNode <- buildCompNodeNoEdge ("block[" ++ show blockHeight ++ "]") (Just blockHash)
    maybeStateNode <- buildBlobRefNode blockNode "state" "state" (TreeState.stbStatePointer beBlock) (Just (v0StateHash $ BS.hpbsHash bs))
    let stateNode = maybe (error "state node should always be created") id maybeStateNode

    -- writeGraphStateRaw output $ "    root -> " ++ show blockNode ++ " [style=invis]"

    bsp <- lift $ BS.loadPBS (BS.hpbsPointers bs)
    dumpBlockStatePointers stateNode bsp

    return blockNode
  where
    dumpBlockStatePointers :: NodeId -> BS.BlockStatePointers pv -> StateDumpMonad m ()
    dumpBlockStatePointers parentNode BS.BlockStatePointers{..} = do
        PLTDump.dumpProtocolLevelTokens parentNode bspProtocolLevelTokens
        AccountsDump.dumpAccounts parentNode bspAccounts
        return ()
