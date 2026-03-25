{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module BlockStateDump.Shared where

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.RWS as RWST
import Control.Monad.Reader
import Data.Coerce
import qualified Data.Map.Lazy as Map
import qualified Data.Text.Lazy as Text
import Data.Word
import qualified GHC.IORef as IORef
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified Text.Pretty.Simple as Pretty

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Logger

import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.GlobalState.Persistent.CachedRef
import qualified Concordium.GlobalState.Persistent.CachedRef as Blob
import qualified Concordium.KonsensusV1.TreeState.LowLevel.LMDB as TreeState
import qualified Concordium.Types.HashableTo as Hash

throwUserError :: (MonadIO m) => String -> m a
throwUserError = liftIO . ioError . userError

liftBSOIO ::
    (BS.SupportsPersistentState pv m) =>
    IO a ->
    m a
liftBSOIO m = do
    liftIO m

liftBSDIO ::
    IO a ->
    StateDumpMonad m a
liftBSDIO m = do
    lift $ liftIO m

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

type StateDumpMonad m a = (MonadIO m) => RWST.RWST OutputFilesPaths () StateDumpBuilderState m a

newtype NodeId = NodeId Word64

instance Show NodeId where
    show (NodeId w) = show w

data OutputFilesPaths = OutputFilesPaths
    { ofpStateGraphFilePath :: FilePath,
      ofpStateFilePath :: FilePath,
      ofpBlocksFilePath :: FilePath
    }

data StateDumpBuilderState = StateDumpBuilderState
    { sdbsNextNodeId :: NodeId,
      sdbsBlobRefToNodeId :: Map.Map (Blob.BlobRef ()) (NodeId, Maybe Hash.Hash),
      sdbsStateGraph :: IO.Handle,
      sdbsBlocks :: IO.Handle,
      sdbsState :: IO.Handle
    }

createBuilderState :: FilePath -> IO (OutputFilesPaths, StateDumpBuilderState)
createBuilderState outDir = do
    Dir.createDirectoryIfMissing True outDir
    let ofpStateGraphFilePath = outDir FP.</> "graph.dot"
    let ofpBlocksFilePath = outDir FP.</> "blocks.txt"
    let ofpStateFilePath = outDir FP.</> "state.txt"
    sdbsStateGraph <- IO.openFile ofpStateGraphFilePath IO.WriteMode
    sdbsBlocks <- IO.openFile ofpBlocksFilePath IO.WriteMode
    sdbsState <- IO.openFile ofpStateFilePath IO.WriteMode

    return
        ( OutputFilesPaths{..},
          StateDumpBuilderState
            { sdbsNextNodeId = NodeId 0,
              sdbsBlobRefToNodeId = Map.empty,
              ..
            }
        )

flushOutputFiles :: StateDumpMonad m ()
flushOutputFiles = do
    StateDumpBuilderState{..} <- RWST.get
    liftBSDIO $ IO.hFlush sdbsStateGraph
    liftBSDIO $ IO.hFlush sdbsBlocks
    liftBSDIO $ IO.hFlush sdbsState
    return ()

closeOutputFiles :: StateDumpMonad m ()
closeOutputFiles = do
    StateDumpBuilderState{..} <- RWST.get
    liftBSDIO $ IO.hClose sdbsStateGraph
    liftBSDIO $ IO.hClose sdbsBlocks
    liftBSDIO $ IO.hClose sdbsState
    return ()

reopenOutputFiles :: StateDumpMonad m ()
reopenOutputFiles = do
    OutputFilesPaths{..} <- RWST.ask
    s <- RWST.get
    sdbsStateGraph <- liftBSDIO $ IO.openFile ofpStateGraphFilePath IO.AppendMode
    sdbsBlocks <- liftBSDIO $ IO.openFile ofpBlocksFilePath IO.AppendMode
    sdbsState <- liftBSDIO $ IO.openFile ofpStateFilePath IO.AppendMode
    RWST.put s{sdbsStateGraph = sdbsStateGraph, sdbsBlocks = sdbsBlocks, sdbsState = sdbsState}
    return ()

hashDisplayLength :: Int
hashDisplayLength = 6

-- Build node if a node with the given blob ref does not already exist.
-- Returns the (possibly existing) node id, regardless of whether a new node was build,
-- and a boolean indicating if a new node was build.
buildBlobRefNodeNoEdge :: String -> Blob.BlobRef a -> Maybe Hash.Hash -> StateDumpMonad m (NodeId, Bool)
buildBlobRefNodeNoEdge label blobRef maybeHash = do
    s@StateDumpBuilderState{..} <- RWST.get

    case Map.lookup (coerce blobRef) sdbsBlobRefToNodeId of
        Nothing -> do
            let nodeLabel = case maybeHash of
                    Just hash ->
                        (escapeQuotes label)
                            ++ "/"
                            ++ take hashDisplayLength (show hash)
                    Nothing -> (escapeQuotes label)
            liftBSDIO $
                IO.hPutStrLn sdbsStateGraph $
                    "    "
                        ++ show sdbsNextNodeId
                        ++ " [label=\""
                        ++ nodeLabel
                        ++ "\" ];"
            let updatedNextNodeId = (NodeId $ coerce sdbsNextNodeId + 1)
            let updatedBlobRefToNodeId = Map.insert (coerce blobRef) (sdbsNextNodeId, maybeHash) sdbsBlobRefToNodeId

            RWST.put
                s
                    { sdbsNextNodeId = updatedNextNodeId,
                      sdbsBlobRefToNodeId = updatedBlobRefToNodeId
                    }

            return (sdbsNextNodeId, True)
        Just (existingNodeId, existingHash) -> do
            unless (maybeHash == existingHash) $ error $ "hash does not match for blob ref " ++ show blobRef ++ ", existing: " ++ show existingHash ++ ", new hash: " ++ show maybeHash
            return (existingNodeId, False)

buildCompNodeNoEdge :: String -> Maybe Hash.Hash -> StateDumpMonad m NodeId
buildCompNodeNoEdge label maybeHash = do
    let nodeLabel = case maybeHash of
            Just hash ->
                (escapeQuotes label)
                    ++ "/"
                    ++ take hashDisplayLength (show hash)
            Nothing -> (escapeQuotes label)

    s@StateDumpBuilderState{..} <- RWST.get

    let updatedNextNodeId = (NodeId $ coerce sdbsNextNodeId + 1)

    liftBSDIO $
        IO.hPutStrLn sdbsStateGraph $
            "    "
                ++ show sdbsNextNodeId
                ++ " [label=\""
                ++ nodeLabel
                ++ "\" ];"

    RWST.put s{sdbsNextNodeId = updatedNextNodeId}

    return sdbsNextNodeId

escapeQuotes :: String -> String
escapeQuotes =
    concatMap
        ( \case
            '"' -> "\\\""
            c -> [c]
        )

-- Build edge between two nodes.
buildBlobRefEdge_ :: String -> NodeId -> NodeId -> Blob.BlobRef a -> StateDumpMonad m ()
buildBlobRefEdge_ label source target blobRef = do
    let edgeLabel = (escapeQuotes label) ++ show blobRef
    StateDumpBuilderState{..} <- RWST.get
    liftBSDIO $
        IO.hPutStrLn sdbsStateGraph $
            "    "
                ++ show source
                ++ " -> "
                ++ show target
                ++ " [label=\""
                ++ edgeLabel
                ++ "\"];"
    return ()

buildCompEdge_ :: String -> NodeId -> NodeId -> StateDumpMonad m ()
buildCompEdge_ label source target = do
    let edgeLabel = label
    StateDumpBuilderState{..} <- RWST.get
    liftBSDIO $
        IO.hPutStrLn sdbsStateGraph $
            "    "
                ++ show source
                ++ " -> "
                ++ show target
                ++ " [arrowhead=\"none\" label=\""
                ++ edgeLabel
                ++ "\"];"
    return ()

-- Build node and edge to it from the parent. The new node is only build, if no existing node exists with the given
-- blob reference. The edge is build in any case. Returns the node id if a node if a new node was build.
buildBlobRefNode :: (Coercible h Hash.Hash) => NodeId -> String -> String -> Blob.BlobRef a -> Maybe h -> StateDumpMonad m (Maybe NodeId)
buildBlobRefNode parent refLabel label blobRef hash = do
    (nodeId, nodeCreated) <- buildBlobRefNodeNoEdge label blobRef (coerce <$> hash)
    buildBlobRefEdge_ refLabel parent nodeId blobRef
    return $ if nodeCreated then Just nodeId else Nothing

buildCompNode :: String -> NodeId -> Maybe Hash.Hash -> StateDumpMonad m NodeId
buildCompNode label parent hash = do
    nodeId <- buildCompNodeNoEdge label hash
    buildCompEdge_ label parent nodeId
    return nodeId

blobRefNodeBuilder :: NodeId -> String -> Blob.BlobRef a -> Maybe Hash.Hash -> BuildNode m
blobRefNodeBuilder parent refLabel blobRef hash = \label ->
    buildBlobRefNode parent refLabel label blobRef hash

compNodeBuilder :: NodeId -> BuildNode m
compNodeBuilder parent = \label ->
    Just <$> buildCompNode label parent Nothing

type BuildNode m = String -> StateDumpMonad m (Maybe NodeId)

visitHBRNode ::
    forall pv m h a.
    ( BS.SupportsPersistentState pv m,
      Hash.MHashableTo m h (Blob.HashedBufferedRef' h a),
      (Coercible h Hash.Hash)
    ) =>
    NodeId -> String -> String -> Blob.HashedBufferedRef' h a -> (NodeId -> Blob.BlobRef a -> h -> StateDumpMonad m ()) -> StateDumpMonad m ()
visitHBRNode parent refLabel label hbr build = do
    (blobRef, hash) <- lift $ getHBRRefAndHash hbr
    maybeNode <- buildBlobRefNode parent refLabel label blobRef (Just hash)
    forM_ maybeNode $ \node -> build node blobRef hash

visitHCRNode ::
    forall pv m h a c.
    ( BS.SupportsPersistentState pv m,
      Hash.MHashableTo m h (Blob.HashedCachedRef' h c a),
      Coercible h Hash.Hash
    ) =>
    NodeId -> String -> String -> Blob.HashedCachedRef' h c a -> (NodeId -> Blob.BlobRef a -> h -> StateDumpMonad m ()) -> StateDumpMonad m ()
visitHCRNode parent refLabel label hbr build = do
    (blobRef, hash) <- lift $ getHCRRefAndHash hbr
    maybeNode <- buildBlobRefNode parent refLabel label blobRef (Just hash)
    forM_ maybeNode $ \node -> build node blobRef hash

visitEBRNode ::
    forall h pv m a.
    ( BS.SupportsPersistentState pv m,
      Hash.HashableTo h a,
      Blob.DirectBlobStorable m a,
      Coercible h Hash.Hash
    ) =>
    NodeId -> String -> String -> Blob.EagerBufferedRef a -> (NodeId -> Blob.BlobRef a -> h -> StateDumpMonad m ()) -> StateDumpMonad m ()
visitEBRNode parent refLabel label ebr build = do
    blobRef <- lift $ getEBRRef ebr
    hash <- Hash.getHash <$> (lift $ Blob.refLoad ebr)
    maybeNode <- buildBlobRefNode parent refLabel label blobRef (Just hash)
    forM_ maybeNode $ \node -> build node blobRef hash

visitEHBRNode ::
    forall pv m h a.
    ( BS.SupportsPersistentState pv m,
      Coercible h Hash.Hash
    ) =>
    NodeId -> String -> String -> Blob.EagerlyHashedBufferedRef' h a -> (NodeId -> Blob.BlobRef a -> h -> StateDumpMonad m ()) -> StateDumpMonad m ()
visitEHBRNode parent refLabel label ehbr build = do
    (blobRef, hash) <- lift $ getHEBRRefAndHash ehbr
    maybeNode <- buildBlobRefNode parent refLabel label blobRef (Just hash)
    forM_ maybeNode $ \node -> build node blobRef hash

buildStateData :: (Show a, Coercible h Hash.Hash, MonadIO m) => Blob.BlobRef a -> h -> a -> StateDumpMonad m ()
buildStateData blobRef hash stateData = do
    StateDumpBuilderState{..} <- RWST.get
    liftBSDIO $
        IO.hPutStrLn sdbsState $
            show blobRef
                ++ "/"
                ++ show (coerce hash :: Hash.Hash)
                ++ ":\n"
                ++ Text.unpack (Pretty.pShowNoColor stateData)
    liftBSDIO $ IO.hPutStrLn sdbsState ""
    return ()

writeGraphStateRaw :: (MonadIO m) => String -> StateDumpMonad m ()
writeGraphStateRaw raw = do
    StateDumpBuilderState{..} <- RWST.get
    liftBSDIO $ IO.hPutStrLn sdbsStateGraph raw

getHBRRefAndHash ::
    forall pv m a h.
    ( BS.SupportsPersistentState pv m,
      Hash.MHashableTo m h (Blob.HashedBufferedRef' h a)
    ) =>
    Blob.HashedBufferedRef' h a -> m (Blob.BlobRef a, h)
getHBRRefAndHash (Blob.HashedBufferedRef br hashIORef) = do
    -- (_hash :: h) <- Hash.getHashM hbr
    hash <- liftIO getHash
    return (getBlobRef, hash)
  where
    getHash = do
        IORef.readIORef hashIORef >>= \case
            Blob.Null -> error "bufferedHash not present"
            Blob.Some hash -> return hash
    getBlobRef = case br of
        Blob.BRBlobbed ref -> ref
        Blob.BRMemory _ _ -> error "BRMemory not expected for HashedBufferedRef"
        Blob.BRBoth ref _ -> ref

getHCRRefAndHash ::
    forall pv m a h c.
    ( BS.SupportsPersistentState pv m,
      Hash.MHashableTo m h (Blob.HashedCachedRef' h c a)
    ) =>
    Blob.HashedCachedRef' h c a -> m (Blob.BlobRef a, h)
getHCRRefAndHash (HCRFlushed blobRef hash) = return (blobRef, hash)
getHCRRefAndHash _ = error "HashedCachedRef not HCRFlushed"

getEBRRef ::
    forall pv m a.
    (BS.SupportsPersistentState pv m) =>
    Blob.EagerBufferedRef a -> m (Blob.BlobRef a)
getEBRRef ebr =
    liftIO $ IORef.readIORef (Blob.ebrIORef ebr)

getURRef ::
    forall pv m a.
    (BS.SupportsPersistentState pv m) =>
    Blob.UnbufferedRef a -> m (Blob.BlobRef a)
getURRef (Blob.URBlobbed ref) = return ref
getURRef _ = error "UnbufferedRef not URBlobbed"

getHEBRRefAndHash ::
    forall pv m a h.
    (BS.SupportsPersistentState pv m) =>
    Blob.EagerlyHashedBufferedRef' h a -> m (Blob.BlobRef a, h)
getHEBRRefAndHash ehbr = do
    ref <- liftIO $ IORef.readIORef (Blob.ebrIORef (Blob.ehbrReference ehbr))
    let hash = Blob.ehbrHash ehbr
    return (ref, hash)
