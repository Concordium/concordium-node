{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module BlockStateDump.Shared where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.IORef as IO
import Data.Word
import qualified System.IO as IO

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Logger

import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.GlobalState.Persistent.CachedRef
import qualified Concordium.GlobalState.Persistent.CachedRef as Blob
import qualified Concordium.KonsensusV1.TreeState.LowLevel.LMDB as TreeState
import qualified Concordium.Types.HashableTo as Hash
import Control.Monad
import Data.Coerce
import qualified Data.Map.Lazy as Map
import qualified Data.Text.Lazy as Text
import qualified GHC.IORef as IORef
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified Text.Pretty.Simple as Pretty

throwUserError :: (MonadIO m) => String -> m a
throwUserError = liftIO . ioError . userError

liftBSOIO ::
    (BS.SupportsPersistentState pv m) =>
    IO a ->
    m a
liftBSOIO m = do
    liftIO m

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

newtype NodeId = NodeId Word64

instance Show NodeId where
    show (NodeId w) = show w

data OutputFiles = OutputFiles
    { ofMutable :: IO.IORef OutputFilesMutable,
      ofStateGraphFilePath :: FilePath,
      ofStateFilePath :: FilePath,
      ofBlocksFilePath :: FilePath
    }

data OutputFilesMutable = OutputFilesMutable
    { ofNextNodeId :: NodeId,
      ofBlobRefToNodeId :: Map.Map (Blob.BlobRef ()) (NodeId, Maybe Hash.Hash),
      ofStateGraph :: IO.Handle,
      ofBlocks :: IO.Handle,
      ofState :: IO.Handle
    }

openOutputFiles :: FilePath -> IO OutputFiles
openOutputFiles outDir = do
    Dir.createDirectoryIfMissing True outDir
    let ofStateGraphFilePath = outDir FP.</> "graph.dot"
    let ofBlocksFilePath = outDir FP.</> "blocks.txt"
    let ofStateFilePath = outDir FP.</> "state.txt"
    ofStateGraph <- IO.openFile ofStateGraphFilePath IO.WriteMode
    ofBlocks <- IO.openFile ofBlocksFilePath IO.WriteMode
    ofState <- IO.openFile ofStateFilePath IO.WriteMode
    ofMutable <-
        IORef.newIORef
            OutputFilesMutable
                { ofNextNodeId = NodeId 0,
                  ofBlobRefToNodeId = Map.empty,
                  ..
                }

    return OutputFiles{..}

flushOutputFiles :: OutputFiles -> IO ()
flushOutputFiles output = do
    OutputFilesMutable{..} <- liftIO $ IO.readIORef (ofMutable output)
    IO.hFlush ofStateGraph
    IO.hFlush ofBlocks
    IO.hFlush ofState
    return ()

closeOutputFiles :: OutputFiles -> IO ()
closeOutputFiles output = do
    OutputFilesMutable{..} <- liftIO $ IO.readIORef (ofMutable output)
    IO.hClose ofStateGraph
    IO.hClose ofBlocks
    IO.hClose ofState
    return ()

reopenOutputFiles :: OutputFiles -> IO ()
reopenOutputFiles OutputFiles{..} = do
    outputMutable <- liftIO $ IO.readIORef ofMutable
    ofStateGraph <- IO.openFile ofStateGraphFilePath IO.AppendMode
    ofBlocks <- IO.openFile ofBlocksFilePath IO.AppendMode
    ofState <- IO.openFile ofStateFilePath IO.AppendMode
    liftIO $ IO.writeIORef ofMutable outputMutable{ofStateGraph = ofStateGraph, ofBlocks = ofBlocks, ofState = ofState}
    return ()

-- ofStateGraph <- IO.openFile (ofStateGraphFilePath output) IO.WriteMode
-- ofBlocks <- IO.openFile (ofBlocksFilePath output) IO.WriteMode
-- ofState <- IO.openFile (ofStateFilePath output) IO.WriteMode
-- return
--     output
--         { ofStateGraph = ofStateGraph,
--           ofBlocks = ofBlocks,
--           ofState = ofState
--         }

hashDisplayLength :: Int
hashDisplayLength = 6

-- Build node if a node with the given blob ref does not already exist.
-- Returns the (possibly existing) node id, regardless of whether a new node was build,
-- and a boolean indicating if a new node was build.
buildBlobRefNodeNoEdge :: OutputFiles -> String -> Blob.BlobRef a -> Maybe Hash.Hash -> IO (NodeId, Bool)
buildBlobRefNodeNoEdge output label blobRef maybeHash = do
    outputMutable@OutputFilesMutable{..} <- IO.readIORef (ofMutable output)

    case Map.lookup (coerce blobRef) ofBlobRefToNodeId of
        Nothing -> do
            let nodeLabel = case maybeHash of
                    Just hash ->
                        (escapeQuotes label)
                            ++ "/"
                            ++ take hashDisplayLength (show hash)
                    Nothing -> (escapeQuotes label)

            IO.hPutStrLn ofStateGraph $
                "    "
                    ++ show ofNextNodeId
                    ++ " [label=\""
                    ++ nodeLabel
                    ++ "\" ];"

            let updatedNextNodeId = (NodeId $ coerce ofNextNodeId + 1)
            let updatedBlobRefToNodeId = Map.insert (coerce blobRef) (ofNextNodeId, maybeHash) ofBlobRefToNodeId
            IO.writeIORef
                (ofMutable output)
                outputMutable
                    { ofNextNodeId = updatedNextNodeId,
                      ofBlobRefToNodeId = updatedBlobRefToNodeId
                    }

            return (ofNextNodeId, True)
        Just (existingNodeId, existingHash) -> do
            unless (maybeHash == existingHash) $ error $ "hash does not match for blob ref " ++ show blobRef ++ ", existing: " ++ show existingHash ++ ", new hash: " ++ show maybeHash
            return (existingNodeId, False)

buildCompNodeNoEdge :: OutputFiles -> String -> Maybe Hash.Hash -> IO NodeId
buildCompNodeNoEdge output label maybeHash = do
    let nodeLabel = case maybeHash of
            Just hash ->
                (escapeQuotes label)
                    ++ "/"
                    ++ take hashDisplayLength (show hash)
            Nothing -> (escapeQuotes label)

    outputFilesMutable@OutputFilesMutable{..} <- IO.readIORef (ofMutable output)
    let updatedNextNodeId = (NodeId $ coerce ofNextNodeId + 1)

    IO.writeIORef
        (ofMutable output)
        outputFilesMutable
            { ofNextNodeId = updatedNextNodeId
            }

    IO.hPutStrLn ofStateGraph $
        "    "
            ++ show ofNextNodeId
            ++ " [label=\""
            ++ nodeLabel
            ++ "\" ];"

    return ofNextNodeId

escapeQuotes :: String -> String
escapeQuotes =
    concatMap
        ( \case
            '"' -> "\\\""
            c -> [c]
        )

-- Build edge between two nodes.
buildBlobRefEdge_ :: OutputFiles -> String -> NodeId -> NodeId -> Blob.BlobRef a -> IO ()
buildBlobRefEdge_ output label source target blobRef = do
    let edgeLabel = (escapeQuotes label) ++ show blobRef
    OutputFilesMutable{..} <- IO.readIORef (ofMutable output)
    IO.hPutStrLn ofStateGraph $
        "    "
            ++ show source
            ++ " -> "
            ++ show target
            ++ " [label=\""
            ++ edgeLabel
            ++ "\"];"
    return ()

buildCompEdge_ :: OutputFiles -> String -> NodeId -> NodeId -> IO ()
buildCompEdge_ output label source target = do
    let edgeLabel = label
    OutputFilesMutable{..} <- IO.readIORef (ofMutable output)
    IO.hPutStrLn ofStateGraph $
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
buildBlobRefNode :: (Coercible h Hash.Hash) => OutputFiles -> NodeId -> String -> String -> Blob.BlobRef a -> Maybe h -> IO (Maybe NodeId)
buildBlobRefNode output parent refLabel label blobRef hash = do
    (nodeId, nodeCreated) <- buildBlobRefNodeNoEdge output label blobRef (coerce <$> hash)
    buildBlobRefEdge_ output refLabel parent nodeId blobRef
    return $ if nodeCreated then Just nodeId else Nothing

buildCompNode :: OutputFiles -> String -> NodeId -> Maybe Hash.Hash -> IO NodeId
buildCompNode output label parent hash = do
    nodeId <- buildCompNodeNoEdge output label hash
    buildCompEdge_ output label parent nodeId
    return nodeId

blobRefNodeBuilder :: OutputFiles -> NodeId -> String -> Blob.BlobRef a -> Maybe Hash.Hash -> BuildNode
blobRefNodeBuilder output parent refLabel blobRef hash = \label ->
    buildBlobRefNode output parent refLabel label blobRef hash

compNodeBuilder :: OutputFiles -> NodeId -> BuildNode
compNodeBuilder output parent = \label ->
    Just <$> buildCompNode output label parent Nothing

visitHBRNode ::
    forall pv m h a.
    ( BS.SupportsPersistentState pv m,
      Hash.MHashableTo m h (Blob.HashedBufferedRef' h a),
      (Coercible h Hash.Hash)
    ) =>
    OutputFiles -> NodeId -> String -> String -> Blob.HashedBufferedRef' h a -> (NodeId -> Blob.BlobRef a -> h -> m ()) -> m ()
visitHBRNode output parent refLabel label hbr build = do
    (blobRef, hash) <- getHBRRefAndHash hbr
    maybeNode <- liftBSOIO $ buildBlobRefNode output parent refLabel label blobRef (Just hash)
    forM_ maybeNode $ \node -> build node blobRef hash

visitHCRNode ::
    forall pv m h a c.
    ( BS.SupportsPersistentState pv m,
      Hash.MHashableTo m h (Blob.HashedCachedRef' h c a),
      Coercible h Hash.Hash
    ) =>
    OutputFiles -> NodeId -> String -> String -> Blob.HashedCachedRef' h c a -> (NodeId -> Blob.BlobRef a -> h -> m ()) -> m ()
visitHCRNode output parent refLabel label hbr build = do
    (blobRef, hash) <- getHCRRefAndHash hbr
    maybeNode <- liftBSOIO $ buildBlobRefNode output parent refLabel label blobRef (Just hash)
    forM_ maybeNode $ \node -> build node blobRef hash

visitEBRNode ::
    forall h pv m a.
    ( BS.SupportsPersistentState pv m,
      Hash.HashableTo h a,
      Blob.DirectBlobStorable m a,
      Coercible h Hash.Hash
    ) =>
    OutputFiles -> NodeId -> String -> String -> Blob.EagerBufferedRef a -> (NodeId -> Blob.BlobRef a -> h -> m ()) -> m ()
visitEBRNode output parent refLabel label ebr build = do
    blobRef <- getEBRRef ebr
    hash <- Hash.getHash <$> Blob.refLoad ebr
    maybeNode <- liftBSOIO $ buildBlobRefNode output parent refLabel label blobRef (Just hash)
    forM_ maybeNode $ \node -> build node blobRef hash

visitEHBRNode ::
    forall pv m h a c.
    ( BS.SupportsPersistentState pv m,
      Coercible h Hash.Hash
    ) =>
    OutputFiles -> NodeId -> String -> String -> Blob.EagerlyHashedBufferedRef' h a -> (NodeId -> Blob.BlobRef a -> h -> m ()) -> m ()
visitEHBRNode output parent refLabel label ehbr build = do
    (blobRef, hash) <- getHEBRRefAndHash ehbr
    maybeNode <- liftBSOIO $ buildBlobRefNode output parent refLabel label blobRef (Just hash)
    forM_ maybeNode $ \node -> build node blobRef hash

type BuildNode = String -> IO (Maybe NodeId)

buildStateData :: (Show a, Coercible h Hash.Hash, MonadIO m) => OutputFiles -> Blob.BlobRef a -> h -> a -> m ()
buildStateData output blobRef hash stateData = do
    OutputFilesMutable{..} <- liftIO $ IO.readIORef (ofMutable output)
    liftIO $
        IO.hPutStrLn ofState $
            show blobRef
                ++ "/"
                ++ show (coerce hash :: Hash.Hash)
                ++ ":\n"
                ++ Text.unpack (Pretty.pShowNoColor stateData)
    liftIO $ IO.hPutStrLn ofState ""
    return ()

writeGraphStateRaw :: (MonadIO m) => OutputFiles -> String -> m ()
writeGraphStateRaw output raw = do
    OutputFilesMutable{..} <- liftIO $ IO.readIORef (ofMutable output)
    liftIO $ IO.hPutStrLn ofStateGraph raw

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
