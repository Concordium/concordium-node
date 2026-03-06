{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
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
import qualified Concordium.KonsensusV1.TreeState.LowLevel.LMDB as TreeState
import Control.Monad
import Data.Coerce
import qualified Data.Map.Lazy as Map
import qualified Data.Text.Lazy as Text
import qualified GHC.IORef as IORef
import qualified Text.Pretty.Simple as Pretty
import qualified System.Directory as Dir
import qualified System.FilePath as FP


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
    { ofStateGraph :: IO.Handle,
      ofBlocks :: IO.Handle,
      ofState :: IO.Handle,
      ofMutable :: IO.IORef OutputFilesMutable
    }

data OutputFilesMutable = OutputFilesMutable
    { ofNextNodeId :: NodeId,
      ofBlobRefToNodeId :: Map.Map (Blob.BlobRef ()) (NodeId, Hash.Hash)
    }


openOutputFiles :: FilePath -> IO OutputFiles
openOutputFiles outDir = do
    Dir.createDirectoryIfMissing True outDir
    ofStateGraph <- IO.openFile (outDir FP.</> "graph.dot") IO.WriteMode
    ofBlocks <- IO.openFile (outDir FP.</> "blocks.txt") IO.WriteMode
    ofState <- IO.openFile (outDir FP.</> "state.txt") IO.WriteMode
    ofMutable <- IORef.newIORef OutputFilesMutable {
        ofNextNodeId = NodeId 0,
        ofBlobRefToNodeId = Map.empty
    }

    return OutputFiles{..}

closeOutputFiles :: OutputFiles -> IO ()
closeOutputFiles OutputFiles{..} = do
    IO.hClose ofStateGraph
    IO.hClose ofBlocks
    IO.hClose ofState
    return ()


hashDisplayLength :: Int
hashDisplayLength = 6

-- Build node if a node with the given blob ref does not already exist. 
-- Returns the (possibly existing) node id, regardless of whether a new node was build, 
-- and a boolean indicating if a new node was build.
buildNode :: OutputFiles -> String -> Blob.BlobRef a -> Hash.Hash -> IO (NodeId, Bool)
buildNode output label blobRef hash = do
    let nodeLabel =
            label
                ++ "/"
                ++ take hashDisplayLength (show hash)

    OutputFilesMutable{..} <- IO.readIORef (ofMutable output)

    case Map.lookup (coerce blobRef) ofBlobRefToNodeId of
        Nothing -> do
            IO.hPutStrLn (ofStateGraph output) $
                "    "
                    ++ show ofNextNodeId
                    ++ " [label=\""
                    ++ nodeLabel
                    ++ "\" ];"

            let updatedNextNodeId = (NodeId $ coerce ofNextNodeId + 1)
            let updatedBlobRefToNodeId = Map.insert (coerce blobRef) (ofNextNodeId, hash) ofBlobRefToNodeId
            IO.writeIORef
                (ofMutable output)
                OutputFilesMutable
                    { ofNextNodeId = updatedNextNodeId,
                      ofBlobRefToNodeId = updatedBlobRefToNodeId
                    }

            return (ofNextNodeId, True)
        Just (existingNodeId, existingHash) -> do
            unless (hash == existingHash) $ error $ "hash does not match for blob ref " ++ show blobRef ++ ", existing: " ++ show existingHash ++ ", new hash: " ++ show hash
            return (existingNodeId, False)

-- Build edge between two nodes.
buildEdge :: OutputFiles -> String -> NodeId -> NodeId -> Blob.BlobRef a -> IO ()
buildEdge output _label source target blobRef = do
    let edgeLabel = show blobRef
    IO.hPutStrLn (ofStateGraph output) $
        "    "
            ++ show source
            ++ " -> "
            ++ show target
            ++ " [label=\""
            ++ edgeLabel
            ++ "\"];"
    return ()

-- Build node and edge to it from the parent. The new node is only build, if no existing node exists with the given
-- blob reference. The edge is build in any case. Returns the node id if a node if a new node was build.
buildNodeWithParent :: OutputFiles -> String -> NodeId -> Blob.BlobRef a -> Hash.Hash -> IO (Maybe NodeId)
buildNodeWithParent output label parent blobRef hash = do
    (nodeId, nodeCreated) <- buildNode output label blobRef hash
    buildEdge output label parent nodeId blobRef
    return $ if nodeCreated then Just nodeId else Nothing

buildStateData :: (Show a) => OutputFiles -> Blob.BlobRef a -> Hash.Hash -> a -> IO ()
buildStateData output blobRef hash stateData = do
    IO.hPutStrLn (ofStateGraph output) $
        show blobRef
            ++ "/"
            ++ show hash
            ++ ":\n"
            ++ Text.unpack (Pretty.pShow stateData)
    return ()

getHBRRefAndHash :: Blob.HashedBufferedRef' h a -> IO (Blob.BlobRef a, h)
getHBRRefAndHash (Blob.HashedBufferedRef br hashIORef) = do
    hash <- getHash
    return (getBlobRef, hash)
  where
    getHash = do
        IORef.readIORef hashIORef >>= \case
            Blob.Null -> error "bufferedHash not present"
            Blob.Some hash -> return hash
    getBlobRef = case br of
        Blob.BRBlobbed ref -> ref
        Blob.BRMemory _ _ -> error "BRMemory not expected present"
        Blob.BRBoth ref _ -> ref