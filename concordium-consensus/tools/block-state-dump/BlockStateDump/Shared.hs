{-# LANGUAGE GADTs #-}
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
import qualified Data.Text.Lazy as Text
import qualified GHC.IORef as IORef
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
    { ofStateGraph :: IO.Handle,
      ofBlocks :: IO.Handle,
      ofState :: IO.Handle,
      ofNextNodeId :: IO.IORef Word64
    }

hashDisplayLength :: Int
hashDisplayLength = 6

buildNode :: OutputFiles -> String -> Hash.Hash -> IO NodeId
buildNode output label hash = do
    let nodeLabel =
            label
                ++ "/"
                ++ take hashDisplayLength (show hash)
    nodeId@(NodeId nodeIdWord) <- NodeId <$> IO.readIORef (ofNextNodeId output)
    IO.writeIORef (ofNextNodeId output) (nodeIdWord + 1)
    IO.hPutStrLn (ofStateGraph output) $
        "    "
            ++ show nodeId
            ++ " [label=\""
            ++ nodeLabel
            ++ "\" ];"
    return nodeId

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

buildNodeWithParent :: OutputFiles -> String -> NodeId -> Hash.Hash -> Blob.BlobRef a -> IO NodeId
buildNodeWithParent output label parent hash blobRef = do
    nodeId <- buildNode output label hash
    buildEdge output label parent nodeId blobRef
    return nodeId

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