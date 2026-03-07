{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BlockStateDump.StateDump.LFMBTree where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Bits as Bits
import Data.Word

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Types.HashableTo as Hash

import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMB

import BlockStateDump.Shared

dumpLFMBTree ::
    forall pv m k v.
    ( BS.SupportsPersistentState pv m,
      Hash.MHashableTo m Hash.Hash v,
      Blob.BlobStorable m v
    ) =>
    OutputFiles ->
    String ->
    NodeId ->
    LFMB.LFMBTree' k Blob.HashedBufferedRef v ->
    (NodeId -> v -> m ()) ->
    m ()
dumpLFMBTree output name rootParentNode tree dumpLeaf = do
    -- (treeHash :: h) <- Hash.getHashM tree
    case tree of
        LFMB.Empty -> do
            _rootNode <- liftIO $ buildCompNode output (name ++ "{size=0}") rootParentNode Nothing
            return ()
        LFMB.NonEmpty size t -> do
            rootNode <- liftIO $ buildCompNode output (name ++ "{size=" ++ show size ++ "}") rootParentNode Nothing
            dumpLFMBT (compNodeBuilder output rootNode) 0 t
  where
    dumpLFMBT :: BuildNode -> Word64 -> LFMB.T Blob.HashedBufferedRef v -> m ()
    dumpLFMBT nodeBuilder index = \case
        LFMB.Leaf val -> do
            maybeNode <- liftIO $ nodeBuilder (name ++ "[" ++ show index ++ "]")
            forM_ maybeNode $ \node -> do
                dumpLeaf node val
                return ()
        LFMB.Node height leftRef rightRef -> do
            maybeNode <- liftIO $ nodeBuilder (name ++ "{height=" ++ show height ++ "}")
            forM_ maybeNode $ \node -> do
                (leftBlobRef, leftHash) <- getHBRRefAndHash leftRef
                left <- Blob.refLoad leftRef
                dumpLFMBT (blobRefNodeBuilder output node "left" leftBlobRef leftHash) index left
                (rightBlobRef, rightHash) <- getHBRRefAndHash rightRef
                right <- Blob.refLoad rightRef
                let rightIndex = index `Bits.setBit` (fromIntegral height)
                dumpLFMBT (blobRefNodeBuilder output node "right" rightBlobRef rightHash) rightIndex right
                return ()
