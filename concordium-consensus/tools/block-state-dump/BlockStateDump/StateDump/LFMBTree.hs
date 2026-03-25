{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BlockStateDump.StateDump.LFMBTree where

import Control.Monad
import Control.Monad.RWS
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
    String ->
    NodeId ->
    LFMB.LFMBTree' k Blob.HashedBufferedRef v ->
    (NodeId -> v -> StateDumpMonad m ()) ->
    StateDumpMonad m ()
dumpLFMBTree name rootParentNode tree dumpLeaf = do
    -- (treeHash :: h) <- Hash.getHashM tree
    case tree of
        LFMB.Empty -> do
            _rootNode <- buildCompNode (name ++ "{size=0}") rootParentNode Nothing
            return ()
        LFMB.NonEmpty size t -> do
            rootNode <- buildCompNode (name ++ "{size=" ++ show size ++ "}") rootParentNode Nothing
            dumpLFMBT (compNodeBuilder rootNode) 0 t
  where
    dumpLFMBT :: BuildNode m -> Word64 -> LFMB.T Blob.HashedBufferedRef v -> StateDumpMonad m ()
    dumpLFMBT nodeBuilder index = \case
        LFMB.Leaf v -> do
            maybeNode <- nodeBuilder (name ++ "[" ++ show index ++ "]")
            forM_ maybeNode $ \node -> do
                dumpLeaf node v
                return ()
        LFMB.Node height leftRef rightRef -> do
            maybeNode <- nodeBuilder (name ++ "{height=" ++ show height ++ "}")
            forM_ maybeNode $ \node -> do
                (leftBlobRef, leftHash) <- lift $ getHBRRefAndHash leftRef
                left <- lift $ Blob.refLoad leftRef
                dumpLFMBT (blobRefNodeBuilder node "left" leftBlobRef (Just leftHash)) index left
                (rightBlobRef, rightHash) <- lift $ getHBRRefAndHash rightRef
                right <- lift $ Blob.refLoad rightRef
                let rightIndex = index `Bits.setBit` (fromIntegral height)
                dumpLFMBT (blobRefNodeBuilder node "right" rightBlobRef (Just rightHash)) rightIndex right
                return ()
