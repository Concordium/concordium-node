{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BlockStateDump.DumpState.ProtocolLevelTokens where

import Control.Monad
import Control.Monad.IO.Class

import Concordium.Types
import qualified Concordium.Types.HashableTo as Hash

import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens as PLT
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState as PLT

import BlockStateDump.Shared
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMB

dumpProtocolLevelTokens ::
    forall pv m.
    (BS.SupportsPersistentState pv m) =>
    OutputFiles ->
    NodeId ->
    (PLT.ProtocolLevelTokensForStateVersion (PltStateVersionFor pv)) ->
    m ()
dumpProtocolLevelTokens output parentNode plt = do
    case plt of
        PLT.ProtocolLevelTokensNone -> return ()
        PLT.ProtocolLevelTokensV0 pltStateRef -> do
            (PLT.ProtocolLevelTokensHash _pltStateHash) <- Hash.getHashM pltStateRef -- todo ar why need to call this, why not present?
            (pltStateBlobRef, PLT.ProtocolLevelTokensHash pltStateHash) <- liftIO $ getHBRRefAndHash pltStateRef
            pltStateNodeMaybe <- liftBSOIO $ buildBlobRefNodeWithParentEdge output "plts" parentNode pltStateBlobRef pltStateHash

            forM_ pltStateNodeMaybe $ \pltStateNode -> do
                pltState <- Blob.refLoad pltStateRef
                let pltTable = PLT._pltTable pltState

                return ()

            return ()
        PLT.ProtocolLevelTokensV1 pltState -> do
            return () -- todo ar

dumpLFMBTree ::
    forall pv m k v.
    (BS.SupportsPersistentState pv m) =>
    OutputFiles ->
    NodeId ->
    LFMB.LFMBTree k Blob.HashedBufferedRef v ->
    m ()
dumpLFMBTree output rootParentNode = do
    \case
        LFMB.Empty -> do
            undefined
        LFMB.NonEmpty size t -> do
            dumpLFMBTree rootParentNode t
  where
    dumpLFMBTree :: NodeId -> LFMB.T Blob.HashedBufferedRef (Blob.HashedBufferedRef v) -> m ()
    dumpLFMBTree parentNode = \case
        LFMB.Leaf ref -> do
            undefined
        LFMB.Node height left right -> do
            undefined
