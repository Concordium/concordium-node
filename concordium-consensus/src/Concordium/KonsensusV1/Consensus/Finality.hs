{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.Finality where

import Control.Monad.State
import Lens.Micro.Platform

import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Statistics
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Control.Monad.Catch
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Sequence as Seq

-- |Check if a valid quorum certificate finalizes a block. If so, the block and its ancestors
-- are finalized, the tree is pruned to the decendants of the new last finalized block, and,
-- if applicable, the epoch is advanced.
--
-- PRECONDITION: the quorum certificate is valid and for a live (non-finalized) block. (If the
-- block is not live, this function will not make any change to the state, and will not error.)
checkFinality :: (MonadState (SkovData (MPV m)) m) => QuorumCertificate -> m ()
checkFinality qc = do
    sd <- get
    case getMemoryBlockStatus (qcBlock qc) sd of
        Just (BlockAlive block) -> checkFinalityWithBlock qc block
        _ -> return ()

-- |Check if a valid quorum certificate finalizes a block. If so, the block and its ancestors
-- are finalized, the tree is pruned to the decendants of the new last finalized block, and,
-- if applicable, the epoch is advanced.
--
-- PRECONDITION: the quorum certificate is valid and for the supplied block, which is live
-- (and not finalized).
checkFinalityWithBlock :: (MonadState (SkovData (MPV m)) m) => QuorumCertificate -> BlockPointer (MPV m) -> m ()
checkFinalityWithBlock qc blockPtr
    | NormalBlock block <- bpBlock blockPtr,
      let parentQC = blockQuorumCertificate block,
      qcRound qc == qcRound parentQC + 1,
      qcEpoch qc == qcEpoch parentQC = do
        let newFinalizationEntry =
                FinalizationEntry
                    { feFinalizedQuorumCertificate = parentQC,
                      feSuccessorQuorumCertificate = qc,
                      feSuccessorProof = getHash (sbBlock block)
                    }
        undefined
    | otherwise = return ()

processFinalization ::
    ( MonadState (SkovData (MPV m)) m,
      TimeMonad m,
      MonadIO m,
      LowLevel.MonadTreeStateStore m,
      BlockStateStorage m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      MonadThrow m
    ) =>
    BlockPointer (MPV m) ->
    FinalizationEntry ->
    m ()
processFinalization newFinalizedBlock newFinalizationEntry = do
    -- Update the finalization statistics
    now <- currentTime
    statistics %=! updateStatsOnFinalize now
    -- Check if the focus block is descended from the new finalized block.
    focusBlockSurvives <- isAncestorOf newFinalizedBlock =<< use focusBlock
    -- If not, we shift the focus block to be the new finalized block, which ensures that the focus
    -- block will be a live/finalized block after the finalization and pruning.
    unless focusBlockSurvives $ updateFocusBlockTo newFinalizedBlock

    oldLastFinalized <- use lastFinalized
    let deltaHeight = fromIntegral $ blockHeight newFinalizedBlock - blockHeight oldLastFinalized
    parent <- gets parentOfLive
    oldBranches <- use branches
    let PruneResult{..} = pruneBranches parent newFinalizedBlock deltaHeight oldBranches

    -- Archive the state of the last finalized block and all newly finalized blocks
    -- excluding the new last finalized block.
    mapM_ (archiveBlockState . bpState) (init (oldLastFinalized : prFinalized))
    blockTable . liveMap %=! flip (foldr' (HM.delete . getHash)) prFinalized
    mapM_ (removeTransactions . blockTransactions) prFinalized
    let makeStoredBlock blockPtr = do
            statePointer <- saveBlockState (bpState blockPtr)
            return
                LowLevel.StoredBlock
                    { stbInfo = blockMetadata blockPtr,
                      stbBlock = bpBlock blockPtr,
                      stbStatePointer = statePointer
                    }
    storedBlocks <- mapM makeStoredBlock prFinalized
    LowLevel.writeBlocks storedBlocks newFinalizationEntry

    forM_ prRemoved $ \removedBlock -> do
        markBlockDead (getHash removedBlock)
        purgeBlockState (bpState removedBlock)
        mapM_ (markTransactionDead (getHash removedBlock)) (blockTransactions removedBlock)

    branches .= prNewBranches

-- TODO: purge the pending blocks.

data PruneResult a = PruneResult
    { prRemoved :: [a],
      prFinalized :: [a],
      prNewBranches :: Seq.Seq [a]
    }

pruneBranches ::
    (Eq a) =>
    -- |Parent function
    (a -> a) ->
    -- |Finalization target
    a ->
    -- |Height of the target after the last finalized block
    Int ->
    -- |Existing branches
    Seq.Seq [a] ->
    PruneResult a
pruneBranches parent newFin deltaHeight oldBranches = PruneResult{..}
  where
    (trunk, limbs) = Seq.splitAt deltaHeight oldBranches
    pruneTrunk remove finalize _ Seq.Empty = (remove, finalize)
    pruneTrunk remove finalize keeper (brs Seq.:|> l) =
        pruneTrunk (remove ++ filter (/= keeper) l) (keeper : finalize) (parent keeper) brs
    (removedFromTrunk, prFinalized) = pruneTrunk [] [] newFin trunk
    pruneLimbs remove _ survivors Seq.Empty = (remove, survivors)
    pruneLimbs remove parents survivors (brs Seq.:<| rest) =
        pruneLimbs (remove ++ newRemoved) newSurvivors (survivors Seq.:|> newSurvivors) rest
      where
        (newSurvivors, newRemoved) = List.partition ((`elem` parents) . parent) brs
    (removedFromBranches, prNewBranches) = pruneLimbs [] [newFin] Seq.Empty limbs
    prRemoved = removedFromTrunk ++ removedFromBranches
