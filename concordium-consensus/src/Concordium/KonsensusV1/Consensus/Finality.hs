{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.Finality where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Sequence as Seq
import GHC.Stack
import Lens.Micro.Platform

import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Types.SeedState
import Concordium.Utils

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Statistics
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Timeout.Internal
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

-- |Check if a valid quorum certificate finalizes a block. If so, the block and its ancestors
-- are finalized, the tree is pruned to the decendants of the new last finalized block, and,
-- if applicable, the epoch is advanced.
--
-- PRECONDITION: the quorum certificate is valid. (If the target of the QC is not live, this
-- function will not make any change to the state, and will not error.)
checkFinality ::
    ( MonadState (SkovData (MPV m)) m,
      TimeMonad m,
      MonadIO m,
      LowLevel.MonadTreeStateStore m,
      BlockStateStorage m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      MonadThrow m,
      MonadConsensusEvent m,
      MonadLogger m,
      IsConsensusV1 (MPV m)
    ) =>
    QuorumCertificate ->
    m ()
checkFinality qc = do
    sd <- get
    case getMemoryBlockStatus (qcBlock qc) sd of
        Just (BlockAlive block) -> checkFinalityWithBlock qc block
        _ -> return ()

-- |Check if a valid quorum certificate finalizes a block (the parent of the block in the QC).
-- If so, the block and its ancestors are finalized, the tree is pruned to the decendants of the
-- new last finalized block, and, if applicable, the epoch is advanced.
--
-- PRECONDITION: the quorum certificate is valid and for the supplied block, which is live
-- (and not finalized).
checkFinalityWithBlock ::
    ( MonadState (SkovData (MPV m)) m,
      TimeMonad m,
      MonadIO m,
      LowLevel.MonadTreeStateStore m,
      BlockStateStorage m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      MonadThrow m,
      MonadConsensusEvent m,
      MonadLogger m,
      IsConsensusV1 (MPV m),
      HasCallStack
    ) =>
    -- |An already verified 'QuorumCertificate' that points
    -- to the provided @BlockPointer (MPV m)@
    QuorumCertificate ->
    -- |A pointer to the block that is checked whether it can be finalized or not.
    BlockPointer (MPV m) ->
    m ()
checkFinalityWithBlock qc blockPtr
    | NormalBlock block <- bpBlock blockPtr,
      let parentQC = blockQuorumCertificate block,
      qcRound qc == qcRound parentQC + 1,
      qcEpoch qc == qcEpoch parentQC = do
        let finalizedBlockHash = qcBlock parentQC
        sd <- get
        unless (finalizedBlockHash == getHash (sd ^. lastFinalized)) $ do
            let !newFinalizedPtr = parentOfLive sd blockPtr
            let newFinalizationEntry =
                    FinalizationEntry
                        { feFinalizedQuorumCertificate = parentQC,
                          feSuccessorQuorumCertificate = qc,
                          feSuccessorProof = getHash (sbBlock block)
                        }
            processFinalization newFinalizedPtr newFinalizationEntry
            shrinkTimeout blockPtr
    | otherwise = return ()

-- |Process the finalization of a block. The block must be live (not finalized) and the finalization
-- entry must be a valid finalization entry for the block.
--
-- The new finalized block MUST be at most one epoch later than the prior last finalized block.
processFinalization ::
    ( MonadState (SkovData (MPV m)) m,
      TimeMonad m,
      MonadIO m,
      LowLevel.MonadTreeStateStore m,
      BlockStateStorage m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      MonadThrow m,
      MonadConsensusEvent m,
      MonadLogger m,
      IsConsensusV1 (MPV m),
      HasCallStack
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
    -- From the branches, compute the new finalized blocks, the removed blocks and the new updated
    -- branches.
    oldLastFinalized <- use lastFinalized
    let deltaHeight = fromIntegral $ blockHeight newFinalizedBlock - blockHeight oldLastFinalized
    parent <- gets parentOfLive
    oldBranches <- use branches
    let PruneResult{..} = pruneBranches parent newFinalizedBlock deltaHeight oldBranches
    -- Archive the state of the last finalized block and all newly finalized blocks
    -- excluding the new last finalized block.
    mapM_ (archiveBlockState . bpState) (init (oldLastFinalized : prFinalized))
    -- Remove the blocks from the live block table.
    markLiveBlocksFinal prFinalized
    -- Finalize the transactions in the in-memory transaction table.
    mapM_ (finalizeTransactions . blockTransactions) prFinalized
    -- Store the blocks and finalization entry in the low-level tree state database, including
    -- indexing the finalized transactions.
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
    -- Mark the removed blocks as dead, including purging their block states and updating the
    -- transaction table accordingly.
    forM_ prRemoved markLiveBlockDead
    -- Update the branches to reflect the pruning.
    branches .= prNewBranches
    -- Update the last finalized block.
    lastFinalized .= newFinalizedBlock
    -- Update the epoch bakers to reflect the new last finalized block.
    checkedAdvanceEpochBakers oldLastFinalized newFinalizedBlock
    -- Purge the 'roundExistingBlocks' up to the last finalized block.
    purgeRoundExistingBlocks (blockRound newFinalizedBlock)
    -- Purge the 'roundExistingQCs' up to the last finalized block.
    purgeRoundExistingQCs (blockRound newFinalizedBlock)
    -- Purge any pending blocks that are no longer viable.
    purgePending
    -- Advance the epoch if the new finalized block triggers the epoch transition.
    checkedAdvanceEpoch newFinalizationEntry newFinalizedBlock
    onFinalize newFinalizationEntry newFinalizedBlock

-- |Advance the current epoch if the new finalized block indicates that it is necessary.
-- This is deemed to be the case if the following hold:
--
--  * The block is in the current epoch; and
--
--  * The block timestamp is past the epoch transition time, as indicated by the
--    'epochTransitionTriggered' flag in the block's seed state.
--
-- Note: the implementation here relies on not skipping epochs. In particular, when a block is
-- finalized, it must either be in the current epoch or the previous epoch. (The latter can occur
-- if we have seen a QC on a block that justifies finalization of a trigger block, causing us to
-- advance the epoch, but others did not see it and moved on.)
checkedAdvanceEpoch ::
    ( MonadState (SkovData (MPV m)) m,
      IsConsensusV1 (MPV m),
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      BlockStateQuery m
    ) =>
    -- |Finalization entry.
    FinalizationEntry ->
    -- |The block that becomes finalized.
    BlockPointer (MPV m) ->
    m ()
checkedAdvanceEpoch finEntry newFinalizedBlock = do
    oldEpoch <- use currentEpoch
    assert (oldEpoch >= blockEpoch newFinalizedBlock) $
        when (oldEpoch == blockEpoch newFinalizedBlock) $ do
            seedState <- getSeedState finState
            when (seedState ^. epochTransitionTriggered) $ do
                currentEpoch .=! oldEpoch + 1
                lastEpochFinalizationEntry .= Present finEntry
  where
    finState = bpState newFinalizedBlock

-- |Get the computed 'BakersAndFinalizers' for the next epoch from a given block state.
getNextEpochBakersAndFinalizers ::
    ( IsConsensusV1 (MPV m),
      BlockStateQuery m
    ) =>
    GSTypes.BlockState m ->
    m BakersAndFinalizers
getNextEpochBakersAndFinalizers finState = do
    nextFullBakers <- getNextEpochBakers finState
    nextFCParams <- getNextEpochFinalizationCommitteeParameters finState
    return $! computeBakersAndFinalizers nextFullBakers nextFCParams

-- |Update the 'epochBakers' to be relative to the new last finalized block.
-- This only updates the bakers if the new last finalized block is in the next epoch from the
-- previous last finalized block. The new last finalized block MUST be at most one epoch after the
-- previous one.
checkedAdvanceEpochBakers ::
    ( IsConsensusV1 (MPV m),
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      MonadState s m,
      BlockStateQuery m,
      HasEpochBakers s
    ) =>
    -- |The previous last finalized block.
    BlockPointer (MPV m) ->
    -- |The new last finalized block.
    BlockPointer (MPV m) ->
    m ()
checkedAdvanceEpochBakers oldFinalizedBlock newFinalizedBlock
    | newEpoch == oldEpoch + 1 = do
        EpochBakers{..} <- use epochBakers
        -- If the new current epoch is the start of a new payday, we need to update the
        -- next payday epoch.
        newNextPayday <- if _nextPayday == newEpoch then getPaydayEpoch finState else return _nextPayday
        -- If the new next epoch is the start of a new payday, we need to compute the
        -- bakers and finalizers. (If it is not, they are unchanged.)
        newNextBakers <-
            if newNextPayday == newEpoch + 1
                then getNextEpochBakersAndFinalizers finState
                else return _nextEpochBakers
        epochBakers
            .= EpochBakers
                { _previousEpochBakers = _currentEpochBakers,
                  _currentEpochBakers = _nextEpochBakers,
                  _nextEpochBakers = newNextBakers,
                  _nextPayday = newNextPayday
                }
    | otherwise = assert (newEpoch == oldEpoch) $ return ()
  where
    oldEpoch = blockEpoch oldFinalizedBlock
    newEpoch = blockEpoch newFinalizedBlock
    finState = bpState newFinalizedBlock

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

-- |Given a block that has never been live, mark the block as dead.
-- Any pending children will also be marked dead recursively.
blockArriveDead :: (MonadState (SkovData pv) m, MonadLogger m) => BlockHash -> m ()
blockArriveDead blockHsh = do
    logEvent Konsensus LLDebug $ "Block " ++ show blockHsh ++ " arrived dead."
    markBlockDead blockHsh
    children <- takePendingChildren blockHsh
    forM_ children (blockArriveDead . getHash)

-- |Purge pending blocks with timestamps preceding the last finalized block.
purgePending :: (MonadState (SkovData pv) m, MonadLogger m) => m ()
purgePending = do
    lfTimestamp <- use $ lastFinalized . to blockTimestamp
    let purgeLoop =
            takeNextPendingUntil lfTimestamp >>= \case
                Nothing -> return ()
                Just pending -> do
                    let pendingHash = getHash pending
                    blockIsPending <- gets (isPending pendingHash)
                    when blockIsPending $ blockArriveDead pendingHash
                    purgeLoop
    purgeLoop
