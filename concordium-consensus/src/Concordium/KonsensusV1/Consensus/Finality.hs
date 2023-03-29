{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.Finality where

import Control.Monad.Catch
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Lens.Micro.Platform

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
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Control.Exception

-- |Shrink the current timeout duration in response to a successful QC for a round.
-- This updates the timeout to @max timeoutBase (timeoutDecrease * oldTimeout)@, where
-- @timeoutBase@ and @timeoutDecrease@ are taken from the chain parameters of the supplied block.
shrinkTimeout ::
    ( GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      BlockStateQuery m,
      MonadState (SkovData (MPV m)) m
    ) =>
    -- |Block to take the timeout parameters from
    BlockPointer (MPV m) ->
    m ()
shrinkTimeout blockPtr = do
    chainParams <- getChainParameters (bpState blockPtr)
    let timeoutParams = chainParams ^. cpConsensusParameters . cpTimeoutParameters
        updateTimeout cur = max (timeoutParams ^. tpTimeoutBase) grow
          where
            grow =
                Duration . ceiling $
                    toRational (timeoutParams ^. tpTimeoutDecrease) * toRational cur
    currentTimeout %=! updateTimeout

-- |Check if a valid quorum certificate finalizes a block. If so, the block and its ancestors
-- are finalized, the tree is pruned to the decendants of the new last finalized block, and,
-- if applicable, the epoch is advanced.
--
-- PRECONDITION: the quorum certificate is valid and for a live (non-finalized) block. (If the
-- block is not live, this function will not make any change to the state, and will not error.)
checkFinality ::
    ( MonadState (SkovData (MPV m)) m,
      TimeMonad m,
      MonadIO m,
      LowLevel.MonadTreeStateStore m,
      BlockStateStorage m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      MonadThrow m,
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
      IsConsensusV1 (MPV m)
    ) =>
    QuorumCertificate ->
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
      IsConsensusV1 (MPV m)
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
    -- Purge the 'roundExistingBlocks' up to the last finalized block.
    purgeRoundExistingBlocks (blockRound newFinalizedBlock)
    -- Purge any pending blocks that are no longer viable.
    purgePending
    -- Advance the epoch if the new finalized block triggers the epoch transition.
    checkedAdvanceEpoch newFinalizedBlock

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
    ( MonadState s m,
      HasEpochBakers s,
      IsConsensusV1 (MPV m),
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      BlockStateQuery m
    ) =>
    BlockPointer (MPV m) ->
    m ()
checkedAdvanceEpoch newFinalizedBlock = do
    EpochBakers{..} <- use epochBakers
    assert (_currentEpoch >= blockEpoch newFinalizedBlock) $
        when (_currentEpoch == blockEpoch newFinalizedBlock) $ do
            seedState <- getSeedState finState
            when (seedState ^. epochTransitionTriggered) $ do
                let newEpoch = _currentEpoch + 1
                -- If the new current epoch is the start of a new payday, we need to update the
                -- next payday epoch.
                newNextPayday <-
                    if _nextPayday == newEpoch
                        then getPaydayEpoch finState
                        else return _nextPayday
                -- If the new next epoch is the start of a new payday, we need to compute the
                -- bakers and finalizers. (If it is not, they are unchanged.)
                newNextBakers <-
                    if newNextPayday == newEpoch + 1
                        then do
                            nextFullBakers <- getNextEpochBakers finState
                            nextFCParams <- getNextEpochFinalizationCommitteeParameters finState
                            return $! computeBakersAndFinalizers nextFullBakers nextFCParams
                        else return _nextEpochBakers
                epochBakers
                    .= EpochBakers
                        { _currentEpoch = newEpoch,
                          _previousEpochBakers = _currentEpochBakers,
                          _currentEpochBakers = _nextEpochBakers,
                          _nextEpochBakers = newNextBakers,
                          _nextPayday = newNextPayday
                        }
  where
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
blockArriveDead :: MonadState (SkovData pv) m => BlockHash -> m ()
blockArriveDead blockHsh = do
    markBlockDead blockHsh
    children <- takePendingChildren blockHsh
    forM_ children (blockArriveDead . getHash)

-- |Purge pending blocks with timestamps preceding the last finalized block.
purgePending :: (MonadState (SkovData pv) m) => m ()
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
    return ()
