{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
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

-- |Ensure that the given certified block is written to the low-level database.
-- Check if the certified block causes its parent to become finalized.
-- If so, the block and its ancestors are finalized, the tree is pruned to the decendants of the
-- new last finalized block, and, if applicable, the epoch is advanced.
-- If the block is already written, then it is assumed that it has already been processed in this
-- manner, and so no further action is taken. (Normally, `processCertifiedBlock` should not be
-- called on a block that is already finalized, but it can happen if a roll-back occurred at
-- start up.)
--
-- This function incorporates the functionality of @checkFinality@ from the bluepaper.
processCertifiedBlock ::
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
    -- |The newly-certified block.
    CertifiedBlock (MPV m) ->
    m ()
processCertifiedBlock cb@CertifiedBlock{..}
    | NormalBlock block <- bpBlock cbQuorumBlock,
      let parentQC = blockQuorumCertificate block,
      qcRound cbQuorumCertificate == qcRound parentQC + 1,
      qcEpoch cbQuorumCertificate == qcEpoch parentQC = unlessStored $ do
        let finalizedBlockHash = qcBlock parentQC
        sd <- get
        if finalizedBlockHash == getHash (sd ^. lastFinalized)
            then do
                -- We do not need to update the last finalized block, but we do need to store this
                -- as a certified block.
                storedBlock <- makeStoredBlock cbQuorumBlock
                LowLevel.writeCertifiedBlock storedBlock cbQuorumCertificate
            else do
                let !newFinalizedPtr = parentOfLive sd cbQuorumBlock
                let newFinalizationEntry =
                        FinalizationEntry
                            { feFinalizedQuorumCertificate = parentQC,
                              feSuccessorQuorumCertificate = cbQuorumCertificate,
                              feSuccessorProof = getHash (sbBlock block)
                            }
                processFinalizationHelper newFinalizedPtr newFinalizationEntry (Just cb)
                shrinkTimeout cbQuorumBlock
    | otherwise = unlessStored $ do
        storedBlock <- makeStoredBlock cbQuorumBlock
        LowLevel.writeCertifiedBlock storedBlock cbQuorumCertificate
  where
    unlessStored a = do
        alreadyStored <- LowLevel.memberBlock (getHash cbQuorumBlock)
        unless alreadyStored a

-- |Process a finalization entry that finalizes a block that is not currently considered finalized.
--
-- PRECONDITION:
--  * The block is live and not already finalized.
--  * The finalization entry is valid.
--  * The block is at most one epoch later than the last finalized block. (This is implied by
--    the block being live.)
processFinalizationEntry ::
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
    -- |Pointer to the block that is finalized.
    BlockPointer (MPV m) ->
    -- |Finalization entry for the block.
    FinalizationEntry ->
    m ()
processFinalizationEntry newFinalizedPtr newFinalizationEntry =
    processFinalizationHelper newFinalizedPtr newFinalizationEntry Nothing

-- |Write a block's state out to the block state database and construct a 'LowLevel.StoredBlock'
-- that can be written to the tree state database.
makeStoredBlock ::
    ( GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      BlockStateStorage m
    ) =>
    BlockPointer (MPV m) ->
    m (LowLevel.StoredBlock (MPV m))
makeStoredBlock blockPtr = do
    statePointer <- saveBlockState (bpState blockPtr)
    return
        LowLevel.StoredBlock
            { stbInfo = blockMetadata blockPtr,
              stbBlock = bpBlock blockPtr,
              stbStatePointer = statePointer
            }

-- |Process the finalization of a block. The block must be live (not finalized) and the finalization
-- entry must be a valid finalization entry for the block.
--
-- The new finalized block MUST be at most one epoch later than the prior last finalized block.
--
-- This optionally takes the certified block following the newly-finalized block as a parameter.
-- If this is provided, the certified block and its QC are written to the tree state database
-- together with updating the finalized block and transaction indexes.
processFinalizationHelper ::
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
    -- |The newly finalized block.
    BlockPointer (MPV m) ->
    -- |Finalization entry for the block.
    FinalizationEntry ->
    -- |Optional newly-certified block to write to the low-level store.
    Maybe (CertifiedBlock (MPV m)) ->
    m ()
{-# INLINE processFinalizationHelper #-}
processFinalizationHelper newFinalizedBlock newFinalizationEntry mCertifiedBlock = do
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
    -- Check if shutdown is triggered by this finalization
    newFinBlockSeedState <- getSeedState $ bpState newFinalizedBlock
    if newFinBlockSeedState ^. shutdownTriggered
        then do
            let oldLastFinalizedState = bpState oldLastFinalized
            lfSeedState <- getSeedState oldLastFinalizedState
            archiveBlockState oldLastFinalizedState
            -- We iterate over the newly finalized blocks and archive all of their states
            -- except for the terminal block and the last finalized block. We also record
            -- which block is the terminal block. The terminal block is the first finalized block
            -- with the 'shutdownTriggered' flag set in the seed state.
            let overNewFinalized False (fin : fins) = do
                    -- Shutdown was not already triggered in the parent.
                    let finState = bpState fin
                    finSeedState <- getSeedState finState
                    if finSeedState ^. shutdownTriggered
                        then do
                            -- Shutdown is triggered now, so record this as the terminal block and
                            -- do not archive it.
                            terminalBlock .=! Present fin
                            logEvent Konsensus LLInfo $
                                "Shutdown triggered in block "
                                    ++ show (getHash @BlockHash newFinalizedBlock)
                                    ++ " (round "
                                    ++ show (theRound $ blockRound newFinalizedBlock)
                                    ++ ") finalized at height "
                                    ++ show (blockHeight newFinalizedBlock)
                            overNewFinalized True fins
                        else unless (null fins) $ do
                            -- Archive the block state since it is not the terminal block or
                            -- the new last finalized block (by the 'unless' check above).
                            archiveBlockState finState
                            overNewFinalized False fins
                overNewFinalized True (fin : fins) = do
                    -- Shutdown is triggered, but the block is not the terminal one, so
                    -- archive the block state unless it is the last finalized block.
                    unless (null fins) $ do
                        archiveBlockState (bpState fin)
                        overNewFinalized True fins
                overNewFinalized _ [] = return ()
            overNewFinalized (lfSeedState ^. shutdownTriggered) prFinalized
        else do
            -- Archive the state of the last finalized block and all newly finalized blocks
            -- excluding the new last finalized block.
            mapM_ (archiveBlockState . bpState) (init (oldLastFinalized : prFinalized))
    -- Remove the blocks from the live block table.
    markLiveBlocksFinal prFinalized
    -- Finalize the transactions in the in-memory transaction table.
    mapM_ (finalizeTransactions . blockTransactions) prFinalized
    -- Store the blocks and finalization entry in the low-level tree state database, including
    -- indexing the finalized transactions.
    -- Store the finalized blocks in the low-level tree state database.
    finalizedBlocks <- mapM makeStoredBlock prFinalized
    case mCertifiedBlock of
        Nothing -> LowLevel.writeFinalizedBlocks finalizedBlocks newFinalizationEntry
        Just certifiedBlock -> do
            storedCertifiedBlock <- makeStoredBlock (cbQuorumBlock certifiedBlock)
            LowLevel.writeCertifiedBlockWithFinalization
                finalizedBlocks
                storedCertifiedBlock
                newFinalizationEntry
    -- Mark the removed blocks as dead, including purging their block states and updating the
    -- transaction table accordingly.
    forM_ prRemoved markLiveBlockDead
    -- Update the branches to reflect the pruning.
    branches .=! prNewBranches
    -- Update the last finalized block.
    lastFinalized .=! newFinalizedBlock
    -- Update the latest finalization entry.
    latestFinalizationEntry .=! Present newFinalizationEntry
    -- Update the epoch bakers to reflect the new last finalized block.
    checkedAdvanceEpochBakers oldLastFinalized newFinalizedBlock
    -- Purge the 'roundExistingBlocks' up to the last finalized block.
    purgeRoundExistingBlocks (blockRound newFinalizedBlock)
    -- Purge the 'roundExistingQCs' before the last finalized block.
    purgeRoundExistingQCs (blockRound newFinalizedBlock)
    -- Purge any pending blocks that are no longer viable.
    purgePending
    -- Advance the epoch if the new finalized block triggers the epoch transition.
    checkedAdvanceEpoch newFinalizationEntry newFinalizedBlock
    -- Log that the blocks are finalized.
    forM_ prFinalized $ \block ->
        logEvent Konsensus LLInfo $
            "Block "
                ++ show (getHash @BlockHash block)
                ++ " (round "
                ++ show (theRound $ blockRound block)
                ++ ") finalized at height "
                ++ show (blockHeight block)
    onFinalize newFinalizationEntry prFinalized

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
    oldEpoch <- use (roundStatus . rsCurrentEpoch)
    assert (oldEpoch >= blockEpoch newFinalizedBlock) $
        when (oldEpoch == blockEpoch newFinalizedBlock) $ do
            seedState <- getSeedState finState
            when (seedState ^. epochTransitionTriggered) $ do
                (roundStatus . rsCurrentEpoch) .=! oldEpoch + 1
                (roundStatus . rsLastEpochFinalizationEntry) .= Present finEntry
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

-- |A result of 'pruneBranches'.
data PruneResult bp = PruneResult
    { -- |Blocks that should be removed as a result of pruning.
      prRemoved :: [bp],
      -- |Blocks that should be marked as finalized as a result of pruning.
      -- Note that the finalized blocks are ordered in ascending order of block height.
      prFinalized :: [bp],
      -- |The updated branches as a result of pruning.
      prNewBranches :: Seq.Seq [bp]
    }

-- |Construct a 'PruneResult' given the existing branches, finalization target and height.
-- This function is written rather abstract as it only relies on the 'Eq' constraint
-- (for the block height) and this makes it easier for testing.
pruneBranches ::
    (Eq blockPointer) =>
    -- |Function for obtaining the parent of a live block.
    -- In practice this is 'parentOfLive' with the correct 'SkovData pv' applied partially.
    (blockPointer -> blockPointer) ->
    -- |Finalization target
    blockPointer ->
    -- |Height of the target after the last finalized block
    Int ->
    -- |Existing branches
    Seq.Seq [blockPointer] ->
    PruneResult blockPointer
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
