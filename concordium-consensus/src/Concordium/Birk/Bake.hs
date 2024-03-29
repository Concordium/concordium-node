{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.Birk.Bake (
    BakerIdentity (..),
    BakeResult (..),
    bakerSignPublicKey,
    bakerElectionPublicKey,
    validateBakerKeys,
    BakerMonad (..),
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.Serialize

import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Block hiding (PendingBlock, makePendingBlock)
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState as TS
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.BakerIdentity
import Concordium.Types.HashableTo
import Concordium.Types.SeedState

import Concordium.Afgjort.Finalize
import Concordium.Birk.LeaderElection
import Concordium.Kontrol
import Concordium.Kontrol.Bakers
import Concordium.Kontrol.BestBlock
import Concordium.Kontrol.UpdateLeaderElectionParameters
import Concordium.Skov
import Concordium.Skov.Update (OnSkov, blockArrive, makeFinalizerInfo, onBlock, updateFocusBlockTo)

import Concordium.Scheduler.TreeStateEnvironment (ExecutionResult, ExecutionResult' (..), FinalizerInfo, constructBlock)
import Concordium.Scheduler.Types (FilteredTransactions (..))

import Concordium.Logger
import Concordium.TimeMonad

processTransactions ::
    ( TreeStateMonad m,
      SkovMonad m
    ) =>
    Slot ->
    SeedState (SeedStateVersionFor (MPV m)) ->
    BlockPointerType m ->
    Maybe FinalizerInfo ->
    BakerId ->
    m (FilteredTransactions, ExecutionResult m)
processTransactions slot ss bh mfinInfo bid = do
    -- update the focus block to the parent block (establish invariant needed by constructBlock)
    updateFocusBlockTo bh
    -- at this point we can construct the block. The function 'constructBlock' also
    -- updates the pending table and purges any transactions deemed invalid
    slotTime <- getSlotTimestamp slot
    constructBlock slot slotTime bh bid mfinInfo ss

-- | Check that a baker's keys match the 'BakerInfo'.
validateBakerKeys :: BakerInfo -> BakerIdentity -> Bool
validateBakerKeys BakerInfo{..} ident =
    _bakerElectionVerifyKey == bakerElectionPublicKey ident
        && _bakerSignatureVerifyKey == bakerSignPublicKey ident
        && _bakerAggregationVerifyKey == bakerAggregationPublicKey ident

doBakeForSlot :: forall m. (FinalizationMonad m, SkovMonad m, TreeStateMonad m, MonadIO m, OnSkov m) => BakerIdentity -> Slot -> m (Maybe (BlockPointerType m))
doBakeForSlot ident@BakerIdentity{..} slot = runMaybeT $ do
    -- Do not bake if consensus is shut down
    shutdown <- isShutDown
    guard (not shutdown)
    bb <- bestBlockBefore slot
    guard (blockSlot bb < slot)
    bbState <- blockState bb
    gd <- TS.getGenesisData
    bakers <- getSlotBakers gd bbState slot
    (binfo, lotteryPower) <- MaybeT . return $ lotteryBaker bakers bakerId
    unless (validateBakerKeys binfo ident) $ do
        logEvent Baker LLWarning "Baker keys are incorrect."
        let logMismatch :: (Eq a, Show a) => String -> a -> a -> MaybeT m ()
            logMismatch desc x y = unless (x == y) $ logEvent Baker LLTrace $ desc ++ " mismatch. Expected: " ++ show x ++ "; actual: " ++ show y
        logMismatch "Election verify key" (_bakerElectionVerifyKey binfo) (bakerElectionPublicKey ident)
        logMismatch "Signature verify key" (_bakerSignatureVerifyKey binfo) (bakerSignPublicKey ident)
        logMismatch "Aggregate signature verify key" (_bakerAggregationVerifyKey binfo) bakerAggregationPublicKey
        fail "Baker keys are incorrect."
    oldSeedState <- getSeedState bbState
    let leNonce = computeLeadershipElectionNonce oldSeedState slot
    slotTime <- getSlotTimestamp slot
    elDiff <- getElectionDifficulty bbState slotTime
    electionProof <-
        MaybeT . liftIO $
            leaderElection leNonce elDiff slot bakerElectionKey lotteryPower
    logEvent Baker LLInfo $ "Won lottery in " ++ show slot ++ "(lottery power: " ++ show lotteryPower ++ "; election difficulty: " ++ show elDiff ++ ")"
    let nonce = computeBlockNonce leNonce slot bakerElectionKey
    nfr <- lift (nextFinalizationRecord bb)
    (lastFinal, mfinInfo, finData) <- case nfr of
        Nothing -> (,Nothing,NoFinalizationData) <$> bpLastFinalized bb
        Just (_, finCom, finRec) ->
            resolveBlock (finalizationBlockPointer finRec) >>= \case
                -- It is possible that we have a finalization proof but we
                -- don't actually have the block that was finalized.
                -- Possibly we should not even bake in this situation.
                Nothing -> (,Nothing,NoFinalizationData) <$> bpLastFinalized bb
                Just finBlock -> return (finBlock, Just (makeFinalizerInfo finCom $ finalizationProof finRec), BlockFinalizationData finRec)
    -- possibly add the block nonce in the seed state
    let newSeedState = updateSeedState slot nonce oldSeedState
    -- Results = {_energyUsed, _finalState, _transactionLog}
    (filteredTxs, result) <- lift (processTransactions slot newSeedState bb mfinInfo bakerId)
    logEvent Baker LLInfo $ "Baked block"
    receiveTime <- currentTime
    transactionOutcomesHash <- getTransactionOutcomesHash (_finalState result)
    stateHash <- getStateHash (_finalState result)
    pb <- makePendingBlock bakerSignKey slot (bpHash bb) bakerId electionProof nonce finData (map (fst . fst) (ftAdded filteredTxs)) stateHash transactionOutcomesHash receiveTime
    -- Add the baked block to the tree.
    newbp <- blockArrive pb bb lastFinal result
    -- re-establish invariants in the transaction table/pending table/anf table.
    filterTransactionTables (blockSlot newbp) (getHash newbp) filteredTxs
    -- update the current focus block to the newly created block to maintain invariants.
    putFocusBlock newbp
    logEvent Baker LLInfo $ "Finished bake block " ++ show newbp
    -- notify the finalization routine after the invariants are re-established.
    lift (finalizationBlockArrival newbp)
    -- notify of the baked block.
    lift (onBlock newbp)

    return newbp

-- | Result of attempting to bake a block.
data BakeResult
    = -- | A block was successfully baked, at the given slot. The serialized, versioned block
      --  is returned.
      BakeSuccess !Slot !ByteString
    | -- | We have attempted to bake up to the specified slot; try again after the timestamp.
      BakeWaitUntil !Slot !Timestamp
    | -- | The consensus is shut down, so we cannot bake.
      BakeShutdown

-- | Try to bake for a slot later than the given slot, up to the current slot.
doTryBake ::
    forall m.
    (FinalizationMonad m, SkovMonad m, TreeStateMonad m, MonadIO m, OnSkov m) =>
    -- | Baker identity
    BakerIdentity ->
    -- | Last slot that we attempted to bake for
    Slot ->
    m BakeResult
doTryBake bid lastSlot = unlessShutdown $ do
    lastFinSlot <- getLastFinalizedSlot
    curSlot <- getCurrentSlot
    slotDuration <- gdSlotDuration <$> TS.getGenesisData
    maxBakingDelay <- rpMaxBakingDelay <$> TS.getRuntimeParameters
    -- Determine the maximum number of slots in the past we can bake for.
    let maxDelaySlots = Slot $ durationMillis maxBakingDelay `div` durationMillis slotDuration
    -- Determine the oldest slot that we could bake for given that we can only bake maxDelaySlots
    -- in the past. This avoids underflow. We can never bake for slot 0.
    let oldestViableSlot = if curSlot <= maxDelaySlots then 1 else curSlot - maxDelaySlots
    -- The starting slot is the minimum slot that is at least the oldest viable slot and
    -- greater than both the last finalized slot and lost slot baked for.
    let startSlot = max oldestViableSlot (max lastFinSlot lastSlot + 1)
    -- Try baking from the candidate slot up to the current slot.
    let bakeLoop candidate
            | candidate > curSlot = do
                ts <- getSlotTimestamp (curSlot + 1)
                return $ BakeWaitUntil curSlot ts
            | otherwise =
                doBakeForSlot bid candidate >>= \case
                    Nothing -> bakeLoop (candidate + 1)
                    Just block ->
                        return $
                            BakeSuccess candidate $
                                runPut $
                                    putVersionedBlock (protocolVersion @(MPV m)) block
    bakeLoop startSlot
  where
    unlessShutdown a =
        isShutDown >>= \case
            True -> return BakeShutdown
            False -> a

class (SkovMonad m, FinalizationMonad m) => BakerMonad m where
    -- | Create a block pointer for the given slot.
    --  This function is in charge of accumulating the pending transactions and
    --  credential deployments, construct the block and update the transaction table,
    --  pending transaction table and block table. It will also update the focus block
    --  to the newly created block.
    bakeForSlot :: BakerIdentity -> Slot -> m (Maybe (BlockPointerType m))

    -- | Try to bake for a slot later than the given slot, up to the current slot.
    --  This will never bake for a slot earlier than the last finalized block, or that precedes
    --  the current slot by more than the 'rpMaxBakingDelay' runtime parameter.
    tryBake :: BakerIdentity -> Slot -> m BakeResult

instance
    (FinalizationMonad (SkovT pv h c m), MonadIO m, SkovMonad (SkovT pv h c m), TreeStateMonad (SkovT pv h c m), OnSkov (SkovT pv h c m)) =>
    BakerMonad (SkovT pv h c m)
    where
    bakeForSlot = doBakeForSlot
    tryBake = doTryBake
