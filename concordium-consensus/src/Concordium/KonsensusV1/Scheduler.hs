{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Concordium.KonsensusV1.Scheduler where

import Data.Time
import Lens.Micro.Platform

import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.LeaderElection
import Concordium.Scheduler
import qualified Concordium.Scheduler.EnvironmentImplementation as EnvImpl
import Concordium.Scheduler.Types

-- |Update the state to reflect an epoch transition. This makes the following changes:
--
--  * The current epoch bakers and capital distribution are replaced with the next epoch bakers
--    and capital distribution.
--
--  * The seed state is updated to reflect the epoch transition.
--
-- Note: this does not update the next epoch bakers or capital distribution, which should be done
-- subsequently where necessary. This also does not update the seed state to account for the block
-- nonce, which should also be done subsequently.
doEpochTransition ::
    (BlockStateOperations m, IsConsensusV1 (MPV m)) =>
    -- |Whether the block is the first in a new epoch
    Bool ->
    -- |The epoch duration
    Duration ->
    -- |State to update
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
doEpochTransition False _ theState = return theState
doEpochTransition True epochDuration theState = do
    theState <- bsoRotateCurrentCapitalDistribution theState
    theState <- bsoRotateCurrentEpochBakers theState
    oldSeedState <- bsoGetSeedState theState
    newBakers <- bsoGetCurrentEpochBakers theState
    let newSeedState = updateSeedStateForEpoch newBakers epochDuration oldSeedState
    bsoSetSeedState theState newSeedState

-- |Update the seed state to account for a block.
-- See 'updateSeedStateForBlock' for details of what this entails.
doUpdateSeedStateForBlock ::
    (BlockStateOperations m, IsConsensusV1 (MPV m)) =>
    Timestamp ->
    BlockNonce ->
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
doUpdateSeedStateForBlock blkTimestamp blkNonce theState = do
    oldSeedState <- bsoGetSeedState theState
    let newSeedState = updateSeedStateForBlock blkTimestamp blkNonce oldSeedState
    bsoSetSeedState theState newSeedState

data BlockExecutionData (pv :: ProtocolVersion) = BlockExecutionData
    { bedIsNewEpoch :: Bool,
      bedEpochDuration :: Duration,
      bedTimestamp :: Timestamp,
      bedBlockNonce :: BlockNonce,
      bedParentState :: PBS.HashedPersistentBlockState pv
    }

data FailureReason
    deriving (Eq, Show)

-- |Compute the updated state resulting from executing a block.
-- If block execution fails, the return value is a reason for the failure.
-- FIXME: This currently only really updates the seed state and rotates the bakers.
executeBlockStateUpdate ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv
    ) =>
    BlockExecutionData pv ->
    m (Either FailureReason (PBS.HashedPersistentBlockState pv))
executeBlockStateUpdate BlockExecutionData{..} = do
    theState <- thawBlockState bedParentState
    theState <- doEpochTransition bedIsNewEpoch bedEpochDuration theState
    theState <- doUpdateSeedStateForBlock bedTimestamp bedBlockNonce theState
    -- TODO: Snapshot bakers in last epoch of payday, update chain parameters, process releases, mint and reward, execute transactions, etc.
    res <- freezeBlockState theState
    return $ Right res

constructBlockTransactions ::
    (BlockStateStorage m, IsConsensusV1 (MPV m), TimeMonad m, MonadLogger m, MonadProtocolVersion m) =>
    RuntimeParameters ->
    TransactionTable ->
    PendingTransactionTable ->
    Timestamp ->
    UpdatableBlockState m ->
    m (FilteredTransactions, UpdatableBlockState m, Energy)
constructBlockTransactions runtimeParams transTable pendingTable blockTimestamp theState = do
    startTime <- currentTime
    let timeout = addUTCTime (durationToNominalDiffTime (rpBlockTimeout runtimeParams)) startTime

    chainParams <- bsoGetChainParameters theState
    let context =
            EnvImpl.ContextState
                { _chainMetadata = ChainMetadata blockTimestamp,
                  _maxBlockEnergy = chainParams ^. cpConsensusParameters . cpBlockEnergyLimit,
                  _accountCreationLimit = chainParams ^. cpAccountCreationLimit
                }

    (ft, finState) <-
        EnvImpl.runSchedulerT
            (filterTransactions maxBlockSize timeout transactionGroups)
            context
            (EnvImpl.makeInitialSchedulerState theState)

    return (ft, finState ^. EnvImpl.ssBlockState, finState ^. EnvImpl.ssEnergyUsed)
  where
    transactionGroups = groupPendingTransactions transTable pendingTable
    maxBlockSize = fromIntegral (rpBlockSize runtimeParams)
