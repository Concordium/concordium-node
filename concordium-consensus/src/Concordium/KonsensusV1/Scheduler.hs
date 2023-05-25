{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Scheduler where

import Control.Monad
import qualified Data.Map as Map
import Data.Time
import Lens.Micro.Platform

import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.SeedState

import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.PoolRewards (BakerPoolRewardDetails)
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CapitalDistribution
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.LeaderElection
import Concordium.Kontrol.Bakers
import Concordium.Scheduler
import qualified Concordium.Scheduler.EnvironmentImplementation as EnvImpl
import Concordium.Scheduler.TreeStateEnvironment (FreeTransactionCounts (countAccountCreation), countFreeTransactions, distributeRewards, doBlockRewardP4, doCalculatePaydayMintAmounts)
import Concordium.Scheduler.Types
import qualified Concordium.TransactionVerification as TVer

-- * Helper types

-- |Parameters that are used frozen/reset at the start of a payday and used to determine minting and
-- reward distribution. When the block is the first in a new payday, the prologue of the block
-- determines the 'PaydayParameters' for the previous payday, which are subsequently used in the
-- epilogue to distribute rewards.
--
-- This is a short-lived datastructure used for parameter passing, hence its fields are lazy.
data PaydayParameters = PaydayParameters
    { -- |The capital distribution among the baker pools.
      paydayCapitalDistribution :: CapitalDistribution,
      -- |The effective stake distribution among the baker pools.
      paydayBakers :: FullBakersEx,
      -- |The rewards accruing to each baker pool.
      paydayPoolRewards :: Map.Map BakerId BakerPoolRewardDetails,
      -- |The mint rate for the payday.
      paydayMintRate :: MintRate
    }

-- |The bakers that participated in the block. Used for determining rewards.
--
-- This is a short-lived datastructure used for parameter passing, hence its fields are lazy.
data ParticipatingBakers = ParticipatingBakers
    { -- |The 'BakerId' of the block baker.
      pbBlockBaker :: BakerId,
      -- |The 'BakerId's of the signatories to the block QC.
      -- No particular ordering is assumed.
      pbQCSignatories :: [BakerId]
    }

-- |Input data used for executing a block (besides the transactions).
--
-- This is a short-lived datastructure used for parameter passing, hence its fields are lazy.
data BlockExecutionData (pv :: ProtocolVersion) = BlockExecutionData
    { -- |Indicates if the block is the first in a new epoch.
      bedIsNewEpoch :: Bool,
      -- |The duration of an epoch. (Obtained from genesis data.)
      bedEpochDuration :: Duration,
      -- |The block timestamp.
      bedTimestamp :: Timestamp,
      -- |The block nonce. Used to update the seed state.
      bedBlockNonce :: BlockNonce,
      -- |The block baker and QC signatories.
      bedParticipatingBakers :: ParticipatingBakers,
      -- |The block state of the parent block.
      bedParentState :: PBS.HashedPersistentBlockState pv
    }

-- |Details of the transactions in a block that are used for computing rewards that accrue to the
-- baker and the reward accounts.
--
-- This is a short-lived datastructure used for parameter passing, hence its fields are lazy.
data TransactionRewardParameters = TransactionRewardParameters
    { -- |Total transaction fees for the block.
      trpTransactionFees :: Amount,
      -- |Number of "free" transactions of each type in the block.
      trpFreeTransactionCounts :: FreeTransactionCounts
    }

-- |The outcome of successfully executing a block's transactions.
--
-- This is a short-lived datastructure used for parameter passing, hence its fields are lazy.
data TransactionExecutionResult m = TransactionExecutionResult
    { -- |Transaction details used for computing the block reward.
      terTransactionRewardParameters :: TransactionRewardParameters,
      -- |The total energy used in executing the block.
      terEnergyUsed :: Energy,
      -- |The block state after executing the transactions.
      terBlockState :: UpdatableBlockState m
    }

-- |The result of executing the prologue.
--
-- This is a short-lived datastructure used for parameter passing, hence its fields are lazy.
data PrologueResult m = PrologueResult
    { -- |The block state after prologue execution.
      prologueBlockState :: UpdatableBlockState m,
      -- |If the block should pay out for a payday, these parameters determine the pay out.
      -- Otherwise, they are 'Nothing'.
      prologuePaydayParameters :: Maybe PaydayParameters
    }

-- * Block prologue

-- |Update the state to reflect an epoch transition.  If the block is not the first in a new epoch
-- then this does nothing.  Otherwise, it makes the following changes:
--
--  * If the new epoch is the first block of a new payday:
--
--      - Captures the 'PaydayParameters' from the state, which are returned.
--
--      - Rotates the capital distribution and epoch bakers so that the values snapshotted at the
--        start of the previous epoch become the current values.
--
--      - Updates the time and mint rate for the next payday, based on the current chain parameters.
--
--      - Process pending cooldowns on bakers and delegators that were set to elapse by the
--        trigger block time for the previous epoch.
--
--  * The seed state is updated to reflect the epoch transition.
--
--  * If the new epoch is the epoch before the next payday, take a snapshot of bakers and
--    delegators, allowing for cooldowns that are set to elapse at by the trigger block time for
--    this epoch.
--
-- Note: If the baker or delegator cooldown period is ever less than the duration of an epoch, then
-- it would be possible to have a baker not in cooldown when the baker snapshot is taken, but be
-- removed when the cooldowns are processed at the payday. This is bad, because the baker/delegator
-- would not have their stake locked while they are baking/delegating. However, this should not be
-- a catastrophic invariant violation.
doEpochTransition ::
    forall m.
    (BlockStateOperations m, IsConsensusV1 (MPV m)) =>
    -- |Whether the block is the first in a new epoch
    Bool ->
    -- |The epoch duration
    Duration ->
    -- |State to update
    UpdatableBlockState m ->
    m (Maybe PaydayParameters, UpdatableBlockState m)
doEpochTransition False _ theState = return (Nothing, theState)
doEpochTransition True epochDuration theState0 = do
    chainParams <- bsoGetChainParameters theState0
    oldSeedState <- bsoGetSeedState theState0
    let newEpoch = (oldSeedState ^. epoch) + 1
    nextPayday <- bsoGetPaydayEpoch theState0
    (theState6, mPaydayParams, newNextPayday) <-
        if newEpoch == nextPayday
            then do
                -- We grab the current payday parameters to use later for minting and distributing
                -- rewards, because we will overwrite them with the parameters for the new payday.
                paydayCapitalDistribution <- bsoGetCurrentCapitalDistribution theState0
                paydayBakers <- bsoGetCurrentEpochFullBakersEx theState0
                paydayPoolRewards <- bsoGetBakerPoolRewardDetails theState0
                paydayMintRate <- bsoGetPaydayMintRate theState0
                let paydayParams = PaydayParameters{..}
                -- Rotate the capital distribution and bakers so that the current values are
                -- replaced by the next values.
                theState1 <- bsoRotateCurrentCapitalDistribution theState0
                theState2 <- bsoRotateCurrentEpochBakers theState1
                -- Set the mint rate and epoch for the next payday.
                let timeParams = chainParams ^. cpTimeParameters
                theState3 <- bsoSetPaydayMintRate theState2 (timeParams ^. tpMintPerPayday)
                let newPayday = nextPayday + rewardPeriodEpochs (timeParams ^. tpRewardPeriodLength)
                theState4 <- bsoSetPaydayEpoch theState3 newPayday
                -- Process bakers and delegators where the cooldown elapsed by the trigger block
                -- time of the previous epoch.
                theState5 <- bsoProcessPendingChanges theState4 (<= oldSeedState ^. triggerBlockTime)
                return (theState5, Just paydayParams, newPayday)
            else return (theState0, Nothing, nextPayday)
    -- Update the seed state.
    newBakers <- bsoGetCurrentEpochBakers theState6
    let newSeedState = updateSeedStateForEpoch newBakers epochDuration oldSeedState
    theState7 <- bsoSetSeedState theState6 newSeedState
    theState9 <-
        if newEpoch + 1 == newNextPayday
            then do
                -- This is the start of the last epoch of a payday, so take a baker snapshot.
                let epochEnd = newSeedState ^. triggerBlockTime
                (activeBakers, passiveDelegators) <-
                    applyPendingChanges (<= epochEnd)
                        <$> bsoGetActiveBakersAndDelegators theState7
                let BakerStakesAndCapital{..} =
                        computeBakerStakesAndCapital
                            (chainParams ^. cpPoolParameters)
                            activeBakers
                            passiveDelegators
                theState8 <-
                    bsoSetNextEpochBakers
                        theState7
                        bakerStakes
                        (chainParams ^. cpFinalizationCommitteeParameters)
                capDist <- capitalDistributionM
                bsoSetNextCapitalDistribution theState8 capDist
            else return theState7
    return (mPaydayParams, theState9)

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

-- |Execute the block prologue. This does the following:
--
--  * Thaw the block state.
--  * Process any chain parameter updates that are effective at or before the timestamp of the block.
--  * If the commission bounds are updated, then constrain the baker commission rates accordingly.
--    (This is done for each commission bound update in sequence).
--  * Unlock scheduled releases that have expired.
--  * Update the seed state and bakers appropriately if transitioning to a new epoch.
--  * Update the seed state to reflect the block nonce.
--
-- Returns the updated state, and, when the block is the first in a new payday, the parameters for
-- paying rewards for the previous payday.
executeBlockPrologue ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv
    ) =>
    BlockExecutionData pv ->
    m (PrologueResult m)
executeBlockPrologue BlockExecutionData{..} = do
    theState0 <- thawBlockState bedParentState
    -- process the update queues
    (updates, theState1) <- bsoProcessUpdateQueues theState0 bedTimestamp
    -- for each pool parameter update, go over all bakers and put their commission rates inside the
    -- new commission ranges.
    activeBakers <- bsoGetActiveBakers theState1
    let fitBounds bounds theState (BakerId ai) = bsoConstrainBakerCommission theState ai bounds
        applyCommissionBounds bs (UVPoolParameters PoolParametersV1{..}) =
            foldM (fitBounds _ppCommissionBounds) bs activeBakers
        applyCommissionBounds bs _ = return bs
    theState2 <- foldM applyCommissionBounds theState1 updates
    -- unlock the scheduled releases that have expired
    theState3 <- bsoProcessReleaseSchedule theState2 bedTimestamp
    -- transition the epoch if necessary
    (mPaydayParms, theState4) <- doEpochTransition bedIsNewEpoch bedEpochDuration theState3
    -- update the seed state using the block time and block nonce
    theState5 <- doUpdateSeedStateForBlock bedTimestamp bedBlockNonce theState4
    return
        PrologueResult
            { prologueBlockState = theState5,
              prologuePaydayParameters = mPaydayParms
            }

-- * Block epilogue

-- |Mint for the payday and record a special transaction outcome for the minting.
-- The amount to mint is determined from the specified mint rate.
-- The mint distribution (how much goes to each reward account) is determined by the current chain
-- parameters.
doMintingP6 ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      IsConsensusV1 pv
    ) =>
    -- |Current mint rate.
    MintRate ->
    -- |Current foundation account address.
    AccountAddress ->
    -- |Block state.
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
doMintingP6 mintRate foundationAddr theState0 = do
    chainParams <- bsoGetChainParameters theState0
    bankStatus <- bsoGetBankStatus theState0
    let mintAmounts =
            doCalculatePaydayMintAmounts
                (chainParams ^. rpMintDistribution)
                mintRate
                (bankStatus ^. totalGTU)
    theState1 <- bsoMint theState0 mintAmounts
    bsoAddSpecialTransactionOutcome
        theState1
        Mint
            { stoMintBakingReward = mintBakingReward mintAmounts,
              stoMintFinalizationReward = mintFinalizationReward mintAmounts,
              stoMintPlatformDevelopmentCharge = mintDevelopmentCharge mintAmounts,
              stoFoundationAccount = foundationAddr
            }

-- |If a payday has elapsed, this mints and distributes rewards for the payday.
processPaydayRewards ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      IsConsensusV1 pv
    ) =>
    Maybe PaydayParameters ->
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
processPaydayRewards Nothing theState = return theState
processPaydayRewards (Just PaydayParameters{..}) theState0 = do
    -- Foundation rewards are always paid to the current foundation account as of the block
    -- in which the rewards are distributed.
    foundationAddr <- getAccountCanonicalAddress =<< bsoGetFoundationAccount theState0
    theState1 <- doMintingP6 paydayMintRate foundationAddr theState0
    distributeRewards foundationAddr paydayCapitalDistribution paydayBakers paydayPoolRewards theState1

-- |Records that the baker baked this block (so it is eligible for baking rewards) and that the
-- finalizers that signed the QC in the block are awake (and eligible for finalizer rewards).
-- Distributes the transaction fees to the appropriate reward accounts.
processBlockRewards ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      IsConsensusV1 pv
    ) =>
    -- |Block baker and QC signatories.
    ParticipatingBakers ->
    -- |Transaction fees and number of "free" transactions.
    TransactionRewardParameters ->
    -- |Block state.
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
processBlockRewards ParticipatingBakers{..} TransactionRewardParameters{..} theState0 = do
    theState1 <- bsoNotifyBlockBaked theState0 pbBlockBaker
    theState2 <- bsoMarkFinalizationAwakeBakers theState1 pbQCSignatories
    doBlockRewardP4 trpTransactionFees trpFreeTransactionCounts pbBlockBaker theState2

-- |Execute the block epilogue. This mints and distributes the rewards for a payday if the block is
-- in a new payday. This also accrues the rewards for the block that will be paid at the next
-- payday.
executeBlockEpilogue ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv
    ) =>
    ParticipatingBakers ->
    Maybe PaydayParameters ->
    TransactionRewardParameters ->
    UpdatableBlockState m ->
    m (PBS.HashedPersistentBlockState pv)
executeBlockEpilogue participants paydayParams transactionRewardParams theState0 = do
    theState1 <- processPaydayRewards paydayParams theState0
    theState2 <- processBlockRewards participants transactionRewardParams theState1
    freezeBlockState theState2

-- * Transactions

-- |Execute transactions for constructing a block. This draws transactions from the transaction
-- table (that are pending according to the pending transaction table) and executes them, selecting
-- only valid transactions for inclusion.  The runtime parameters limit the block size and time to
-- spend on constructing the block.
-- The return value is the 'TransactionExecutionResult', which records the energy used, reward
-- parameters (fees and free transaction counts), and the resulting block state.
--
-- Note that this does not update the transaction table or pending transaction table.
constructBlockTransactions ::
    ( BlockStateStorage m,
      IsConsensusV1 (MPV m),
      TimeMonad m,
      MonadLogger m,
      MonadProtocolVersion m
    ) =>
    RuntimeParameters ->
    -- |Time at start of block construction.
    UTCTime ->
    TransactionTable ->
    PendingTransactionTable ->
    -- |Block timestamp.
    Timestamp ->
    -- |Block state.
    UpdatableBlockState m ->
    m (FilteredTransactions, TransactionExecutionResult m)
constructBlockTransactions runtimeParams startTime transTable pendingTable blockTimestamp theState0 = do
    -- The block energy limit and account creation limit are taken from the current chain parameters.
    chainParams <- bsoGetChainParameters theState0
    let context =
            EnvImpl.ContextState
                { _chainMetadata = ChainMetadata blockTimestamp,
                  _maxBlockEnergy = chainParams ^. cpConsensusParameters . cpBlockEnergyLimit,
                  _accountCreationLimit = chainParams ^. cpAccountCreationLimit
                }
    -- Filter the transactions, executing the valid ones.
    (ft, finState) <-
        EnvImpl.runSchedulerT
            (filterTransactions maxBlockSize timeout transactionGroups)
            context
            (EnvImpl.makeInitialSchedulerState theState0)
    let theState1 = finState ^. EnvImpl.ssBlockState
    -- Record the transaction outcomes.
    theState2 <- bsoSetTransactionOutcomes theState1 (snd <$> ftAdded ft)
    let result =
            TransactionExecutionResult
                { terTransactionRewardParameters =
                    TransactionRewardParameters
                        { trpFreeTransactionCounts =
                            countFreeTransactions (fst . fst <$> ftAdded ft) False,
                          trpTransactionFees =
                            finState ^. EnvImpl.ssExecutionCosts
                        },
                  terEnergyUsed = finState ^. EnvImpl.ssEnergyUsed,
                  terBlockState = theState2
                }
    return (ft, result)
  where
    timeout = addUTCTime (durationToNominalDiffTime (rpBlockTimeout runtimeParams)) startTime
    transactionGroups = groupPendingTransactions transTable pendingTable
    maxBlockSize = fromIntegral (rpBlockSize runtimeParams)

-- |Execute the transactions within a block. If successful, the return value is the
-- 'TransactionExecutionResult', which records the energy used, reward parameters (fees and free
-- transaction counts), and the resulting block state. If unsuccessful, a result of @Left Nothing@
-- indicates that the block energy limit was succeeded, and otherwise a result of @Left (Just fk)@
-- indicates the failure kind of the first failed transaction.
executeBlockTransactions ::
    (BlockStateStorage m, IsConsensusV1 (MPV m), MonadLogger m, MonadProtocolVersion m) =>
    Timestamp ->
    [(BlockItem, TVer.VerificationResult)] ->
    UpdatableBlockState m ->
    m (Either (Maybe FailureKind) (TransactionExecutionResult m))
executeBlockTransactions blockTimestamp transactions theState0 = do
    -- The account creation limit and max block energy are determined by the current chain parameters.
    chainParams <- bsoGetChainParameters theState0
    let accountCreationLim = chainParams ^. cpAccountCreationLimit
    -- The number of free transaction is later used for allocating rewards, but we can also abort
    -- execution if the account creation limit is exceeded here.
    let freeCounts = countFreeTransactions (fst <$> transactions) False
    if countAccountCreation freeCounts > accountCreationLim
        then do
            dropUpdatableBlockState theState0
            return $ Left $ Just ExceedsMaxCredentialDeployments
        else do
            let context =
                    EnvImpl.ContextState
                        { _chainMetadata = ChainMetadata blockTimestamp,
                          _maxBlockEnergy = chainParams ^. cpConsensusParameters . cpBlockEnergyLimit,
                          _accountCreationLimit = accountCreationLim
                        }
            let initState = EnvImpl.makeInitialSchedulerState theState0
            (res, finState) <-
                EnvImpl.runSchedulerT
                    (runTransactions ((_2 %~ Just) <$> transactions))
                    context
                    initState
            let theState1 = finState ^. EnvImpl.ssBlockState
            case res of
                Left failKind -> do
                    dropUpdatableBlockState theState1
                    return $ Left failKind
                Right outcomes -> do
                    theState2 <- bsoSetTransactionOutcomes theState1 (snd <$> outcomes)
                    let result =
                            TransactionExecutionResult
                                { terTransactionRewardParameters =
                                    TransactionRewardParameters
                                        { trpFreeTransactionCounts = freeCounts,
                                          trpTransactionFees = finState ^. EnvImpl.ssExecutionCosts
                                        },
                                  terEnergyUsed = finState ^. EnvImpl.ssEnergyUsed,
                                  terBlockState = theState2
                                }
                    return $ Right result

-- * Blocks

-- |Execute a block, computing the new block state. If successful, the return value is the
-- resulting block state and used energy. If unsuccessful, a result of @Left Nothing@ indicates that
-- the block energy limit was succeeded, and otherwise a result of @Left (Just fk)@ indicates the
-- failure kind of the first failed transaction.
executeBlockState ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv,
      MonadLogger m,
      MonadProtocolVersion m
    ) =>
    BlockExecutionData pv ->
    [(BlockItem, TVer.VerificationResult)] ->
    m (Either (Maybe FailureKind) (PBS.HashedPersistentBlockState pv, Energy))
executeBlockState execData@BlockExecutionData{..} transactions = do
    PrologueResult{..} <- executeBlockPrologue execData
    tRes <- executeBlockTransactions bedTimestamp transactions prologueBlockState
    forM tRes $ \TransactionExecutionResult{..} -> do
        endState <-
            executeBlockEpilogue
                bedParticipatingBakers
                prologuePaydayParameters
                terTransactionRewardParameters
                terBlockState
        return (endState, terEnergyUsed)

-- |Construct a block, computing the new block state. This draws transactions from the transaction
-- table (that are pending according to the pending transaction table) and executes them, selecting
-- only valid transactions for inclusion.  The runtime parameters limit the block size and time to
-- spend on constructing the block.  This returns a 'FilteredTransactions' that indicates which
-- transactions were included in the block, as well as any that were found to be invalid. It also
-- returns the new block state and the energy used by the transactions.
--
-- Note that this does not update the transaction table.
constructBlockState ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv,
      TimeMonad m,
      MonadLogger m,
      MonadProtocolVersion m
    ) =>
    RuntimeParameters ->
    TransactionTable ->
    PendingTransactionTable ->
    BlockExecutionData pv ->
    m (FilteredTransactions, PBS.HashedPersistentBlockState pv, Energy)
constructBlockState runtimeParams transactionTable pendingTable execData@BlockExecutionData{..} = do
    startTime <- currentTime
    PrologueResult{..} <- executeBlockPrologue execData
    (filteredTrs, TransactionExecutionResult{..}) <-
        constructBlockTransactions
            runtimeParams
            startTime
            transactionTable
            pendingTable
            bedTimestamp
            prologueBlockState
    finalState <-
        executeBlockEpilogue
            bedParticipatingBakers
            prologuePaydayParameters
            terTransactionRewardParameters
            terBlockState
    endTime <- currentTime
    logEvent Scheduler LLInfo $ "Constructed a block in " ++ show (diffUTCTime endTime startTime)
    return (filteredTrs, finalState, terEnergyUsed)
