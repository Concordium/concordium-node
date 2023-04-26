{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Concordium.KonsensusV1.Scheduler where

import Control.Monad
import qualified Data.Map as Map
import Data.Time
import Lens.Micro.Platform

import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types

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
import Concordium.Types.SeedState

-- * Helper types

-- |Parameters that are used frozen/reset at the start of a payday and used to determine minting and
-- reward distribution. When the block is the first in a new payday, the prologue of the block
-- determines the 'PaydayParameters' for the previous payday, which are subsequently used in the
-- epilogue to distribute rewards.
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
data ParticipatingBakers = ParticipatingBakers
    { -- |The 'BakerId' of the block baker.
      pbBlockBaker :: BakerId,
      -- |The 'BakerId's of the signatories to the block QC.
      pbQCSignatories :: [BakerId]
    }

-- |Input data used for executing a block (besides the transactions).
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
data TransactionRewardParameters = TransactionRewardParameters
    { -- |Total transaction fees for the block.
      trpTransactionFees :: Amount,
      -- |Number of "free" transactions of each type in the block.
      trpFreeTransactionCounts :: FreeTransactionCounts
    }

-- |The outcome of successfully executing a block's transactions.
data TransactionExecutionResult m = TransactionExecutionResult
    { -- |Transaction details used for computing the block reward.
      terTransactionRewardParameters :: TransactionRewardParameters,
      -- |The total energy used in executing the block.
      terEnergyUsed :: Energy,
      -- |The block state after executing the transactions.
      terBlockState :: UpdatableBlockState m
    }

-- TODO: Remove
data FailureReason
    deriving (Eq, Show)

data PrologueResult m = PrologueResult
    { prologueBlockState :: UpdatableBlockState m,
      prologuePaydayParameters :: Maybe PaydayParameters
    }

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
doEpochTransition True epochDuration theState = do
    chainParams <- bsoGetChainParameters theState
    oldSeedState <- bsoGetSeedState theState
    let newEpoch = (oldSeedState ^. epoch) + 1
    nextPayday <- bsoGetPaydayEpoch theState
    (theState, mPaydayParams, newNextPayday) <-
        if newEpoch == nextPayday
            then do
                -- We grab the current payday parameters to use later for minting and distributing
                -- rewards, because we will overwrite them with the parameters for the new payday.
                paydayCapitalDistribution <- bsoGetCurrentCapitalDistribution theState
                paydayBakers <- bsoGetCurrentEpochFullBakersEx theState
                paydayPoolRewards <- bsoGetBakerPoolRewardDetails theState
                paydayMintRate <- bsoGetPaydayMintRate theState
                let paydayParams = PaydayParameters{..}
                theState <- bsoRotateCurrentCapitalDistribution theState
                theState <- bsoRotateCurrentEpochBakers theState
                let timeParams = chainParams ^. cpTimeParameters
                let newPayday = nextPayday + rewardPeriodEpochs (timeParams ^. tpRewardPeriodLength)
                theState <- bsoSetPaydayMintRate theState (timeParams ^. tpMintPerPayday)
                theState <- bsoSetPaydayEpoch theState newPayday
                theState <- bsoProcessPendingChanges theState (<= (oldSeedState ^. triggerBlockTime))
                return (theState, Just paydayParams, newPayday)
            else return (theState, Nothing, nextPayday)
    newBakers <- bsoGetCurrentEpochBakers theState
    let newSeedState = updateSeedStateForEpoch newBakers epochDuration oldSeedState
    theState <- bsoSetSeedState theState newSeedState
    theState <-
        if newEpoch + 1 == newNextPayday
            then do
                -- This is the start of the last epoch of a payday, so take a baker snapshot.
                let epochEnd = newSeedState ^. triggerBlockTime
                (activeBakers, passiveDelegators) <-
                    applyPendingChanges (<= epochEnd)
                        <$> bsoGetActiveBakersAndDelegators theState
                let BakerStakesAndCapital{..} =
                        computeBakerStakesAndCapital
                            (chainParams ^. cpPoolParameters)
                            activeBakers
                            passiveDelegators
                theState <-
                    bsoSetNextEpochBakers
                        theState
                        bakerStakes
                        (chainParams ^. cpFinalizationCommitteeParameters)
                capDist <- capitalDistributionM
                bsoSetNextCapitalDistribution theState capDist
            else return theState
    return (mPaydayParams, theState)

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

executeBlockPrologue ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv
    ) =>
    BlockExecutionData pv ->
    m (PrologueResult m)
executeBlockPrologue BlockExecutionData{..} = do
    theState <- thawBlockState bedParentState
    -- process the update queues
    (updates, theState) <- bsoProcessUpdateQueues theState bedTimestamp
    -- for each pool parameter update, go over all bakers and put their commission rates inside the
    -- new commission ranges.
    activeBakers <- bsoGetActiveBakers theState
    let fitBounds bounds theState (BakerId ai) = bsoConstrainBakerCommission theState ai bounds
        applyCommissionBounds bs (UVPoolParameters PoolParametersV1{..}) =
            foldM (fitBounds _ppCommissionBounds) bs activeBakers
        applyCommissionBounds bs _ = return bs
    theState <- foldM applyCommissionBounds theState updates
    -- unlock the scheduled releases that have expired
    theState <- bsoProcessReleaseSchedule theState bedTimestamp
    (mPaydayParms, theState) <- doEpochTransition bedIsNewEpoch bedEpochDuration theState
    theState <- doUpdateSeedStateForBlock bedTimestamp bedBlockNonce theState
    return
        PrologueResult
            { prologueBlockState = theState,
              prologuePaydayParameters = mPaydayParms
            }

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
doMintingP6 mintRate foundationAddr theState = do
    chainParams <- bsoGetChainParameters theState
    bankStatus <- bsoGetBankStatus theState
    let mintAmounts =
            doCalculatePaydayMintAmounts
                (chainParams ^. rpMintDistribution)
                mintRate
                (bankStatus ^. totalGTU)
    theState <- bsoMint theState mintAmounts
    bsoAddSpecialTransactionOutcome
        theState
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
processPaydayRewards (Just PaydayParameters{..}) theState = do
    -- Foundation rewards are always paid to the current foundation account as of the block
    -- in which the rewards are distributed.
    foundationAddr <- getAccountCanonicalAddress =<< bsoGetFoundationAccount theState
    theState <- doMintingP6 paydayMintRate foundationAddr theState
    distributeRewards foundationAddr paydayCapitalDistribution paydayBakers paydayPoolRewards theState

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
processBlockRewards ParticipatingBakers{..} TransactionRewardParameters{..} theState = do
    theState <- bsoNotifyBlockBaked theState pbBlockBaker
    theState <- bsoMarkFinalizationAwakeBakers theState pbQCSignatories
    doBlockRewardP4 trpTransactionFees trpFreeTransactionCounts pbBlockBaker theState

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
executeBlockEpilogue participants paydayParams transactionRewardParams theState = do
    theState <- processPaydayRewards paydayParams theState
    theState <- processBlockRewards participants transactionRewardParams theState
    freezeBlockState theState

-- |Compute the updated state resulting from executing a block.
-- If block execution fails, the return value is a reason for the failure.
-- FIXME: This currently only really updates the seed state and rotates the bakers.
executeBlockStateUpdate ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv,
      IsProtocolVersion pv
    ) =>
    BlockExecutionData pv ->
    m (Either FailureReason (PBS.HashedPersistentBlockState pv))
executeBlockStateUpdate BlockExecutionData{..} = do
    theState <- thawBlockState bedParentState
    (mPaydayParams, theState) <- doEpochTransition bedIsNewEpoch bedEpochDuration theState
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
    m (FilteredTransactions, TransactionExecutionResult m)
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
    let theState = finState ^. EnvImpl.ssBlockState
    theState <- bsoSetTransactionOutcomes theState (snd <$> ftAdded ft)
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
                  terBlockState = theState
                }
    return (ft, result)
  where
    transactionGroups = groupPendingTransactions transTable pendingTable
    maxBlockSize = fromIntegral (rpBlockSize runtimeParams)

executeBlockTransactions ::
    (BlockStateStorage m, IsConsensusV1 (MPV m), MonadLogger m, MonadProtocolVersion m) =>
    Timestamp ->
    [TVer.BlockItemWithStatus] ->
    UpdatableBlockState m ->
    m (Either (Maybe FailureKind) (TransactionExecutionResult m))
executeBlockTransactions blockTimestamp transactions theState = do
    chainParams <- bsoGetChainParameters theState
    let accountCreationLim = chainParams ^. cpAccountCreationLimit
    let freeCounts = countFreeTransactions (fst <$> transactions) False
    if countAccountCreation freeCounts > accountCreationLim
        then do
            dropUpdatableBlockState theState
            return $ Left $ Just ExceedsMaxCredentialDeployments
        else do
            let context =
                    EnvImpl.ContextState
                        { _chainMetadata = ChainMetadata blockTimestamp,
                          _maxBlockEnergy = chainParams ^. cpConsensusParameters . cpBlockEnergyLimit,
                          _accountCreationLimit = chainParams ^. cpAccountCreationLimit
                        }
            let initState = EnvImpl.makeInitialSchedulerState theState
            (res, finState) <- EnvImpl.runSchedulerT (runTransactions transactions) context initState
            let theState = finState ^. EnvImpl.ssBlockState
            case res of
                Left failKind -> do
                    dropUpdatableBlockState theState
                    return $ Left failKind
                Right outcomes -> do
                    theState <- bsoSetTransactionOutcomes theState (snd <$> outcomes)
                    let result =
                            TransactionExecutionResult
                                { terTransactionRewardParameters =
                                    TransactionRewardParameters
                                        { trpFreeTransactionCounts = freeCounts,
                                          trpTransactionFees = finState ^. EnvImpl.ssExecutionCosts
                                        },
                                  terEnergyUsed = finState ^. EnvImpl.ssEnergyUsed,
                                  terBlockState = theState
                                }
                    return $ Right result

executeBlock ::
    ( pv ~ MPV m,
      BlockStateStorage m,
      BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv,
      MonadLogger m,
      MonadProtocolVersion m
    ) =>
    BlockExecutionData pv ->
    [TVer.BlockItemWithStatus] ->
    m (Either (Maybe FailureKind) (PBS.HashedPersistentBlockState pv))
executeBlock execData@BlockExecutionData{..} transactions = do
    PrologueResult{..} <- executeBlockPrologue execData
    tRes <- executeBlockTransactions bedTimestamp transactions prologueBlockState
    forM tRes $ \TransactionExecutionResult{..} -> do
        executeBlockEpilogue
            bedParticipatingBakers
            prologuePaydayParameters
            terTransactionRewardParameters
            terBlockState

constructBlock ::
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
    m (FilteredTransactions, PBS.HashedPersistentBlockState pv)
constructBlock runtimeParams transactionTable pendingTable execData@BlockExecutionData{..} = do
    PrologueResult{..} <- executeBlockPrologue execData
    (filtered, TransactionExecutionResult{..}) <-
        constructBlockTransactions
            runtimeParams
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
    return (filtered, finalState)
