{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
-- FIXME: This is to suppress compiler warnings for derived instances of SchedulerMonad.
-- This may be fixed in GHC 9.0.1.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Concordium.Scheduler.TreeStateEnvironment where

import Control.Monad
import Data.Foldable
import qualified Data.Map as Map
import Data.Maybe
import Data.Ratio
import qualified Data.Sequence as Seq
import Data.Time
import qualified Data.Vector as Vec
import Data.Word
import Lens.Micro.Platform

import qualified Concordium.GlobalState.BakerInfo as BI
import Concordium.GlobalState.Basic.BlockState.PoolRewards
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CapitalDistribution
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.TreeState
import Concordium.Kontrol.Bakers
import Concordium.Logger
import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.EnvironmentImplementation as EnvImpl
import Concordium.Scheduler.Types
import Concordium.TimeMonad
import qualified Concordium.TransactionVerification as TVer
import qualified Concordium.Types.Accounts as Types
import Concordium.Types.SeedState
import qualified Concordium.Types.Transactions as Types
import Concordium.Types.UpdateQueues (currentParameters)

-- This hack of ExecutionResult' and ExecutionResult is so that we
-- automatically get the property that if BlockState m = BlockState m'
-- then ExecutionResult m is interchangeable with ExecutionResult m'
data ExecutionResult' s = ExecutionResult
    { _finalState :: !s,
      _energyUsed :: !Energy
    }

type ExecutionResult m = ExecutionResult' (BlockState m)

makeLenses ''ExecutionResult'

-- |Distribute the baking rewards for the last epoch to the bakers of
-- blocks in that epoch. This should be called in the first block of
-- a new epoch. This resets the list of blocks baked in the epoch.
rewardLastEpochBakers ::
    (SupportsTransactionOutcomes (MPV m), BlockStateOperations m, AccountVersionFor (MPV m) ~ 'AccountV0) =>
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
rewardLastEpochBakers bs0 = do
    rewards <- _bankRewardAccounts <$> bsoGetBankStatus bs0
    (totalBlocks, bakerShares) <- bsoGetEpochBlocksBaked bs0
    if totalBlocks == 0
        then -- No blocks, so no reward.
        -- In practice, this should not happen.
            return bs0
        else do
            bs1 <- bsoClearEpochBlocksBaked bs0
            -- There are some blocks, so it will be safe to divide.
            let (perBlock, braRem) = (rewards ^. bakingRewardAccount) `divMod` fromIntegral totalBlocks
            bs2 <- bsoSetRewardAccounts bs1 (rewards & bakingRewardAccount .~ braRem)
            let rewardBaker (m, bs) (bid@(BakerId aid), blockCount) = do
                    let brew = fromIntegral (blockCount :: Word64) * perBlock
                    (macct, bs') <- bsoRewardAccount bs aid brew
                    case macct of
                        Nothing -> error $ "rewardLastEpochBakers: invariant violation: baker account (" ++ show bid ++ ") does not exist"
                        Just acct -> return (Map.insert acct brew m, bs')
            (m, bs3) <- foldM rewardBaker (Map.empty, bs2) bakerShares
            bsoAddSpecialTransactionOutcome bs3 BakingRewards{stoBakerRewards = AccountAmounts m, stoRemainder = braRem}

-- |Determine the amount and distribution of minting.
--
-- * First slot to mint for should be @1 + parent block slot@.
-- * Last slot to mint for should be @new block slot@.
-- * Initial reward parameters should be the parameters as of the parent block.
-- * Updates to the reward parameters should be the queued updates to reward
--   parameters, as of the parent block, with the slot they take effect, in
--   order.
-- * Total GTU should be as of the parent block.
--
-- The calculation mints in periods for each change of reward parameters
-- with the amounts being distributed among the recipients separately
-- for each period.  This is significant, since rounding off occurs with
-- each minting and each distribution, so a comptabile implementation MUST
-- round in the same fashion.
calculateMintAmounts ::
    -- |First slot to mint for
    Slot ->
    -- |Last slot to mint for
    Slot ->
    -- |Initial mint distribution
    MintDistribution 'MintDistributionVersion0 ->
    -- |Ordered updates to the minting parameters
    [(Slot, MintDistribution 'MintDistributionVersion0)] ->
    -- |Total GTU
    Amount ->
    MintAmounts
calculateMintAmounts = go mempty
  where
    go !acc start end md0 upds tGTU = case upds of
        ((s, md1) : upds')
            | s <= start -> go acc start end md1 upds' tGTU
            | s <= end ->
                let (tGTU', a1) = mintRange start (s - 1) md0 tGTU
                in  go (a1 <> acc) s end md1 upds' tGTU'
        _ -> acc <> (snd $ mintRange start end md0 tGTU)

    mintRange s e md t =
        let mintSupply s' !m !t'
                | s' <= e = let !a = mintAmount (md ^. mdMintPerSlot . unconditionally) t' in mintSupply (s' + 1) (m + a) (t' + a)
                | otherwise = (m, t')
            (newMint, newTotal) = mintSupply s 0 t
            mintBakingReward = takeFraction (md ^. mdBakingReward) newMint
            mintFinalizationReward = takeFraction (md ^. mdFinalizationReward) newMint
            mintDevelopmentCharge = newMint - (mintBakingReward + mintFinalizationReward)
        in  (newTotal, MintAmounts{..})

-- |Determine the amount and distribution of minting for one payday.
doCalculatePaydayMintAmounts ::
    -- |Initial mint distribution
    MintDistribution 'MintDistributionVersion1 ->
    -- |Payday mint rate
    MintRate ->
    -- |Total GTU
    Amount ->
    MintAmounts
doCalculatePaydayMintAmounts md mr amt =
    let newMint = mintAmount mr amt
        mintBakingReward = takeFraction (md ^. mdBakingReward) newMint
        mintFinalizationReward = takeFraction (md ^. mdFinalizationReward) newMint
        mintDevelopmentCharge = newMint - (mintBakingReward + mintFinalizationReward)
    in  MintAmounts{..}

-- |Determine the amount and distribution of minting for one payday, taking updates to mint distribution and mint rate into account.
calculatePaydayMintAmounts ::
    (MintDistributionVersionFor cpv ~ 'MintDistributionVersion1) =>
    -- |Initial mint distribution
    MintDistribution 'MintDistributionVersion1 ->
    -- |Payday mint rate
    MintRate ->
    -- |Payday slot
    Slot ->
    -- |Changes to mint distribution or mint rate
    [(Slot, UpdateValue cpv)] ->
    -- |Total GTU
    Amount ->
    MintAmounts
calculatePaydayMintAmounts md mr ps updates amt =
    let selectBestSlotMintDistribution (s2, md2) (s1, UVMintDistribution md1) =
            if s1 >= s2 && s1 <= ps then (s1, md1) else (s2, md2)
        selectBestSlotMintDistribution a _ = a
        newMintDistribution = snd $ foldl' selectBestSlotMintDistribution (0, md) updates
    in  doCalculatePaydayMintAmounts newMintDistribution mr amt

-- |Mint for all slots since the last block, recording a
-- special transaction outcome for the minting.
doMinting ::
    (ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0, BlockStateOperations m, BlockPointerMonad m, SupportsTransactionOutcomes (MPV m)) =>
    -- |Parent block
    BlockPointerType m ->
    -- |New slot
    Slot ->
    -- |Current foundation account
    AccountAddress ->
    -- |Ordered updates to the minting parameters
    [(Slot, MintDistribution 'MintDistributionVersion0)] ->
    -- |Block state
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
doMinting blockParent slotNumber foundationAddr mintUpds bs0 = do
    parentUpdates <- getUpdates =<< blockState blockParent
    totGTU <- (^. totalGTU) <$> bsoGetBankStatus bs0
    let mint =
            calculateMintAmounts
                (blockSlot blockParent + 1)
                slotNumber
                (parentUpdates ^. currentParameters . rpMintDistribution)
                mintUpds
                totGTU
    bs1 <- bsoMint bs0 mint
    bsoAddSpecialTransactionOutcome
        bs1
        Mint
            { stoMintBakingReward = mintBakingReward mint,
              stoMintFinalizationReward = mintFinalizationReward mint,
              stoMintPlatformDevelopmentCharge = mintDevelopmentCharge mint,
              stoFoundationAccount = foundationAddr
            }

-- |Mint for the given payday, recording a
-- special transaction outcome for the minting.
doMintingP4 ::
    ( ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      BlockStateOperations m,
      SupportsTransactionOutcomes (MPV m),
      SeedStateVersionFor (MPV m) ~ 'SeedStateVersion0
    ) =>
    -- |Chain parameters
    ChainParameters (MPV m) ->
    -- |Payday epoch to mint for
    Epoch ->
    -- |Payday mint rate
    MintRate ->
    -- |Current foundation account
    AccountAddress ->
    -- |Ordered updates to the minting parameters
    [(Slot, UpdateValue 'ChainParametersV1)] ->
    -- |Block state
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
doMintingP4 oldChainParameters paydayEpoch paydayMintRate foundationAddr mintUpds bs0 = do
    totGTU <- (^. totalGTU) <$> bsoGetBankStatus bs0
    seedstate <- bsoGetSeedState bs0
    let mint =
            calculatePaydayMintAmounts
                (oldChainParameters ^. rpMintDistribution)
                paydayMintRate
                (seedstate ^. epochLength * fromIntegral paydayEpoch)
                mintUpds
                totGTU
    bs1 <- bsoMint bs0 mint
    bsoAddSpecialTransactionOutcome
        bs1
        Mint
            { stoMintBakingReward = mintBakingReward mint,
              stoMintFinalizationReward = mintFinalizationReward mint,
              stoMintPlatformDevelopmentCharge = mintDevelopmentCharge mint,
              stoFoundationAccount = foundationAddr
            }

-- |Info about finalizers used for distributing rewards.
data FinalizerInfo = FinalizerInfo
    { -- |List of the parties in the finalization committee, with their relative voting power.
      -- Finalization rewards are distributed in proportion to their power for protocol
      -- version <= 3.
      committeeVoterPower :: !(Vec.Vector (BakerId, VoterPower)),
      -- |List of bakers who signed finalization proof. This is used for protocol version >= 4.
      committeeSigners :: ![BakerId]
    }

-- |Distribute the finalization rewards to the finalizers
-- in proportion to their voting weight. This also adds a
-- special transaction outcome recording the reward.
doFinalizationRewards ::
    (BlockStateOperations m, SupportsTransactionOutcomes (MPV m)) =>
    FinalizerInfo ->
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
doFinalizationRewards finInfo bs0
    | totalPower == 0 = error "doFinalizationRewards: Total finalizer weight is 0"
    | otherwise = do
        rewards <- _bankRewardAccounts <$> bsoGetBankStatus bs0
        let finRew = rewards ^. finalizationRewardAccount
        let awardFinalizer (t, m, bs) (bkr@(BakerId aid), power) = do
                let amt = fromInteger $ toInteger finRew * toInteger power `div` toInteger totalPower
                (mbaddr, bs') <- bsoRewardAccount bs aid amt
                case mbaddr of
                    Nothing -> error $ "doFinalizationRewards: Finalizer BakerId (" ++ show bkr ++ ") is not valid."
                    Just baddr -> return (t + amt, Map.insert baddr amt m, bs')
        (totalAward, awardMap, bs1) <- foldM awardFinalizer (0, Map.empty, bs0) (committeeVoterPower finInfo)
        let remainder = finRew - totalAward
        bs2 <- bsoSetRewardAccounts bs1 (rewards & finalizationRewardAccount .~ remainder)
        bsoAddSpecialTransactionOutcome
            bs2
            FinalizationRewards
                { stoFinalizationRewards = AccountAmounts awardMap,
                  stoRemainder = remainder
                }
  where
    totalPower = sum (snd <$> committeeVoterPower finInfo)

-- |The counts of the various 'free' transactions for the
-- purposes of determining the block reward.
data FreeTransactionCounts = FreeTransactionCounts
    { -- |Number of credential deployment transactions.
      countAccountCreation :: !Word16,
      -- |Number of chain update transactions.
      countUpdate :: !Word16,
      -- |Number of finalization records included.
      -- (Currently can only be 0 or 1, but higher values
      -- could be possible if the block format is revised.)
      countFinRecs :: !Word16
    }

-- |Distribute the transaction fees between the baker,
-- foundation, and GAS account.  Additionally, a proportion
-- of the GAS account is paid to the baker, consisting of a
-- base fraction and additional fractions for the 'free'
-- transactions in the block.
doBlockReward ::
    forall m.
    (BlockStateOperations m, MonadProtocolVersion m, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0, AccountVersionFor (MPV m) ~ 'AccountV0) =>
    -- |Transaction fees paid
    Amount ->
    -- |Counts of unpaid transactions
    FreeTransactionCounts ->
    -- |Block baker
    BakerId ->
    -- |Foundation account
    AccountAddress ->
    -- |Block state
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
doBlockReward transFees FreeTransactionCounts{..} (BakerId aid) foundationAddr bs0 = do
    chainParameters <- bsoGetChainParameters bs0
    let rewardParams = chainParameters ^. rewardParameters
    oldRewardAccts <- (^. rewardAccounts) <$> bsoGetBankStatus bs0
    let gasIn = oldRewardAccts ^. gasAccount
        bakerFees = takeFraction (rewardParams ^. tfdBaker) transFees
        gasFees = takeFraction (rewardParams ^. tfdGASAccount) transFees

        platformFees = transFees - (bakerFees + gasFees)
        -- Compute the GAS carried over. This is done at full precision and then
        -- rounded up (so that the payment to the baker is rounded up).
        gasGAS =
            ceiling $
                toRational gasIn
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasBaker)
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasAccountCreation) ^ countAccountCreation
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasChainUpdate) ^ countUpdate
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasFinalizationProof . unconditionally) ^ countFinRecs
        bakerGAS = gasIn - gasGAS
        gasOut = gasFees + gasGAS
        bakerOut = bakerFees + bakerGAS
    bs1 <- bsoSetRewardAccounts bs0 (oldRewardAccts & gasAccount .~ gasOut)
    bs2 <- bsoRewardFoundationAccount bs1 platformFees
    (mbkr, bs3) <- bsoRewardAccount bs2 aid bakerOut
    bkr <- case mbkr of
        Nothing -> error "Invalid baker account"
        Just bkr -> return bkr
    bsoAddSpecialTransactionOutcome
        bs3
        BlockReward
            { stoTransactionFees = transFees,
              stoOldGASAccount = gasIn,
              stoNewGASAccount = gasOut,
              stoBakerReward = bakerOut,
              stoFoundationCharge = platformFees,
              stoBaker = bkr,
              stoFoundationAccount = foundationAddr
            }

-- |Accrue the rewards for a block to the relevant pool, the passive delegators, and the foundation.
doBlockRewardP4 ::
    forall m.
    ( BlockStateOperations m,
      MonadProtocolVersion m,
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      PVSupportsDelegation (MPV m),
      SupportsTransactionOutcomes (MPV m),
      SeedStateVersionFor (MPV m) ~ 'SeedStateVersion0
    ) =>
    -- |Transaction fees paid
    Amount ->
    -- |Counts of unpaid transactions
    FreeTransactionCounts ->
    -- |Block baker
    BakerId ->
    -- |Block state
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
doBlockRewardP4 transFees FreeTransactionCounts{..} bid bs0 = do
    chainParameters <- bsoGetChainParameters bs0
    capitalDistribution <- bsoGetCurrentCapitalDistribution bs0
    bakers <- bsoGetCurrentEpochBakers bs0
    let rewardParams = chainParameters ^. rewardParameters
        passiveTransCommission = chainParameters ^. cpPoolParameters . ppPassiveCommissions . transactionCommission
    oldRewardAccts <- (^. rewardAccounts) <$> bsoGetBankStatus bs0
    -- Note: the GAS payout fraction \sigma_{G,out} is tfdGASAccount / (1 - \sigma_{T,F}).
    --
    let
        -- Previous value of gas account: GAS^(j-1)
        gasIn = oldRewardAccts ^. gasAccount
        -- Baker share of the incoming transaction fees, but includes some fees due to passive
        -- delegation: R_T * (1 - \sigma_{G,in})
        -- where R_T = (1 - \sigma_{T,F}) * T is the incoming transaction fees
        -- less the foundation cut.
        poolsAndPassiveFees = takeFraction (rewardParams ^. tfdBaker) transFees
        -- GAS account share of the incoming transaction fees, but includes some fees due to
        -- passive delegation: R_T * \sigma_{G,in}
        gasFees = takeFraction (rewardParams ^. tfdGASAccount) transFees
        -- Total stake of passive delegators
        passiveStake = sum $ dcDelegatorCapital <$> passiveDelegatorsCapital capitalDistribution
        -- Relative stake of passive delegators: s_L
        passiveRelativeStake = toRational $ passiveStake % (BI.bakerTotalStake bakers + passiveStake)
        -- Fraction of transaction fee rewards for the passive delegators:  (1 - \mu_{T,L}) * s_L
        passiveFraction = fractionToRational (complementAmountFraction passiveTransCommission) * passiveRelativeStake
        -- Share of the calculated GAS fees owed to passive:
        -- R_T * \sigma{G,in} * (1 - \mu_{T,L}) * s_L
        -- = \sigma{G,in} * R_{T,L}
        passiveGASFees = floor $ toRational gasFees * passiveFraction
        -- Share of the calculated transaction fees owed to passive:
        -- R_T * (1 - \sigma_{G,in}) * (1 - \mu_{T,L}) * s_L
        passiveTransFees = floor $ toRational poolsAndPassiveFees * passiveFraction
        -- The total amount owed to the passive delegators:
        -- R_{T,L} = R_T * (1 - \mu_{T,L}) * s_L
        passiveOut = passiveGASFees + passiveTransFees
        -- Share of transaction fees owed to the baker pool:
        -- (R_T - R_{T,L}) * (1 - \sigma_{G,in})
        poolFees = poolsAndPassiveFees - passiveTransFees
        -- Share of transaction fees paid as platform development charge:
        -- F_T = T * \sigma{T,F}
        --     = T - R_T
        --     = T - (R_T * (1 - \sigma_{G,in}) + R_T * \sigma_{G,in})
        platformFees = transFees - (poolsAndPassiveFees + gasFees)
        -- The share of the old GAS account that goes tot he new GAS account:
        -- gasIn * (1 - \sigma{G,out}) * (1 - f_acc)^a * (1 - f_gov)^u * (1 - f_fin)^f
        -- = GAS^(j-1) * (1 - \sigma{G,out}) * (1 - NGT(f,a,u))
        gasGAS =
            ceiling $
                toRational gasIn
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasBaker)
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasAccountCreation) ^ countAccountCreation
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasChainUpdate) ^ countUpdate
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasFinalizationProof . unconditionally) ^ countFinRecs
        -- Share of the old GAS account that is paid to the baker pool:
        -- gasIn * (1 - (1 - \sigma{G,out}) * (1 - NGT(f,a,u)))
        -- = GAS^(j-1) * (\sigma{G,out} + NGT(f,a,u) - \sigma{G,out} * NGT(f,a,u))
        bakerGAS = gasIn - gasGAS
        -- New balance of the GAS account:
        -- GAS^(j) = (R_T - R_{T,L}) * \sigma{G,in} + GAS^(j-1) * (1 - \sigma{G,out}) * (1 - NGT(f,a,u))
        gasOut = gasFees - passiveGASFees + gasGAS
        -- Amount of transaction fees and GAS account accruing to the baker pool:
        -- R_{T,P} = (R_T - R_{T,L}) * (1 - \sigma_{G,in}) + GAS^(j-1) * (\sigma{G,out} + NGT(f,a,u) - \sigma{G,out} * NGT(f,a,u))
        poolOut = poolFees + bakerGAS
    -- Sanity check:
    -- gasOut + platformFees + passiveOut + poolOut
    -- = (gasFees - passiveGASFees + gasGAS) + (transFees - (poolsAndPassiveFees + gasFees)) + (passiveGASFees + passiveTransFees) + (poolFees + bakerGAS)
    -- = gasGAS + transFees - poolsAndPassiveFees + passiveTransFees + poolFees + bakerGAS
    -- = gasGAS + transFees - poolsAndPassiveFees + passiveTransFees + poolsAndPassiveFees - passiveTransFees + bakerGAS
    -- = gasGAS + transFees + bakerGAS
    -- = gasGAS + transFees + gasIn - gasGAS
    -- = transFees + gasIn
    bs1 <- bsoSetRewardAccounts bs0 (oldRewardAccts & gasAccount .~ gasOut)
    bs2 <- bsoUpdateAccruedTransactionFeesFoundationAccount bs1 (amountToDelta platformFees)
    bs3 <- bsoUpdateAccruedTransactionFeesPassive bs2 (amountToDelta passiveOut)
    bs4 <- bsoUpdateAccruedTransactionFeesBaker bs3 bid (amountToDelta poolOut)
    bsoAddSpecialTransactionOutcome
        bs4
        BlockAccrueReward
            { stoTransactionFees = transFees,
              stoOldGASAccount = gasIn,
              stoNewGASAccount = gasOut,
              stoBakerReward = poolOut,
              stoPassiveReward = passiveOut,
              stoFoundationCharge = platformFees,
              stoBakerId = bid
            }

-- |@scaleAmount a b c@ computes @(a * c) `div` b@. It is expected that @0 <= a <= b@ and @0 < b@.
-- This is used to scale an amount in proportion to a ratio of two quantities for the purposes of
-- computing rewards.
scaleAmount :: Integral a => a -> a -> Amount -> Amount
scaleAmount num den amt = fromInteger $ (toInteger num * toInteger amt) `div` toInteger den

-- |Accumulated rewards and outcomes to a set of delegators.
data DelegatorRewardOutcomes = DelegatorRewardOutcomes
    { -- |Total finalization rewards distributed.
      _delegatorAccumFinalization :: !Amount,
      -- |Total baking rewards distributed.
      _delegatorAccumBaking :: !Amount,
      -- |Total transaction fees distributed.
      _delegatorAccumTransaction :: !Amount,
      -- |Accumulated transaction outcomes.
      _delegatorOutcomes :: !(Seq.Seq Types.SpecialTransactionOutcome)
    }

makeLenses ''DelegatorRewardOutcomes

-- |Reward each delegator in a pool.
-- Each type of reward is distributed separately, and the amounts to each delegator are rounded down.
-- The fraction awarded to each delegator is its capital divided by the total pool capital.
-- For baking pools, the total capital also includes the baker's equity capital.
--
-- Where the reward would be 0, no reward is paid, and no outcome is generated.
rewardDelegators ::
    forall m.
    ( PVSupportsDelegation (MPV m),
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      BlockStateOperations m
    ) =>
    UpdatableBlockState m ->
    -- |Finalization reward to distribute
    Amount ->
    -- |Baking reward to distribute
    Amount ->
    -- |Transaction fee reward to distribute
    Amount ->
    -- |Total capital of the pool (including owner)
    Amount ->
    -- |Identity and capital of each delegator to the pool
    Vec.Vector DelegatorCapital ->
    m (DelegatorRewardOutcomes, UpdatableBlockState m)
rewardDelegators bs totalFinalizationReward totalBakingReward totalTransactionReward totalCapital dcs
    | totalCapital == 0 = return (emptyRewardOutcomes, bs)
    | otherwise = foldM rewardDelegator (emptyRewardOutcomes, bs) dcs
  where
    emptyRewardOutcomes = DelegatorRewardOutcomes 0 0 0 Seq.empty
    rewardDelegator (accumRewardOutcomes, bs1) dc = do
        let
            -- c_{D,P} = capital_{D,P} / capital_P
            -- Note that we already checked that capital_P (i.e. totalCapital) is nonzero.
            scaleToCapital = scaleAmount (dcDelegatorCapital dc) totalCapital
            finalizationReward = scaleToCapital totalFinalizationReward
            bakingReward = scaleToCapital totalBakingReward
            transactionReward = scaleToCapital totalTransactionReward
            reward = finalizationReward + bakingReward + transactionReward
        if reward == 0
            then return (accumRewardOutcomes, bs1)
            else do
                (mAcct, bs2) <- bsoRewardAccount bs1 (delegatorAccountIndex $ dcDelegatorId dc) reward
                let acct = case mAcct of
                        Nothing -> error $ "Invariant violation: delegator account for " ++ show (dcDelegatorId dc) ++ " does not exist"
                        Just a -> a
                let !delegatorOutcome =
                        PaydayAccountReward
                            { stoAccount = acct,
                              stoTransactionFees = transactionReward,
                              stoBakerReward = bakingReward,
                              stoFinalizationReward = finalizationReward
                            }
                return
                    ( accumRewardOutcomes
                        & delegatorAccumFinalization +~ finalizationReward
                        & delegatorAccumBaking +~ bakingReward
                        & delegatorAccumTransaction +~ transactionReward
                        & delegatorOutcomes %~ (Seq.:|> delegatorOutcome),
                      bs2
                    )

-- |Accumulated rewards and outcomes to a set of baker pools.
data BakerRewardOutcomes = BakerRewardOutcomes
    { -- |Total finalization rewards distributed.
      _bakerAccumFinalization :: !Amount,
      -- |Total baking rewards distributed.
      _bakerAccumBaking :: !Amount,
      -- |Accumulated transaction outcomes.
      _bakerOutcomes :: Seq.Seq Types.SpecialTransactionOutcome
    }

makeLenses ''BakerRewardOutcomes

-- |Distribute the rewards for a reward period to the baker pools.
rewardBakers ::
    forall m.
    ( PVSupportsDelegation (MPV m),
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      BlockStateOperations m
    ) =>
    UpdatableBlockState m ->
    -- |The baker stakes and commission rates in the reward period
    BI.FullBakersEx ->
    -- |The total baking rewards payable to baker pools
    Amount ->
    -- |The total finalization rewards payable to baker pools
    Amount ->
    -- |The capital of bakers and delegators in the reward period
    Vec.Vector BakerCapital ->
    -- |The details of rewards earned by each baker pool
    Map.Map BakerId BakerPoolRewardDetails ->
    m (BakerRewardOutcomes, UpdatableBlockState m)
rewardBakers bs bakers bakerTotalBakingRewards bakerTotalFinalizationRewards bcs poolRewardDetails
    | paydayBlockCount == 0 = return (emptyRewardOutcomes, bs)
    | otherwise = do
        foldM rewardBaker (emptyRewardOutcomes, bs) bcs
  where
    -- B - the total number of blocks baked by all bakers
    paydayBlockCount = sum $ blockCount <$> poolRewardDetails
    emptyRewardOutcomes = BakerRewardOutcomes 0 0 Seq.empty
    -- Map from baker IDs to the pool stake and commission rates.
    -- These are taken from the 'FullBakersEx', which was frozen prior to the reward period.
    bakerStakesAndRatesMap =
        foldl'
            ( \m bi ->
                Map.insert
                    (bi ^. BI.theBakerInfo . Types.bakerIdentity)
                    (bi ^. BI.bakerStake, bi ^. BI.bakerPoolCommissionRates)
                    m
            )
            Map.empty
            (BI.bakerInfoExs bakers)
    finStake (bid, bprd)
        | finalizationAwake bprd = case Map.lookup bid bakerStakesAndRatesMap of
            Nothing ->
                error "Invariant violation: baker from pool reward details is not an epoch baker"
            Just (s, _) ->
                s
        | otherwise = 0
    -- The total finalization stake.
    -- This is used for distributing finalization rewards in proportion to stake.
    totalFinStake = sum $ finStake <$> Map.toList poolRewardDetails

    -- Reward a baker pool, accumulating the baking and finalization reward totals and reward outcomes.
    rewardBaker (accumRewards, bsIn) bc = do
        let bprd = case Map.lookup (bcBakerId bc) poolRewardDetails of
                Nothing -> error "Invariant violation: baker from capital distribution does not have pool reward details"
                Just prd -> prd
        let poolTransactionReward = transactionFeesAccrued bprd
            bakerBlockCount = blockCount bprd
            bakerBlockFraction = scaleAmount bakerBlockCount paydayBlockCount
            poolBakingReward = bakerBlockFraction bakerTotalBakingRewards
            finalized = finalizationAwake bprd
            -- 'scaleToFinalizationStake' scales an amount by the ratio of the finalization stake
            -- to the total finalization stake (i.e. fs_P).
            -- The finalization stake (fstake_P) is the stake if the baker is awake for
            -- finalization, and 0 otherwise.
            (scaleToFinalizationStake, commissionRates) = case Map.lookup (bcBakerId bc) bakerStakesAndRatesMap of
                Nothing ->
                    error "Invariant violation: baker from capital distribution is not an epoch baker"
                Just (s, commRates) ->
                    ( if finalized && totalFinStake /= 0 -- It should never be that totalFinStake == 0
                        then scaleAmount s totalFinStake
                        else const 0,
                      commRates
                    )
            -- R_{F,P} = (R_F - R_{F,L})·fs_P
            poolFinalizationReward = scaleToFinalizationStake bakerTotalFinalizationRewards
            -- capital_P -- the total capital of the pool
            totalPoolCapital = bcBakerEquityCapital bc + sum (dcDelegatorCapital <$> bcDelegatorCapital bc)
        -- We do not log an event (or issue rewards) if the rewards would be 0.
        if poolBakingReward == 0 && poolFinalizationReward == 0 && poolTransactionReward == 0
            then return (accumRewards, bsIn)
            else do
                let
                    -- 1 - μ_{F,P}
                    finalizationFraction = complementAmountFraction (commissionRates ^. finalizationCommission)
                    -- 1 - μ_{B,P}
                    bakingFraction = complementAmountFraction (commissionRates ^. bakingCommission)
                    -- 1 - μ_{T,P}
                    transactionFraction = complementAmountFraction (commissionRates ^. transactionCommission)
                    -- R_{F,P} · (1 - μ_{F,P})
                    totalDelegationFinalization = takeFraction finalizationFraction poolFinalizationReward
                    -- R_{F,B} · (1 - μ_{F,B})
                    totalDelegationBaking = takeFraction bakingFraction poolBakingReward
                    -- R_{F,T} · (1 - μ_{F,T})
                    totalDelegationTransaction = takeFraction transactionFraction poolTransactionReward
                -- We reward the delegators each their share of the rewards, and the rest is assigned to the
                -- pool owner.  (Since amounts to delegators are rounded down, all rounding errors go to
                -- the pool owner.)
                (DelegatorRewardOutcomes{..}, bsDel) <-
                    rewardDelegators
                        bsIn
                        totalDelegationFinalization
                        totalDelegationBaking
                        totalDelegationTransaction
                        totalPoolCapital
                        (bcDelegatorCapital bc)
                let transactionRewardToBaker = poolTransactionReward - _delegatorAccumTransaction
                    bakingRewardToBaker = poolBakingReward - _delegatorAccumBaking
                    finalizationRewardToBaker = poolFinalizationReward - _delegatorAccumFinalization
                    bakerReward = transactionRewardToBaker + bakingRewardToBaker + finalizationRewardToBaker
                (mAcct, bsBak) <- bsoRewardAccount bsDel (bakerAccountIndex $ bcBakerId bc) bakerReward
                let poolOutcome =
                        PaydayPoolReward
                            { stoPoolOwner = Just (bcBakerId bc),
                              stoTransactionFees = poolTransactionReward,
                              stoBakerReward = poolBakingReward,
                              stoFinalizationReward = poolFinalizationReward
                            }
                    acct = case mAcct of
                        Nothing -> error $ "Invariant violation: baker account for " ++ show (bcBakerId bc) ++ " does not exist"
                        Just a -> a
                    bakerOutcome =
                        PaydayAccountReward
                            { stoAccount = acct,
                              stoTransactionFees = transactionRewardToBaker,
                              stoBakerReward = bakingRewardToBaker,
                              stoFinalizationReward = finalizationRewardToBaker
                            }
                return
                    ( accumRewards
                        & bakerAccumFinalization +~ poolFinalizationReward
                        & bakerAccumBaking +~ poolBakingReward
                        & bakerOutcomes %~ (<> (poolOutcome Seq.:<| bakerOutcome Seq.:<| _delegatorOutcomes)),
                      bsBak
                    )

-- |Distribute the rewards that have accrued as part of a payday.
distributeRewards ::
    forall m.
    ( PVSupportsDelegation (MPV m),
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      BlockStateOperations m,
      TreeStateMonad m
    ) =>
    -- |Foundation account address
    AccountAddress ->
    CapitalDistribution ->
    BI.FullBakersEx ->
    Map.Map BakerId BakerPoolRewardDetails ->
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
distributeRewards foundationAddr capitalDistribution bakers poolRewardDetails bs0 = do
    -- The chain parameters are used to determine the passive finalization and baking commission rates.
    -- Note: unlike baker pool commission rates, these are not frozen at the prior to the reward period.
    passiveCommissions <- (^. cpPoolParameters . ppPassiveCommissions) <$> bsoGetChainParameters bs0
    rewardAccts <- (^. bankRewardAccounts) <$> bsoGetBankStatus bs0
    passiveAccrued <- bsoGetAccruedTransactionFeesPassive bs0
    let
        -- stake_L -- The passive stake is the total capital passively delegated
        passiveStake = sum $ dcDelegatorCapital <$> passiveDelegatorsCapital capitalDistribution
        -- s_L -- The relative stake of the passive delegators; s_L = stake_L / (stake + stake_L)
        passiveRelativeStake :: Rational
        passiveRelativeStake = toRational $ passiveStake % (BI.bakerPoolTotalStake bakers + passiveStake)
        -- Finalization commission rate for the passive delegators
        passiveFinalizationCommission = passiveCommissions ^. finalizationCommission
        -- (1 - μ_{F,L})·s_L -- the passive delegators' fraction of the finalization rewards
        passiveFinalizationFraction = fractionToRational (complementAmountFraction passiveFinalizationCommission) * passiveRelativeStake
        -- R_{F,L} = R_F·(1 - μ_{F,L})·s_L -- the passive delegators' finalization rewards. Rounded down.
        passiveFinalizationReward = floor $ toRational (rewardAccts ^. finalizationRewardAccount) * passiveFinalizationFraction
        -- Baking commission rate for the passive delegators
        passiveBakingCommission = passiveCommissions ^. bakingCommission
        -- (1 - μ_{B,L))·s_L -- the passive delegators' fraction of the baking rewards
        passiveBakingFraction = fractionToRational (complementAmountFraction passiveBakingCommission) * passiveRelativeStake
        -- R_{B,L} = R_B·(1 - μ_{B,L})·s_L -- the passive delegators' baking rewards. Rounded down.
        passiveBakingReward = floor $ toRational (rewardAccts ^. bakingRewardAccount) * passiveBakingFraction
    -- Reward the passive delegators.
    (passiveRes, bs1) <-
        rewardDelegators bs0 passiveFinalizationReward passiveBakingReward passiveAccrued passiveStake (passiveDelegatorsCapital capitalDistribution)
    -- Subtract the rewarded transaction fees from those accrued to the passive delegators. There result can be
    -- positive due to rounding down the rewards to each delegator.
    bs2 <- bsoUpdateAccruedTransactionFeesPassive bs1 (amountDiff 0 (passiveRes ^. delegatorAccumTransaction))
    -- (R_B - R_{B,L}) -- Total baking rewards due to baking pools
    let bakerTotalBakingRewards = rewardAccts ^. bakingRewardAccount - passiveBakingReward
    -- (R_F - R_{F,L}) -- Total finalization rewards due to baking pools
    let bakerTotalFinalizationRewards = rewardAccts ^. finalizationRewardAccount - passiveFinalizationReward
    -- Reward each baking pool, including the pool owner and delegators.
    -- This accumulates the total baking and finalization reward payout to baking pools, as well
    -- as outcomes for each (non-zero) reward.
    (BakerRewardOutcomes{..}, bs3) <-
        rewardBakers bs2 bakers bakerTotalBakingRewards bakerTotalFinalizationRewards (bakerPoolCapital capitalDistribution) poolRewardDetails
    -- To update the finalization reward account and the baking reward account, we subtract the amounts
    -- paid out from each.
    let newRewardAccts =
            rewardAccts
                & finalizationRewardAccount -~ ((passiveRes ^. delegatorAccumFinalization) + _bakerAccumFinalization)
                & bakingRewardAccount -~ ((passiveRes ^. delegatorAccumBaking) + _bakerAccumBaking)
    bs4 <- bsoSetRewardAccounts bs3 newRewardAccts
    -- Pay out to the foundation the transaction fees accrued to it.
    -- Note, the foundation receives its share of the baking and finalization rewards as part of the
    -- minting, which is handled in 'doMintingP4'.
    accruedFoundation <- bsoGetAccruedTransactionFeesFoundationAccount bs4
    bs5 <- bsoRewardFoundationAccount bs4 accruedFoundation
    bs6 <- bsoUpdateAccruedTransactionFeesFoundationAccount bs5 (amountDiff 0 accruedFoundation)
    -- Log the special outcomes for the reward payouts.
    bs7 <-
        bsoAddSpecialTransactionOutcome
            bs6
            PaydayFoundationReward
                { stoFoundationAccount = foundationAddr,
                  stoDevelopmentCharge = accruedFoundation
                }
    -- We record the actual totals paid to the passive delegators, which may be smaller than the amounts
    -- accrued to the passive delegators due to rounding. The remainders are carried over till the next payday.
    bs8 <-
        bsoAddSpecialTransactionOutcome
            bs7
            PaydayPoolReward
                { stoPoolOwner = Nothing,
                  stoTransactionFees = passiveRes ^. delegatorAccumTransaction,
                  stoBakerReward = passiveRes ^. delegatorAccumBaking,
                  stoFinalizationReward = passiveRes ^. delegatorAccumFinalization
                }
    bs9 <- foldM bsoAddSpecialTransactionOutcome bs8 (passiveRes ^. delegatorOutcomes)
    foldM bsoAddSpecialTransactionOutcome bs9 _bakerOutcomes

-- |Get the updated value of the time parameters at a given slot, given the original time
-- parameters and the elapsed updates.
updatedTimeParameters ::
    -- |Target slot
    Slot ->
    -- |Original time parameters
    TimeParameters ->
    -- |Updates
    [(Slot, UpdateValue cpv)] ->
    TimeParameters
updatedTimeParameters targetSlot tp0 upds =
    timeParametersAtSlot
        targetSlot
        tp0
        [(slot, tp) | (slot, UVTimeParameters tp) <- upds]

-- |Mint for any skipped paydays, returning the epoch of the next future payday, the mint rate
-- for that payday, and the updated block state.  This does not mint for the first payday after
-- the previous block, which is assumed to have already been minted.  This is only invoked in the
-- first block after a payday.
--
-- This function does the following:
--   - Compute the first payday after the most recently minted payday, accounting for any changes
--     in payday length and mint rate that occurred prior to the most recently minted payday.
--   - If this payday has elapsed, mint for it, and continue recursively.
--   - Otherwise, return the payday and mint rate.
mintForSkippedPaydays ::
    ( ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      BlockStateOperations m,
      SupportsTransactionOutcomes (MPV m),
      SeedStateVersionFor (MPV m) ~ 'SeedStateVersion0
    ) =>
    -- |Epoch of the current block
    Epoch ->
    -- |Epoch of the most recently minted payday
    Epoch ->
    -- |Chain parameters at the parent block
    ChainParameters (MPV m) ->
    -- |Foundation account address
    AccountAddress ->
    -- |Updates processed in this block
    [(Slot, UpdateValue 'ChainParametersV1)] ->
    -- |Block state
    UpdatableBlockState m ->
    m (Epoch, MintRate, UpdatableBlockState m)
mintForSkippedPaydays newEpoch payday oldChainParameters foundationAccount updates bs0 = do
    seedstate <- bsoGetSeedState bs0
    let paydaySlot = seedstate ^. epochLength * fromIntegral payday
        bestTP = updatedTimeParameters paydaySlot (oldChainParameters ^. cpTimeParameters . supportedOParam) updates
        nextMintRate = bestTP ^. tpMintPerPayday
        nextRPL = bestTP ^. tpRewardPeriodLength
        nextPayday = payday + rewardPeriodEpochs nextRPL
    if newEpoch < nextPayday
        then return (nextPayday, nextMintRate, bs0)
        else do
            bs1 <- doMintingP4 oldChainParameters nextPayday nextMintRate foundationAccount updates bs0
            mintForSkippedPaydays newEpoch nextPayday oldChainParameters foundationAccount updates bs1

-- |Find committee signers in 'FinalizerInfo' and mark them as awake finalizers.
addAwakeFinalizers ::
    (BlockStateOperations m, PVSupportsDelegation (MPV m)) =>
    Maybe FinalizerInfo ->
    UpdatableBlockState m ->
    m (UpdatableBlockState m)
addAwakeFinalizers Nothing bs = return bs
addAwakeFinalizers (Just FinalizerInfo{..}) bs0 =
    bsoMarkFinalizationAwakeBakers bs0 committeeSigners

-- |Parameters used by 'mintAndReward' that are determined by 'updateBirkParameters'.
-- 'updateBirkParameters' determines these, since it makes state changes that would make them
-- inaccessible in 'mintAndReward'.
data MintRewardParams (cpv :: ChainParametersVersion) where
    MintRewardParamsV0 ::
        { -- |Whether the block is in a different epoch to its parent.
          isNewEpoch :: !Bool
        } ->
        MintRewardParams 'ChainParametersV0
    -- |Indicates that no payday has elapsed since the parent block.
    MintRewardParamsV1NoPayday :: MintRewardParams 'ChainParametersV1
    -- |Indicates that at least one payday has elapsed since the parent block.
    MintRewardParamsV1Payday ::
        { -- |The distribution of the capital at the first elapsed payday.
          paydayCapitalDistribution :: !CapitalDistribution,
          -- |The baker stakes at the first elapsed payday.
          paydayBakers :: !BI.FullBakersEx,
          -- |The reward details for each baker pool at the first payday.
          paydayBakerPoolRewards :: !(Map.Map BakerId BakerPoolRewardDetails),
          -- |The epoch of the latest elapsed payday.
          lastElapsedPayday :: !Epoch
        } ->
        MintRewardParams 'ChainParametersV1

-- |Mint new tokens and distribute rewards to bakers, finalizers and the foundation.
--
-- For P1 to P3, the process consists of the following four steps:
--
-- 1. If the block is the first in a new epoch, distribute the baking reward account
--    to the bakers of the previous epoch in proportion to the number of blocks they
--    baked. (The reward per block is BakingRewardAccount/#blocks, rounded down.
--    Any remainder is kept in the account.)
--
-- 2. GTU is minted for each slot since the previous block.  The minted GTUs are
--    distributed to the baker reward account, finalization reward account, and
--    foundation account. (Rounding favours the foundation account.  GTU is minted
--    for a series of slots before it is divided among the recipients, which should
--    reduce the effect of rounding.  The series of slots is typically the entire
--    number between blocks, but is broken for updates to the mint rate and distribution
--    fractions.)
--
-- 3. If the block contains a finalization record, the finalizers are rewarded with
--    the balance of the finalization reward account, distributed according to their
--    voting power (i.e. their stake).  The reward to each finalizer is rounded down,
--    with any remaining balance remaining in the reward account.  (Note: the rounding
--    here is different to the baking reward distribution.)
--
-- 4. The transaction fees are distributed between the baker, GAS account, and foundation
--    account.  Additionally, a fraction of the old GAS account is paid to the baker,
--    including incentives for including the 'free' transaction types.  (Rounding of
--    the fee distribution favours the foundation.  The GAS reward is rounded down.)
--
-- For P4 onwards, the process is as follows:
--
-- 1. If a payday has elapsed then:
--
--    (1) Mint for the (first) elapsed payday. Minting is distributed to the foundation account,
--        finalization reward account and baking reward account.  The amount is determined by the
--        mint rate set at the last payday.  The distribution is determined by the state of the
--        chain parameters as of the first slot of the payday.
--
--    (2) Distribute the rewards earned over the reward period to the bakers and delegators.
--        Note that the stakes, capital and accrued rewards are determined in 'updateBirkParameters'.
--        This is important, because that function rotates the stakes and capital, and clears the
--        accrued rewards.
--
--    (3) Mint for any additional paydays. The amount is determined by the mint rate
--
--    (4) Set the mint rate and payday epoch for the next payday based on the time parameters
--        at the most recent payday.
--
-- 2. If the block contains a finalization record, mark all finalizers in that record as being
--    awake.
--
-- 3. Record that the baker has baked a block in the current reward period.
--
-- 4. The transaction fees are distributed between the GAS account, foundation accrued transaction
--    rewards, baker pool accrued transaction rewards and passive delegation accrued transaction rewards.
--    Additionally, a fraction of the old GAS account accrues to the baker pool transaction rewards,
--    including incentives for including the 'free' transaction types.
mintAndReward ::
    forall m.
    (BlockStateOperations m, TreeStateMonad m, MonadProtocolVersion m) =>
    -- |Block state
    UpdatableBlockState m ->
    -- |Parent block
    BlockPointerType m ->
    -- |Block slot
    Slot ->
    -- |Baker ID
    BakerId ->
    -- |Epoch of the new block
    Epoch ->
    -- |Parameters determined by 'updateBirkParameters'
    MintRewardParams (ChainParametersVersionFor (MPV m)) ->
    -- |Info on finalization committee for included record, if any
    Maybe FinalizerInfo ->
    -- |Transaction fees
    Amount ->
    -- |Number of "free" transactions of each type
    FreeTransactionCounts ->
    -- |Ordered chain updates since the last block
    [(Slot, UpdateValue (ChainParametersVersionFor (MPV m)))] ->
    m (UpdatableBlockState m)
mintAndReward bshandle blockParent slotNumber bid newEpoch mintParams mfinInfo transFees freeCounts updates =
    case protocolVersion @(MPV m) of
        SP1 -> mintAndRewardCPV0AccountV0
        SP2 -> mintAndRewardCPV0AccountV0
        SP3 -> mintAndRewardCPV0AccountV0
        SP4 -> mintAndRewardCPV1AccountV1
        SP5 -> mintAndRewardCPV1AccountV1
        SP6 -> error "Minting undefined for P6" -- FIXME: implement
  where
    mintAndRewardCPV0AccountV0 ::
        ( AccountVersionFor (MPV m) ~ 'AccountV0,
          ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0
        ) =>
        m (UpdatableBlockState m)
    mintAndRewardCPV0AccountV0 = do
        -- First, reward bakers from previous epoch, if we are starting a new one.
        bshandleEpoch <-
            (if isNewEpoch mintParams then rewardLastEpochBakers else return) bshandle
                -- Add the block to the list of blocks baked in this epoch
                >>= flip bsoNotifyBlockBaked bid

        -- Foundation rewards are always paid to the current foundation account as of the block
        -- in which the rewards are distributed.
        foundationAccount <- getAccountCanonicalAddress =<< bsoGetFoundationAccount bshandleEpoch

        -- Then mint GTU.
        let mintUpdates = [(slot, md) | (slot, UVMintDistribution md) <- updates]
        bshandleMint <- doMinting blockParent slotNumber foundationAccount mintUpdates bshandleEpoch

        -- Next, reward the finalizers, if the block includes a finalization record.
        bshandleFinRew <- case mfinInfo of
            Nothing -> return bshandleMint
            Just finInfo -> doFinalizationRewards finInfo bshandleMint

        -- Finally, reward the block baker.
        doBlockReward transFees freeCounts bid foundationAccount bshandleFinRew

    mintAndRewardCPV1AccountV1 ::
        ( PVSupportsDelegation (MPV m),
          ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
          SeedStateVersionFor (MPV m) ~ 'SeedStateVersion0
        ) =>
        m (UpdatableBlockState m)
    mintAndRewardCPV1AccountV1 = do
        -- If a payday has elapsed, distribute the accrued rewards.
        bshandlePayday <- case mintParams of
            MintRewardParamsV1NoPayday -> return bshandle
            MintRewardParamsV1Payday{..} -> do
                -- Foundation rewards are always paid to the current foundation account as of the block
                -- in which the rewards are distributed.
                foundationAccount <- getAccountCanonicalAddress =<< bsoGetFoundationAccount bshandle
                nextPayday <- bsoGetPaydayEpoch bshandle
                nextMintRate <- bsoGetPaydayMintRate bshandle
                oldChainParameters <- (^. currentParameters) <$> (getUpdates =<< blockState blockParent)
                bshandleMint <- doMintingP4 oldChainParameters nextPayday nextMintRate foundationAccount updates bshandle
                bshandleRewards <-
                    distributeRewards
                        foundationAccount
                        paydayCapitalDistribution
                        paydayBakers
                        paydayBakerPoolRewards
                        bshandleMint
                (newPayday, newMintRate, bshandleSkipped) <-
                    mintForSkippedPaydays newEpoch nextPayday oldChainParameters foundationAccount updates bshandleRewards
                bshandleSetPayday <- bsoSetPaydayEpoch bshandleSkipped newPayday
                bsoSetPaydayMintRate bshandleSetPayday newMintRate
        bshandleFinAwake <- addAwakeFinalizers mfinInfo bshandlePayday
        bshandleNotify <- bsoNotifyBlockBaked bshandleFinAwake bid
        doBlockRewardP4 transFees freeCounts bid bshandleNotify

-- |Update the bakers and seed state of the block state.
-- The epoch for the new seed state must be at least the epoch of
-- the old seed state (currently on the block state).
--
-- Prior to protocol version P4, if the epoch is new, the old bakers are rewarded with the balance
-- of the baking reward account, in proportion to the number of blocks they baked in that epoch.
-- If the new epoch is not the same as or direct successor of the current
-- one, then 'bsoTransitionEpochBakers' is called twice. This should be
-- sufficient because the bakers will not change in the intervening epochs.
-- The return value indicates if the epoch is new.
--
-- From protocol version P4, we consider the paydays after the previous block and update the next
-- bakers if the epoch before that payday has since elapsed, and rotate the current bakers if the
-- payday itself has since elapsed.  If a payday has elapsed, the baker and delegator cooldowns
-- are processed by calling 'bsoProcessPendingChanges' with the time of the last elapsed payday.
-- The return value indicates whether a payday has elapsed, and if so, the bakers and capital
-- distribution for that payday, the accrued rewards to each baker pool, and the epoch of the
-- last elapsed payday.  Returning the bakers, capital distribution and accrued rewards is
-- necessary, since they are rotated, but the old values are required later for distributing
-- rewards.
updateBirkParameters ::
    forall m.
    (BlockStateOperations m, TreeStateMonad m, MonadProtocolVersion m) =>
    -- |New seed state
    SeedState (SeedStateVersionFor (MPV m)) ->
    -- |Block state
    UpdatableBlockState m ->
    -- |Chain parameters at the previous block
    ChainParameters (MPV m) ->
    -- |Chain updates since the previous block
    [(Slot, UpdateValue (ChainParametersVersionFor (MPV m)))] ->
    m (MintRewardParams (ChainParametersVersionFor (MPV m)), UpdatableBlockState m)
updateBirkParameters newSeedState bs0 oldChainParameters updates = case protocolVersion @(MPV m) of
    SP1 -> updateCPV0AccountV0
    SP2 -> updateCPV0AccountV0
    SP3 -> updateCPV0AccountV0
    SP4 -> updateCPV1AccountV1
    SP5 -> updateCPV1AccountV1
    SP6 -> error "updateBirkParameters not implemented for P6" -- FIXME: implement
  where
    updateCPV0AccountV0 ::
        AccountVersionFor (MPV m) ~ 'AccountV0 =>
        m (MintRewardParams 'ChainParametersV0, UpdatableBlockState m)
    updateCPV0AccountV0 = do
        oldSeedState <- bsoGetSeedState bs0
        let isNewEpoch = oldSeedState ^. epoch /= newSeedState ^. epoch
        bs1 <-
            if isNewEpoch
                then do
                    upToLast <-
                        if oldSeedState ^. epoch /= newSeedState ^. epoch - 1
                            then bsoTransitionEpochBakers bs0 (newSeedState ^. epoch - 1)
                            else return bs0
                    bsoTransitionEpochBakers upToLast (newSeedState ^. epoch)
                else return bs0
        (MintRewardParamsV0 isNewEpoch,) <$> bsoSetSeedState bs1 newSeedState
    updateCPV1AccountV1 ::
        ( PVSupportsDelegation (MPV m),
          ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
          SeedStateVersionFor (MPV m) ~ 'SeedStateVersion0
        ) =>
        m (MintRewardParams 'ChainParametersV1, UpdatableBlockState m)
    updateCPV1AccountV1 = do
        oldSeedState <- bsoGetSeedState bs0
        if oldSeedState ^. epoch == newSeedState ^. epoch
            then (MintRewardParamsV1NoPayday,) <$> bsoSetSeedState bs0 newSeedState
            else do
                -- This is the start of a new epoch.
                -- Assume: oldSeedState ^. epoch < newSeedState ^. epoch
                payday <- bsoGetPaydayEpoch bs0
                let oldTimeParameters = oldChainParameters ^. cpTimeParameters . supportedOParam
                    -- Convert an Epoch to a Slot.
                    slotFor = (oldSeedState ^. epochLength *) . fromIntegral
                    -- For each payday after the parent block:
                    --   - If the epoch before the payday is elapsed, generate the next bakers for the
                    --     reward period that starts from that payday.
                    --   - If the payday is elapsed, rotate the next epoch bakers and capital distribution
                    --     to become the current epoch bakers and capital distribution.
                    processPaydays pd mrps0 bspp0 = do
                        bspp1 <-
                            if oldSeedState ^. epoch < pd - 1 && pd - 1 <= newSeedState ^. epoch
                                then generateNextBakers pd bspp0
                                else return bspp0
                        if pd <= newSeedState ^. epoch
                            then do
                                -- Calculate the next payday by adding the reward period length given by the
                                -- time parameters as of the payday.
                                let effectiveTPs = updatedTimeParameters (slotFor pd) oldTimeParameters updates
                                    nextPd = pd + rewardPeriodEpochs (effectiveTPs ^. tpRewardPeriodLength)
                                mrps1 <- case mrps0 of
                                    MintRewardParamsV1NoPayday -> do
                                        paydayCapitalDistribution <- bsoGetCurrentCapitalDistribution bspp1
                                        paydayBakers <- bsoGetCurrentEpochFullBakersEx bspp1
                                        paydayBakerPoolRewards <- bsoGetBakerPoolRewardDetails bspp1
                                        return
                                            MintRewardParamsV1Payday
                                                { lastElapsedPayday = pd,
                                                  ..
                                                }
                                    MintRewardParamsV1Payday{..} ->
                                        return
                                            MintRewardParamsV1Payday
                                                { lastElapsedPayday = pd,
                                                  ..
                                                }
                                bspp2 <- bsoRotateCurrentCapitalDistribution bspp1
                                bspp3 <- bsoRotateCurrentEpochBakers bspp2
                                processPaydays nextPd mrps1 bspp3
                            else -- The payday has not elapsed, so we are done.
                                return (mrps0, bspp1)
                (res, bs1) <- processPaydays payday MintRewardParamsV1NoPayday bs0
                -- If this is the first block after a payday, we process the pending changes on bakers and
                -- delegators (i.e. unlock stake that exits cooldown).
                bs2 <- case res of
                    MintRewardParamsV1NoPayday -> return bs1
                    MintRewardParamsV1Payday{..} -> do
                        et <- effectiveTest lastElapsedPayday
                        bsoProcessPendingChanges bs1 et
                (res,) <$> bsoSetSeedState bs2 newSeedState

-- |Count the free transactions of each kind in a list.
-- The second argument indicates if the block contains a finalization record.
countFreeTransactions :: [BlockItem] -> Bool -> FreeTransactionCounts
countFreeTransactions bis hasFinRec = foldl' cft f0 bis
  where
    f0 =
        FreeTransactionCounts
            { countAccountCreation = 0,
              countUpdate = 0,
              countFinRecs = if hasFinRec then 1 else 0
            }
    cft f bi = case wmdData bi of
        CredentialDeployment{} -> let !c = countAccountCreation f + 1 in f{countAccountCreation = c}
        ChainUpdate{} -> let !c = countUpdate f + 1 in f{countUpdate = c}
        _ -> f

-- |Given 'CommissionRanges' and a 'BakerId', update the baker's commission rates to fall within
-- the ranges (at the closest rate to the existing rate).
putBakerCommissionsInRange ::
    forall m.
    ( PoolParametersVersionFor (ChainParametersVersionFor (MPV m)) ~ 'PoolParametersVersion1,
      BlockStateOperations m,
      IsProtocolVersion (MPV m)
    ) =>
    CommissionRanges ->
    UpdatableBlockState m ->
    BakerId ->
    m (UpdatableBlockState m)
putBakerCommissionsInRange ranges bs (BakerId ai) = case protocolVersion @(MPV m) of
    SP4 -> bsoConstrainBakerCommission bs ai ranges
    SP5 -> bsoConstrainBakerCommission bs ai ranges
    SP6 -> bsoConstrainBakerCommission bs ai ranges

-- |The result of executing the block prologue.
data PrologueResult m = PrologueResult
    { -- |Ordered list of chain parameter updates that have occurred since the previous block.
      prologueUpdates :: [(Slot, UpdateValue (ChainParametersVersionFor (MPV m)))],
      -- |The parameters required for mint and rewarding in the block epilogue.
      prologueMintRewardParams :: MintRewardParams (ChainParametersVersionFor (MPV m)),
      -- |The updated block state after executing the prologue.
      prologueBlockState :: UpdatableBlockState m
    }

-- |Execute the prologue of a block, i.e. the state updates that occur before the (explicit)
-- block transactions are executed.  This consists of:
--
-- 1. Process the update queues, enacting any chain parameter updates that have occurred since the
--    parent block.
--
-- 2. When the commission ranges have been updated, adjust all bakers to be inside the new
--    commission ranges. (If multiple changes are made then all of them are enacted sequentially,
--    as this can have a different outcome from simply enacting the last change.)
--
-- 3. Process scheduled releases, unlocking the amounts when the release time has expired.
--
-- 4. Update the bakers and seed state. (See 'updateBirkParameters' for details.)
--
-- The result of the function includes the processed updates, parameters for minting and rewarding,
-- and the updated block state.
executeBlockPrologue ::
    forall m.
    (BlockPointerMonad m, TreeStateMonad m, MonadLogger m) =>
    -- |Slot time of the new block
    Timestamp ->
    -- |New seed state
    SeedState (SeedStateVersionFor (MPV m)) ->
    -- |Chain parameters from the parent block
    ChainParameters (MPV m) ->
    -- |Block state from the parent block
    UpdatableBlockState m ->
    m (PrologueResult m)
executeBlockPrologue slotTime newSeedState oldChainParameters bsStart = do
    -- process the update queues
    (updates, bsDoneUpdates) <- bsoProcessUpdateQueues bsStart slotTime
    genData <- getGenesisData
    -- We convert the transaction times to slot times as this is more convenient for determining
    -- when the updates occur with respect to epoch boundaries etc.
    let prologueUpdates =
            (_1 %~ transactionTimeToSlot (gdGenesisTime genData) (gdSlotDuration genData))
                <$> Map.toAscList updates
    ab <- bsoGetActiveBakers bsDoneUpdates
    -- for each pool parameter update, go over all bakers and put their commissions inside
    -- the new commission ranges.
    let applyCommissionBounds bs (UVPoolParameters PoolParametersV1{..}) =
            foldM (putBakerCommissionsInRange _ppCommissionBounds) bs ab
        applyCommissionBounds bs _ = return bs
    bsDoneCommissions <- foldM applyCommissionBounds bsDoneUpdates (snd <$> prologueUpdates)
    -- unlock the scheduled releases that have expired
    bsDoneReleases <- bsoProcessReleaseSchedule bsDoneCommissions slotTime
    -- update the bakers and seed state
    (prologueMintRewardParams, prologueBlockState) <-
        updateBirkParameters newSeedState bsDoneReleases oldChainParameters prologueUpdates
    return PrologueResult{..}

-- |Execute a block from a given starting state.
-- Fail if any of the transactions fails, otherwise return the new 'BlockState' and the amount of energy used
-- during this block execution.
--
-- The slot number must exceed the slot of the parent block, and the seed state
-- must indicate the correct epoch of the block.
executeFrom ::
    forall m.
    (BlockPointerMonad m, TreeStateMonad m, MonadLogger m) =>
    -- |Hash of the block we are executing. Used only for committing transactions.
    BlockHash ->
    -- |Slot number of the block being executed.
    Slot ->
    -- |Unix timestamp of the beginning of the slot.
    Timestamp ->
    -- |Parent pointer from which to start executing
    BlockPointerType m ->
    -- |Identity of the baker who should be rewarded.
    BakerId ->
    -- |Parties to the finalization record in this block, if any
    Maybe FinalizerInfo ->
    -- |New seed state
    SeedState (SeedStateVersionFor (MPV m)) ->
    -- |Transactions on this block with their associated verification result.
    [TVer.BlockItemWithStatus] ->
    m (Either (Maybe FailureKind) (ExecutionResult m))
executeFrom blockHash slotNumber slotTime blockParent blockBaker mfinInfo newSeedState txs =
    let cm = ChainMetadata{..}
    in  do
            bshandle0 <- thawBlockState =<< blockState blockParent
            oldChainParameters <- bsoGetChainParameters bshandle0
            let accountCreationLim = oldChainParameters ^. cpAccountCreationLimit
            let counts = countFreeTransactions (map fst txs) (isJust mfinInfo)
            if countAccountCreation counts > accountCreationLim
                then return $ Left (Just ExceedsMaxCredentialDeployments)
                else do
                    PrologueResult{..} <- executeBlockPrologue slotTime newSeedState oldChainParameters bshandle0
                    maxBlockEnergy <- gdMaxBlockEnergy <$> getGenesisData
                    let context =
                            EnvImpl.ContextState
                                { _chainMetadata = cm,
                                  _maxBlockEnergy = maxBlockEnergy,
                                  _accountCreationLimit = accountCreationLim
                                }
                    (res, finState) <- EnvImpl.runSchedulerT (Sch.runTransactions txs) context (EnvImpl.makeInitialSchedulerState prologueBlockState)
                    let usedEnergy = finState ^. EnvImpl.ssEnergyUsed
                    let bshandle2 = finState ^. EnvImpl.ssBlockState
                    case res of
                        Left fk -> Left fk <$ dropUpdatableBlockState bshandle2
                        Right outcomes -> do
                            -- Record the transaction outcomes
                            bshandle3 <- bsoSetTransactionOutcomes bshandle2 (map snd outcomes)
                            -- Record transaction outcomes in the transaction table as well.
                            zipWithM_ (commitTransaction slotNumber blockHash . fst) txs [0 ..]
                            -- the main execution is now done. At this point we must mint new currency
                            -- and reward the baker and other parties.
                            bshandle4 <-
                                mintAndReward
                                    bshandle3
                                    blockParent
                                    slotNumber
                                    blockBaker
                                    (newSeedState ^. epoch)
                                    prologueMintRewardParams
                                    mfinInfo
                                    (finState ^. EnvImpl.ssExecutionCosts)
                                    counts
                                    prologueUpdates
                            finalbsHandle <- freezeBlockState bshandle4
                            return
                                ( Right
                                    ( ExecutionResult
                                        { _energyUsed = usedEnergy,
                                          _finalState = finalbsHandle
                                        }
                                    )
                                )

-- |PRECONDITION: Focus block is the parent block of the block we wish to make,
-- hence the pending transaction table is correct for the new block.
-- EFFECTS: This function only updates the block state. It has no effects on the transaction table.
-- POSTCONDITION: The function always returns a list of transactions which make a valid block in `ftAdded`,
-- and also returns a list of transactions which failed, and a list of those which were not processed.
constructBlock ::
    forall m.
    (BlockPointerMonad m, TreeStateMonad m, MonadLogger m, TimeMonad m) =>
    -- |Slot number of the block to bake
    Slot ->
    -- |Unix timestamp of the beginning of the slot.
    Timestamp ->
    -- |Parent pointer from which to start executing
    BlockPointerType m ->
    -- |The baker of the block.
    BakerId ->
    -- |Parties to the finalization record in this block, if any
    Maybe FinalizerInfo ->
    -- |New seed state
    SeedState (SeedStateVersionFor (MPV m)) ->
    m (Sch.FilteredTransactions, ExecutionResult m)
constructBlock slotNumber slotTime blockParent blockBaker mfinInfo newSeedState =
    let cm = ChainMetadata{..}
    in  do
            -- when we start constructing the block
            startTime <- currentTime
            bshandle0 <- thawBlockState =<< blockState blockParent
            oldChainParameters <- bsoGetChainParameters bshandle0
            let accountCreationLim = oldChainParameters ^. cpAccountCreationLimit
            -- Execute the block prologue
            PrologueResult{..} <- executeBlockPrologue slotTime newSeedState oldChainParameters bshandle0

            transactionGroups <- getGroupedPendingTransactions

            -- lookup the maximum block size as mandated by the runtime parameters
            maxSize <- rpBlockSize <$> getRuntimeParameters
            timeoutDuration <- rpBlockTimeout <$> getRuntimeParameters
            let timeout = addUTCTime (durationToNominalDiffTime timeoutDuration) startTime
            genData <- getGenesisData
            let maxBlockEnergy = gdMaxBlockEnergy genData
            let context =
                    EnvImpl.ContextState
                        { _chainMetadata = cm,
                          _maxBlockEnergy = maxBlockEnergy,
                          _accountCreationLimit = accountCreationLim
                        }
            (ft@Sch.FilteredTransactions{..}, finState) <-
                EnvImpl.runSchedulerT (Sch.filterTransactions (fromIntegral maxSize) timeout transactionGroups) context (EnvImpl.makeInitialSchedulerState prologueBlockState)

            -- FIXME: At some point we should log things here using the same logging infrastructure as in consensus.

            let usedEnergy = finState ^. EnvImpl.ssEnergyUsed
            let bshandle2 = finState ^. EnvImpl.ssBlockState

            bshandle3 <- bsoSetTransactionOutcomes bshandle2 (map snd ftAdded)
            let counts = countFreeTransactions (map (fst . fst) ftAdded) (isJust mfinInfo)
            bshandle4 <-
                mintAndReward
                    bshandle3
                    blockParent
                    slotNumber
                    blockBaker
                    (newSeedState ^. epoch)
                    prologueMintRewardParams
                    mfinInfo
                    (finState ^. EnvImpl.ssExecutionCosts)
                    counts
                    prologueUpdates

            bshandleFinal <- freezeBlockState bshandle4
            endTime <- currentTime
            logEvent Scheduler LLInfo $ "Constructed a block in " ++ show (diffUTCTime endTime startTime)
            return
                ( ft,
                  ExecutionResult
                    { _energyUsed = usedEnergy,
                      _finalState = bshandleFinal
                    }
                )
