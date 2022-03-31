{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
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

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Kind as DK
import qualified Data.PQueue.Prio.Min as MinPQ
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Word
import Control.Monad
import Concordium.TimeMonad
import Data.Time
import Data.Ratio

import Concordium.Types
import qualified Concordium.Types.Accounts as Types
import qualified Concordium.Types.Transactions as Types
import Concordium.Logger
import Concordium.GlobalState.Basic.BlockState.PoolRewards
import Concordium.GlobalState.CapitalDistribution
import Concordium.GlobalState.TreeState
import qualified Concordium.GlobalState.BakerInfo as BI
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Parameters
import Concordium.Types.SeedState
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.Scheduler.Types
import Concordium.Types.UpdateQueues (currentParameters)
import Concordium.Kontrol.Bakers
import Concordium.Scheduler.Environment
import Concordium.Scheduler.EnvironmentImplementation
    (BSOMonadWrapper(..),
     ContextState(..),
     HasSchedulerState(..),
     schedulerBlockState, schedulerEnergyUsed
     )
import qualified Concordium.TransactionVerification as TVer

import Control.Monad.RWS.Strict

import Lens.Micro.Platform

import qualified Concordium.Scheduler as Sch

newtype BlockStateMonad w state m a = BSM { _runBSM :: RWST ContextState w state m a}
    deriving (Functor, Applicative, Monad, MonadState state, MonadReader ContextState, MonadTrans, MonadWriter w, MonadLogger, TimeMonad)

deriving via (BSOMonadWrapper ContextState w state (MGSTrans (RWST ContextState w state) m))
    instance
        (SS state ~ UpdatableBlockState m, Monoid w, HasSchedulerState state,
        BlockStateOperations m, Footprint (ATIStorage m) ~ w)
             => StaticInformation (BlockStateMonad w state m)

instance (ATIStorage m ~ w, ATITypes m) => ATITypes (BlockStateMonad w state m) where
  type ATIStorage (BlockStateMonad w state m) = ATIStorage m

data LogSchedulerState (m :: DK.Type -> DK.Type) = LogSchedulerState {
  _lssBlockState :: !(UpdatableBlockState m),
  _lssSchedulerEnergyUsed :: !Energy,
  _lssSchedulerExecutionCosts :: !Amount,
  _lssNextIndex :: !TransactionIndex,
  _lssSchedulerTransactionLog :: !(ATIStorage m)
  }

makeLenses ''LogSchedulerState

-- This hack of ExecutionResult' and ExecutionResult is so that we
-- automatically get the property that if BlockState m = BlockState m'
-- then ExecutionResult m is interchangeable with ExecutionResult m'
data ExecutionResult' s ati = ExecutionResult{
  _finalState :: !s,
  _energyUsed :: !Energy,
  _transactionLog :: !ati
  }

type ExecutionResult m = ExecutionResult' (BlockState m) (ATIStorage m)

makeLenses ''ExecutionResult'

instance TreeStateMonad m => HasSchedulerState (LogSchedulerState m) where
  type SS (LogSchedulerState m) = UpdatableBlockState m
  type TransactionLog (LogSchedulerState m) = ATIStorage m
  schedulerBlockState = lssBlockState
  schedulerEnergyUsed = lssSchedulerEnergyUsed
  schedulerExecutionCosts = lssSchedulerExecutionCosts
  nextIndex = lssNextIndex
  schedulerTransactionLog = lssSchedulerTransactionLog

mkInitialSS :: CanExtend (ATIStorage m) => UpdatableBlockState m -> LogSchedulerState m
mkInitialSS _lssBlockState =
  LogSchedulerState{_lssSchedulerEnergyUsed = 0,
                    _lssSchedulerExecutionCosts = 0,
                    _lssSchedulerTransactionLog = defaultValue,
                    _lssNextIndex = 0,
                    ..}

deriving via (MGSTrans (RWST ContextState w state) m)
    instance (MonadProtocolVersion m) => MonadProtocolVersion (BlockStateMonad w state m)

deriving via (MGSTrans (RWST ContextState w state) m)
    instance BlockStateTypes (BlockStateMonad w state m)

deriving via (MGSTrans (RWST ContextState w state) m)
    instance (Monoid w, AccountOperations m) => AccountOperations (BlockStateMonad w state m)

deriving via (BSOMonadWrapper ContextState w state (MGSTrans (RWST ContextState w state) m))
    instance (
              SS state ~ UpdatableBlockState m,
              Footprint (ATIStorage m) ~ w,
              HasSchedulerState state,
              TreeStateMonad m,
              MonadLogger m,
              BlockStateOperations m) => SchedulerMonad (BlockStateMonad w state m)

deriving via (BSOMonadWrapper ContextState w state (MGSTrans (RWST ContextState w state) m))
    instance (
              SS state ~ UpdatableBlockState m,
              Footprint (ATIStorage m) ~ w,
              HasSchedulerState state,
              TreeStateMonad m,
              MonadLogger m,
              BlockStateOperations m) => TVer.TransactionVerifier (BlockStateMonad w state m)

runBSM :: Monad m => BlockStateMonad w b m a -> ContextState -> b -> m (a, b)
runBSM m cm s = do
  (r, s', _) <- runRWST (_runBSM m) cm s
  return (r, s')

-- |Distribute the baking rewards for the last epoch to the bakers of
-- blocks in that epoch. This should be called in the first block of
-- a new epoch. This resets the list of blocks baked in the epoch.
rewardLastEpochBakers :: (BlockStateOperations m, AccountVersionFor (MPV m) ~ 'AccountV0)
  => UpdatableBlockState m
  -> m (UpdatableBlockState m)
rewardLastEpochBakers bs0 = do
  rewards <- _bankRewardAccounts <$> bsoGetBankStatus bs0
  (totalBlocks, bakerShares) <- bsoGetEpochBlocksBaked bs0
  if totalBlocks == 0 then
    -- No blocks, so no reward.
    -- In practice, this should not happen.
    return bs0
  else do
    bs1 <- bsoClearEpochBlocksBaked bs0
    -- There are some blocks, so it will be safe to divide.
    let (perBlock, braRem) = (rewards ^. bakingRewardAccount) `divMod` fromIntegral totalBlocks
    bs2 <- bsoSetRewardAccounts bs1 (rewards & bakingRewardAccount .~ braRem)
    let rewardBaker (m,bs) (bid@(BakerId aid), blockCount) = do
          let brew = fromIntegral (blockCount :: Word64) * perBlock
          (macct,bs')  <- bsoRewardAccount bs aid brew
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
  Slot
  -- ^First slot to mint for
  -> Slot
  -- ^Last slot to mint for
  -> MintDistribution 'ChainParametersV0
  -- ^Initial mint distribution
  -> [(Slot, MintDistribution 'ChainParametersV0)]
  -- ^Ordered updates to the minting parameters
  -> Amount
  -- ^Total GTU
  -> MintAmounts
calculateMintAmounts  = go mempty
  where go !acc start end md0 upds tGTU = case upds of
          ((s,md1):upds')
            | s <= start -> go acc start end md1 upds' tGTU
            | s <= end -> let (tGTU', a1) = mintRange start (s-1) md0 tGTU
                        in go (a1 <> acc) s end md1 upds' tGTU'
          _ -> acc <> (snd $ mintRange start end md0 tGTU)

        mintRange s e md t =
          let mintSupply s' !m !t'
                  | s' <= e = let !a = mintAmount (md ^. mdMintPerSlot . mpsMintPerSlot) t' in mintSupply (s'+1) (m+a) (t'+a)
                  | otherwise = (m, t')
              (newMint, newTotal) = mintSupply s 0 t
              mintBakingReward = takeFraction (md ^. mdBakingReward) newMint
              mintFinalizationReward = takeFraction (md ^. mdFinalizationReward) newMint
              mintDevelopmentCharge = newMint - (mintBakingReward + mintFinalizationReward)
          in (newTotal, MintAmounts{..})

-- |Determine the amount and distribution of minting for one payday.
doCalculatePaydayMintAmounts ::
  MintDistribution 'ChainParametersV1
  -- ^Initial mint distribution
  -> MintRate
  -- ^Payday mint rate
  -> Amount
  -- ^Total GTU
  -> MintAmounts
doCalculatePaydayMintAmounts md mr amt =
  let newMint = mintAmount mr amt
      mintBakingReward = takeFraction (md ^. mdBakingReward) newMint
      mintFinalizationReward = takeFraction (md ^. mdFinalizationReward) newMint
      mintDevelopmentCharge = newMint - (mintBakingReward + mintFinalizationReward)
  in MintAmounts{..}

-- |Determine the amount and distribution of minting for one payday, taking updates to mint distribution and mint rate into account.
calculatePaydayMintAmounts ::
  MintDistribution 'ChainParametersV1
  -- ^Initial mint distribution
  -> MintRate
  -- ^Payday mint rate
  -> Slot
  -- ^Payday slot
  -> [(Slot, UpdateValue 'ChainParametersV1)]
  -- ^Changes to mint distribution or mint rate
  -> Amount
  -- ^Total GTU
  -> MintAmounts
calculatePaydayMintAmounts md mr ps updates amt =
  let selectBestSlotMintDistribution (s2, md2) (s1, UVMintDistribution md1) =
        if s1 >= s2 && s1 <= ps then (s1, md1) else (s2, md2)
      selectBestSlotMintDistribution a _ = a
      newMintDistribution = snd $ foldl' selectBestSlotMintDistribution (0, md) updates
  in doCalculatePaydayMintAmounts newMintDistribution mr amt

-- |Mint for all slots since the last block, recording a
-- special transaction outcome for the minting.
doMinting :: (ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0, BlockStateOperations m, BlockPointerMonad m)
  => BlockPointerType m
  -- ^Parent block
  -> Slot
  -- ^New slot
  -> AccountAddress
  -- ^Current foundation account
  -> [(Slot, MintDistribution 'ChainParametersV0)]
  -- ^Ordered updates to the minting parameters
  -> UpdatableBlockState m
  -- ^Block state
  -> m (UpdatableBlockState m)
doMinting blockParent slotNumber foundationAddr mintUpds bs0 = do
  parentUpdates <- getUpdates =<< blockState blockParent
  totGTU <- (^. totalGTU) <$> bsoGetBankStatus bs0
  let mint = calculateMintAmounts
              (blockSlot blockParent + 1)
              slotNumber
              (parentUpdates ^. currentParameters . rpMintDistribution)
              mintUpds
              totGTU
  bs1 <- bsoMint bs0 mint
  bsoAddSpecialTransactionOutcome bs1 Mint{
    stoMintBakingReward = mintBakingReward mint,
    stoMintFinalizationReward = mintFinalizationReward mint,
    stoMintPlatformDevelopmentCharge = mintDevelopmentCharge mint,
    stoFoundationAccount = foundationAddr
  }

-- |Mint for the given payday, recording a
-- special transaction outcome for the minting.
doMintingP4 :: (ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1, BlockStateOperations m)
  => ChainParameters (MPV m)
  -- ^Chain parameters
  -> Epoch
  -- ^Payday epoch to mint for
  -> MintRate
  -- ^Payday mint rate
  -> AccountAddress
  -- ^Current foundation account
  -> [(Slot, UpdateValue 'ChainParametersV1)]
  -- ^Ordered updates to the minting parameters
  -> UpdatableBlockState m
  -- ^Block state
  -> m (UpdatableBlockState m)
doMintingP4 oldChainParameters paydayEpoch paydayMintRate foundationAddr mintUpds bs0 = do
  totGTU <- (^. totalGTU) <$> bsoGetBankStatus bs0
  seedstate <- bsoGetSeedState bs0
  let mint = calculatePaydayMintAmounts
              (oldChainParameters ^. rpMintDistribution)
              paydayMintRate
              (epochLength seedstate * fromIntegral paydayEpoch)
              mintUpds
              totGTU
  bs1 <- bsoMint bs0 mint
  bsoAddSpecialTransactionOutcome bs1 Mint{
    stoMintBakingReward = mintBakingReward mint,
    stoMintFinalizationReward = mintFinalizationReward mint,
    stoMintPlatformDevelopmentCharge = mintDevelopmentCharge mint,
    stoFoundationAccount = foundationAddr
  }

-- |Info about finalizers used for distributing rewards.
data FinalizerInfo = FinalizerInfo {
        -- |List of the parties in the finalization committee, with their relative voting power.
        -- Finalization rewards are distributed in proportion to their power for protocol
        -- version <= 3.
        committeeVoterPower :: Vec.Vector (BakerId, VoterPower),
        -- |List of bakers who signed finalization proof. This is used for protocol version >= 4.
        committeeSigners :: [BakerId]
    }

-- |Distribute the finalization rewards to the finalizers
-- in proportion to their voting weight. This also adds a
-- special transaction outcome recording the reward.
doFinalizationRewards :: (BlockStateOperations m)
  => FinalizerInfo
  -> UpdatableBlockState m
  -> m (UpdatableBlockState m)
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
        bsoAddSpecialTransactionOutcome bs2 FinalizationRewards{
          stoFinalizationRewards = AccountAmounts awardMap,
          stoRemainder = remainder
        }
  where
    totalPower = sum (snd <$> committeeVoterPower finInfo)

-- |The counts of the various 'free' transactions for the
-- purposes of determining the block reward.
data FreeTransactionCounts = FreeTransactionCounts {
  -- |Number of credential deployment transactions.
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
doBlockReward :: forall m. (BlockStateOperations m, MonadProtocolVersion m, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0, AccountVersionFor (MPV m) ~ 'AccountV0)
  => Amount
  -- ^Transaction fees paid
  -> FreeTransactionCounts
  -- ^Counts of unpaid transactions
  -> BakerId
  -- ^Block baker
  -> AccountAddress
  -- ^Foundation account
  -> UpdatableBlockState m
  -- ^Block state
  -> m (UpdatableBlockState m)
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
        gasGAS = ceiling $ toRational gasIn
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasBaker)
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasAccountCreation)^countAccountCreation
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasChainUpdate)^countUpdate
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasFinalizationProof)^countFinRecs
        bakerGAS = gasIn - gasGAS
        gasOut = gasFees + gasGAS
        bakerOut = bakerFees + bakerGAS
    bs1 <- bsoSetRewardAccounts bs0 (oldRewardAccts & gasAccount .~ gasOut)
    bs2 <- bsoRewardFoundationAccount bs1 platformFees
    (mbkr, bs3) <- bsoRewardAccount bs2 aid bakerOut
    bkr <- case mbkr of
      Nothing -> error "Invalid baker account"
      Just bkr -> return bkr
    bsoAddSpecialTransactionOutcome bs3 BlockReward{
      stoTransactionFees = transFees,
      stoOldGASAccount = gasIn,
      stoNewGASAccount = gasOut,
      stoBakerReward = bakerOut,
      stoFoundationCharge = platformFees,
      stoBaker = bkr,
      stoFoundationAccount = foundationAddr
    }

-- |Accrue the rewards for a block to the relevant pool, the L-pool, and the foundation.
doBlockRewardP4 :: forall m. (BlockStateOperations m, MonadProtocolVersion m, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1, AccountVersionFor (MPV m) ~ 'AccountV1)
  => Amount
  -- ^Transaction fees paid
  -> FreeTransactionCounts
  -- ^Counts of unpaid transactions
  -> BakerId
  -- ^Block baker
  -> UpdatableBlockState m
  -- ^Block state
  -> m (UpdatableBlockState m)
doBlockRewardP4 transFees FreeTransactionCounts{..} bid bs0 = do
    chainParameters <- bsoGetChainParameters bs0
    capitalDistribution <- bsoGetCurrentCapitalDistribution bs0
    bakers <- bsoGetCurrentEpochBakers bs0
    let rewardParams = chainParameters ^. rewardParameters
        lPoolTransCommission = chainParameters ^. cpPoolParameters . ppLPoolCommissions . transactionCommission
    oldRewardAccts <- (^. rewardAccounts) <$> bsoGetBankStatus bs0
    let gasIn = oldRewardAccts ^. gasAccount
        poolsAndLpoolFees = takeFraction (rewardParams ^. tfdBaker) transFees
        gasFees = takeFraction (rewardParams ^. tfdGASAccount) transFees
        lPoolStake = sum $ dcDelegatorCapital <$> lPoolCapital capitalDistribution
        lPoolRelativeStake = lPoolStake % (BI.bakerTotalStake bakers + lPoolStake)
        lPoolFraction = fractionToRational (complementAmountFraction lPoolTransCommission) * toRational lPoolRelativeStake
        lPoolGASFees = floor $ toRational gasFees * lPoolFraction
        lPoolTransFees = floor $ toRational poolsAndLpoolFees * lPoolFraction
        lPoolOut = lPoolGASFees + lPoolTransFees
        poolFees = poolsAndLpoolFees - lPoolOut
        platformFees = transFees - (poolsAndLpoolFees + gasFees)
        gasGAS = ceiling $ toRational gasIn
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasBaker)
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasAccountCreation)^countAccountCreation
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasChainUpdate)^countUpdate
                    * (fractionToRational . complementAmountFraction $ rewardParams ^. gasFinalizationProof)^countFinRecs
        bakerGAS = gasIn - gasGAS
        gasOut = gasFees - lPoolGASFees + gasGAS
        poolOut = poolFees + bakerGAS
    bs1 <- bsoSetRewardAccounts bs0 (oldRewardAccts & gasAccount .~ gasOut)
    bs2 <- bsoUpdateAccruedTransactionFeesFoundationAccount bs1 (amountToDelta platformFees)
    bs3 <- bsoUpdateAccruedTransactionFeesLPool bs2 (amountToDelta lPoolOut)
    bs4 <- bsoUpdateAccruedTransactionFeesBaker bs3 bid (amountToDelta poolOut)
    bsoAddSpecialTransactionOutcome bs4 BlockAccrueReward{
      stoTransactionFees = transFees,
      stoOldGASAccount = gasIn,
      stoNewGASAccount = gasOut,
      stoBakerReward = poolOut,
      stoLPoolReward = lPoolOut,
      stoFoundationCharge = platformFees,
      stoBakerId = bid
    }


-- |Accumulated rewards and outcomes to a set of delegators.
data DelegatorRewardOutcomes = DelegatorRewardOutcomes
    { -- |Total finalization rewards distributed.
      _delegatorAccumFinalization :: !Amount,
      -- |Total baking rewards distributed.
      _delegatorAccumBaking :: !Amount,
      -- |Total transaction fees distributed.
      _delegatorAccumTransaction :: !Amount,
      -- |Accumulated transaction outcomes.
      _delegatorOutcomes :: Seq.Seq Types.SpecialTransactionOutcome
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
    ( AccountVersionFor (MPV m) ~ 'AccountV1,
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      BlockStateOperations m,
      TreeStateMonad m
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
        let -- c_{D,P} = capital_{D,P} / capital_P
            relativeCapital = dcDelegatorCapital dc % totalCapital
            finalizationReward = floor $ relativeCapital * fromIntegral totalFinalizationReward
            bakingReward = floor $ relativeCapital * fromIntegral totalBakingReward
            transactionReward = floor $ relativeCapital * fromIntegral totalTransactionReward
            reward = finalizationReward + bakingReward + transactionReward
        if reward == 0
            then return (accumRewardOutcomes, bs1)
            else do
                (mAcct, bs2) <- bsoRewardAccount bs1 (delegatorAccountIndex $ dcDelegatorId dc) reward
                let acct = case mAcct of
                        Nothing -> error $ "Invariant violation: delegator account for " ++ show (dcDelegatorId dc) ++ " does not exist"
                        Just a -> a
                let delegatorOutcome =
                        PaydayAccountReward
                            { stoAccount = acct,
                              stoTransactionFees = transactionReward,
                              stoBakerReward = bakingReward,
                              stoFinalizationReward = finalizationReward
                            }
                return
                    ( accumRewardOutcomes & delegatorAccumFinalization +~ finalizationReward
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
    ( AccountVersionFor (MPV m) ~ 'AccountV1,
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      BlockStateOperations m,
      TreeStateMonad m
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
    -- |The total number of blocks baked in the reward period
    Word64 ->
    m (BakerRewardOutcomes, UpdatableBlockState m)
rewardBakers bs bakers bakerTotalBakingRewards bakerTotalFinalizationRewards bcs paydayBlockCount
    | paydayBlockCount == 0 = return (emptyRewardOutcomes, bs)
    | otherwise = do
        -- Compute the total finalization stake.
        -- This is used for distributing finalization rewards in proportion to stake.
        totalFinStake <- foldM addFinalizationStake 0 bcs
        foldM (rewardBaker totalFinStake) (emptyRewardOutcomes, bs) bcs
  where
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

    -- Accumulate the finalization stake for a baker.
    addFinalizationStake a bc = do
        bprd <- bsoGetBakerPoolRewardDetails bs (bcBakerId bc)
        -- The finalization stake is just the stake if the pool is awake, and 0 otherwise.
        if finalizationAwake bprd
            then case Map.lookup (bcBakerId bc) bakerStakesAndRatesMap of
                Nothing ->
                    error "Invariant violation: baker from capital distribution is not an epoch baker"
                Just (s, _) ->
                    return (a + s)
            else return a

    -- Reward a baker pool, accumulating the baking and finalization reward totals and reward outcomes.
    rewardBaker totalFinStake (accumRewards, bsIn) bc = do
        bprd <- bsoGetBakerPoolRewardDetails bsIn (bcBakerId bc)
        let poolTransactionReward = transactionFeesAccrued bprd
            bakerBlockCount = blockCount bprd
            bakerBlockFraction = bakerBlockCount % paydayBlockCount
            poolBakingReward = floor $ fromIntegral bakerTotalBakingRewards * bakerBlockFraction
            finalized = finalizationAwake bprd
            -- The relativeFinalizationStake (fs_P) is the ratio of the finalization stake to the
            -- total finalization stake. The finalization stake (fstake_P) is the stake if the baker
            -- is awake for finalization, and 0 otherwise.
            (relativeFinalizationStake, commissionRates) = case Map.lookup (bcBakerId bc) bakerStakesAndRatesMap of
                Nothing ->
                    error "Invariant violation: baker from capital distribution is not an epoch baker"
                Just (s, commRates) ->
                    ( if finalized && totalFinStake /= 0 -- It should never be that totalFinStake == 0
                        then s % totalFinStake
                        else 0,
                      commRates
                    )
            -- R_{F,P} = (R_F - R_{F,L})·fs_P
            poolFinalizationReward = floor $ fromIntegral bakerTotalFinalizationRewards * relativeFinalizationStake
            -- capital_P -- the total capital of the pool
            totalPoolCapital = bcBakerEquityCapital bc + sum (dcDelegatorCapital <$> bcDelegatorCapital bc)
        -- We do not log an event (or issue rewards) if the rewards would be 0.
        if poolBakingReward == 0 && poolFinalizationReward == 0 && poolTransactionReward == 0 then
          return (accumRewards, bsIn)
        else do
          let -- 1 - μ_{F,P}
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
          -- Note: we do not reset the transaction fees, awake status or block count for the pool here.
          -- This is handled in 'prepareForFollowingPayday' by calling
          -- 'bsoRotateCurrentCapitalDistribution'.
          return
              ( accumRewards &
                  bakerAccumFinalization +~ poolFinalizationReward
                  & bakerAccumBaking +~ poolBakingReward
                  & bakerOutcomes %~ (<> (poolOutcome Seq.:<| bakerOutcome Seq.:<| _delegatorOutcomes)),
                bsBak
              )


-- |Distribute the rewards that have accrued as part of a payday.
distributeRewards
  :: forall m
   . (AccountVersionFor (MPV m) ~ 'AccountV1,
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      BlockStateOperations m,
      TreeStateMonad m)
  => AccountAddress -- ^Foundation account address
  -> UpdatableBlockState m
  -> m (UpdatableBlockState m)
distributeRewards foundationAddr bs0 = do
  -- The chain parameters are used to determine the L-pool finalization and baking commission rates.
  -- Note: unlike baker pool commission rates, these are not frozen at the prior to the reward period.
  lPoolCommissions <- (^. cpPoolParameters . ppLPoolCommissions) <$> bsoGetChainParameters bs0
  rewardAccts <- (^. bankRewardAccounts) <$> bsoGetBankStatus bs0
  lPoolAccrued <- bsoGetAccruedTransactionFeesLPool bs0
  capitalDistribution <- bsoGetCurrentCapitalDistribution bs0
  bakers <- bsoGetCurrentEpochFullBakersEx bs0
  let -- stake_L -- The L-pool stake is the total capital delegated to the L-pool
      lPoolStake = sum $ dcDelegatorCapital <$> lPoolCapital capitalDistribution
      -- s_L -- The relative stake of the L-pool; s_L = stake_L / (stake + stake_L)
      lPoolRelativeStake = lPoolStake % (BI.bakerPoolTotalStake bakers + lPoolStake)
      -- Finalization commission rate for the L-pool
      lPoolFinalizationCommission = lPoolCommissions ^. finalizationCommission
      -- (1 - μ_{F,L})·s_L -- the L-pool fraction of the finalization rewards
      lPoolFinalizationFraction = fractionToRational (complementAmountFraction lPoolFinalizationCommission) * toRational lPoolRelativeStake
      -- R_{F,L} = R_F·(1 - μ_{F,L})·s_L -- the L-pool finalization rewards. Rounded down.
      lPoolFinalizationReward = floor $ fromIntegral (rewardAccts ^. finalizationRewardAccount) * lPoolFinalizationFraction
      -- Baking commission rate for the L-pool
      lPoolBakingCommission = lPoolCommissions ^. bakingCommission
      -- (1 - μ_{B,L))·s_L -- the L-pool fraction of the baking rewards
      lPoolBakingFraction = fractionToRational (complementAmountFraction lPoolBakingCommission) * toRational lPoolRelativeStake
      -- R_{B,L} = R_B·(1 - μ_{B,L})·s_L -- the L-pool baking rewards. Rounded down.
      lPoolBakingReward = floor $ fromIntegral (rewardAccts ^. bakingRewardAccount) * lPoolBakingFraction
  -- Reward the L-pool delegators.
  (lpoolRes, bs1) <-
    rewardDelegators bs0 lPoolFinalizationReward lPoolBakingReward lPoolAccrued lPoolStake (lPoolCapital capitalDistribution)
  -- Subtract the rewarded transaction fees from those accrued to the L-pool. There result can be
  -- positive due to rounding down the rewards to each delegator.
  bs2 <- bsoUpdateAccruedTransactionFeesLPool bs1 (amountDiff 0 (lpoolRes ^. delegatorAccumTransaction))
  -- (R_B - R_{B,L}) -- Total baking rewards due to baking pools
  let bakerTotalBakingRewards = rewardAccts ^. bakingRewardAccount - lPoolBakingReward
  -- (R_F - R_{F,L}) -- Total finalization rewards due to baking pools
  let bakerTotalFinalizationRewards = rewardAccts ^. finalizationRewardAccount - lPoolFinalizationReward
  -- B -- the total number of blocks produced in the reward period
  paydayBlocks <- bsoGetTotalRewardPeriodBlockCount bs2
  -- Reward each baking pool, including the pool owner and delegators.
  -- This accumulates the total baking and finalization reward payout to baking pools, as well
  -- as outcomes for each (non-zero) reward.
  (BakerRewardOutcomes{..}, bs3) <-
    rewardBakers bs2 bakers bakerTotalBakingRewards bakerTotalFinalizationRewards (bakerPoolCapital capitalDistribution) paydayBlocks
  -- To update the finalization reward account and the baking reward account, we subtract the amounts
  -- paid out from each.
  let newRewardAccts = rewardAccts
        & finalizationRewardAccount -~ ((lpoolRes ^. delegatorAccumFinalization) + _bakerAccumFinalization)
        & bakingRewardAccount -~ ((lpoolRes ^. delegatorAccumBaking) + _bakerAccumBaking)
  bs4 <- bsoSetRewardAccounts bs3 newRewardAccts
  -- Pay out to the foundation the transaction fees accrued to it.
  -- Note, the foundation receives its share of the baking and finalization rewards as part of the
  -- minting, which is handled in 'doMintingP4'.
  accruedFoundation <- bsoGetAccruedTransactionFeesFoundationAccount bs4
  bs5 <- bsoRewardFoundationAccount bs4 accruedFoundation
  bs6 <- bsoUpdateAccruedTransactionFeesFoundationAccount bs5 (amountDiff 0 accruedFoundation)
  -- Log the special outcomes for the reward payouts.
  bs7 <- bsoAddSpecialTransactionOutcome bs6 PaydayFoundationReward{
    stoFoundationAccount = foundationAddr,
    stoDevelopmentCharge = accruedFoundation
  }
  bs8 <- bsoAddSpecialTransactionOutcome bs7 PaydayPoolReward{
      stoPoolOwner = Nothing,
      stoTransactionFees = lPoolAccrued,
      stoBakerReward = lPoolBakingReward,
      stoFinalizationReward = lPoolFinalizationReward
    }
  bs9 <- foldM bsoAddSpecialTransactionOutcome bs8 (lpoolRes ^. delegatorOutcomes)
  foldM bsoAddSpecialTransactionOutcome bs9 _bakerOutcomes

-- |Get the updated value of the time parameters at a given slot, given the original time
-- parameters and the elapsed updates.
bestTimeParameters ::
  -- |Target slot
  Slot ->
  -- |Original time parameters
  TimeParameters cpv ->
  -- |Updates
  [(Slot, UpdateValue cpv)] ->
  TimeParameters cpv
bestTimeParameters targetSlot tp0 upds =
    fromMaybe tp0 $
        getLast $
            mconcat
                [Last (Just tp) | (slot, UVTimeParameters tp) <- upds, slot <= targetSlot]

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
mintForSkippedPaydays
  :: (ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      BlockStateOperations m)
  => Epoch
  -- ^Epoch of the current block
  -> Epoch
  -- ^Epoch of the most recently minted payday
  -> ChainParameters (MPV m)
  -- ^Chain parameters at the parent block
  -> AccountAddress
  -- ^Foundation account address
  -> [(Slot, UpdateValue 'ChainParametersV1)]
  -- ^Updates processed in this block
  -> UpdatableBlockState m
  -- ^Block state
  -> m (Epoch, MintRate, UpdatableBlockState m)
mintForSkippedPaydays newEpoch payday oldChainParameters foundationAccount updates bs0 = do
  seedstate <- bsoGetSeedState bs0
  let paydaySlot = epochLength seedstate * fromIntegral payday
      bestTP = bestTimeParameters paydaySlot (oldChainParameters ^. cpTimeParameters) updates
      nextMintRate = bestTP ^. tpMintPerPayday
      nextRPL = bestTP ^. tpRewardPeriodLength
      nextPayday = payday + rewardPeriodEpochs nextRPL
  if newEpoch < nextPayday
  then return (nextPayday, nextMintRate, bs0)
  else do
    bs1 <- doMintingP4 oldChainParameters nextPayday nextMintRate foundationAccount updates bs0
    mintForSkippedPaydays newEpoch nextPayday oldChainParameters foundationAccount updates bs1

prepareForFollowingPayday
  :: (ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      AccountVersionFor (MPV m) ~ 'AccountV1,
      GlobalStateTypes m,
      BlockStateOperations m,
      BlockPointerMonad m)
  => Epoch
  -- ^Epoch of the current block
  -> Epoch
  -- ^Epoch of the current payday
  -> ChainParameters (MPV m)
  -- ^Chain parameters at the parent block
  -> AccountAddress
  -- ^Foundation account address
  -> [(Slot, UpdateValue 'ChainParametersV1)]
  -- ^Ordered chain updates since the last block
  -> UpdatableBlockState m
  -- ^Block state
  -> m (UpdatableBlockState m)
prepareForFollowingPayday newEpoch payday oldChainParameters foundationAccount updates bs0 = do
  (newPayday, newMintRate, bs1) <- mintForSkippedPaydays newEpoch payday oldChainParameters foundationAccount updates bs0
  bs2 <- bsoSetPaydayEpoch bs1 newPayday
  bs3 <- bsoSetPaydayMintRate bs2 newMintRate
  -- Update the current capital distribution based on the next capital distribution.
  -- This also resets the accumulated rewards for each pool.
  bs4 <- bsoRotateCurrentCapitalDistribution bs3
  -- Update the current bakers based on the next bakers.
  -- This is always kept in sync with the capital distribution.
  bsoRotateCurrentEpochBakers bs4

-- |Find committee signers in 'FinalizerInfo' and mark them as awake finalizers.
addAwakeFinalizers
    :: (BlockStateOperations m, AccountVersionFor (MPV m) ~ 'AccountV1)
    => Maybe FinalizerInfo
    -> UpdatableBlockState m
    -> m (UpdatableBlockState m)
addAwakeFinalizers Nothing bs = return bs
addAwakeFinalizers (Just FinalizerInfo{..}) bs0 =
    foldM (\bs bid -> bsoMarkFinalizationAwakeBaker bs bid) bs0 committeeSigners

-- |Mint new tokens and distribute rewards to bakers, finalizers and the foundation.
-- The process consists of the following four steps:
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
mintAndReward :: forall m. (BlockStateOperations m, TreeStateMonad m, MonadProtocolVersion m)
    => UpdatableBlockState m
    -- ^Block state
    -> BlockPointerType m
    -- ^Parent block
    -> Slot
    -- ^Block slot
    -> BakerId
    -- ^Baker ID
    -> Epoch
    -- ^Epoch of the new block
    -> Bool
    -- ^Is a new epoch
    -> Maybe FinalizerInfo
    -- ^Info on finalization committee for included record, if any
    -> Amount
    -- ^Transaction fees
    -> FreeTransactionCounts
    -- ^Number of "free" transactions of each type
    -> [(Slot, UpdateValue (ChainParametersVersionFor (MPV m)))]
    -- ^Ordered chain updates since the last block
    -> m (UpdatableBlockState m)
mintAndReward bshandle blockParent slotNumber bid newEpoch isNewEpoch mfinInfo transFees freeCounts updates =
  case protocolVersion @(MPV m) of
    SP1 -> mintAndRewardCPV0AccountV0
    SP2 -> mintAndRewardCPV0AccountV0
    SP3 -> mintAndRewardCPV0AccountV0
    SP4 -> mintAndRewardCPV1AccountV1
  where
    mintAndRewardCPV0AccountV0
        :: (AccountVersionFor (MPV m) ~ 'AccountV0,
            ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0)
        => m (UpdatableBlockState m)
    mintAndRewardCPV0AccountV0 = do
      -- First, reward bakers from previous epoch, if we are starting a new one.
      bshandleEpoch <- (if isNewEpoch then rewardLastEpochBakers else return) bshandle
        -- Add the block to the list of blocks baked in this epoch
        >>= flip bsoNotifyBlockBaked bid

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

    mintAndRewardCPV1AccountV1
        :: (AccountVersionFor (MPV m) ~ 'AccountV1,
            ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1)
        => m (UpdatableBlockState m)
    mintAndRewardCPV1AccountV1 = do
      foundationAccount <- getAccountCanonicalAddress =<< bsoGetFoundationAccount bshandle
      nextPayday <- bsoGetPaydayEpoch bshandle
      bshandleFinRew <- if newEpoch < nextPayday
        then return bshandle
        else do
          nextMintRate <- bsoGetPaydayMintRate bshandle
          oldChainParameters <- (^. currentParameters) <$> (getUpdates =<< blockState blockParent)
          bshandleMint <- doMintingP4 oldChainParameters nextPayday nextMintRate foundationAccount updates bshandle
          bshandleRewards <- distributeRewards foundationAccount bshandleMint
          prepareForFollowingPayday newEpoch nextPayday oldChainParameters foundationAccount updates bshandleRewards
      bshandleFinAwake <- addAwakeFinalizers mfinInfo bshandleFinRew
      bshandleNotify <- bsoNotifyBlockBaked bshandleFinAwake bid
      doBlockRewardP4 transFees freeCounts bid bshandleNotify

-- |Update the bakers and seed state of the block state.
-- The epoch for the new seed state must be at least the epoch of
-- the old seed state (currently on the block state).
-- If the epoch is new, the old bakers are rewarded with the balance
-- of the baking reward account, in proportion to the number of blocks
-- they baked in that epoch.
--
-- If the new epoch is not the same as or direct successor of the current
-- one, then 'bsoTransitionEpochBakers' is called twice. This should be
-- sufficient because the bakers will not change in the intervening epochs.
--
-- The return value is @True@ if the block is the first in a new epoch.
updateBirkParameters :: forall m. (BlockStateOperations m, TreeStateMonad m, MonadProtocolVersion m)
  => SeedState
  -- ^New seed state
  -> UpdatableBlockState m
  -- ^Block state
  -> ChainParameters (MPV m)
  -- ^Chain parameters at the previous block
  -> [(Slot, UpdateValue (ChainParametersVersionFor (MPV m)))]
  -- ^Chain updates since the previous block
  -> m (Bool, UpdatableBlockState m)
updateBirkParameters newSeedState bs0 oldChainParameters updates = case protocolVersion @(MPV m) of
  SP1 -> updateCPV0AccountV0
  SP2 -> updateCPV0AccountV0
  SP3 -> updateCPV0AccountV0
  SP4 -> updateCPV1AccountV1
  where
    updateCPV0AccountV0 :: AccountVersionFor (MPV m) ~ 'AccountV0 => m (Bool, UpdatableBlockState m)
    updateCPV0AccountV0 = do
      oldSeedState <- bsoGetSeedState bs0
      let isNewEpoch = epoch oldSeedState /= epoch newSeedState
      bs1 <- if isNewEpoch
        then do
          upToLast <- if epoch oldSeedState /= epoch newSeedState - 1
            then bsoTransitionEpochBakers bs0 (epoch newSeedState - 1)
            else return bs0
          bsoTransitionEpochBakers upToLast (epoch newSeedState)
        else
          return bs0
      (isNewEpoch,) <$> bsoSetSeedState bs1 newSeedState
    updateCPV1AccountV1 :: (AccountVersionFor (MPV m) ~ 'AccountV1, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1) => m (Bool, UpdatableBlockState m)
    updateCPV1AccountV1 = do
      oldSeedState <- bsoGetSeedState bs0
      if epoch oldSeedState == epoch newSeedState then
        (False,) <$> bsoSetSeedState bs0 newSeedState
      else do
        -- This is the start of a new epoch.
        -- Assume: epoch oldSeedState < epoch newSeedState
        payday <- bsoGetPaydayEpoch bs0
        let oldTPs = oldChainParameters ^. cpTimeParameters
            computePaydays lastPayday fromPayday hitEpochBeforePayday0
              | epoch newSeedState < fromPayday = (lastPayday, fromPayday, hitEpochBeforePayday1)
              | otherwise = computePaydays (Just fromPayday) nextPayday hitEpochBeforePayday1
                  where
                    !nextPayday = fromPayday +
                      bestTimeParameters (slotFor fromPayday) oldTPs updates
                        ^. tpRewardPeriodLength . to rewardPeriodEpochs
                    slotFor = (epochLength oldSeedState *) . fromIntegral
                    !hitEpochBeforePayday1 = hitEpochBeforePayday0 ||
                      (epoch oldSeedState + 1 < fromPayday && epoch newSeedState + 1 >= fromPayday)
            (mLastElapsedPayday, futurePayday, hitEpochBeforePayday) = computePaydays Nothing payday False
        -- Here: epoch newSeedState < futurePayday, payday =< futurePayday
       -- 'hitEpochBeforePayday' checks if (old block epoch, new block epoch] contains an epoch
        -- that is one before a payday. In such a case, we need to compute the next bakers.
        bs1 <- if hitEpochBeforePayday
          then
            -- We generate the bakers for the next reward period based on the epoch before the payday.
            -- First, compute what the most recent payday will be in one epoch from now.
            let bakersUpdatePayday
                  | epoch newSeedState + 1 == futurePayday = futurePayday
                  | (Just lastElapsed) <- mLastElapsedPayday = lastElapsed
                    -- This last case cannot happen because if @mLastElapsedPayday == Nothing@ then
                    -- it must be that @epoch newSeedState < payday@, @epoch newSeedState + 1 >= payday@
                    -- and @futurePayday == payday@.
                  | otherwise = payday
            in generateNextBakers bakersUpdatePayday bs0
          else return bs0
        -- If this is the first block after a payday, we process the pending changes on bakers and
        -- delegators (i.e. unlock stake that exits cooldown).
        bs2 <- case mLastElapsedPayday of
          Nothing -> return bs1
          Just lastElapsed -> do
            et <- effectiveTest lastElapsed
            bsoProcessPendingChanges bs1 et
        (True,) <$> bsoSetSeedState bs2 newSeedState

-- |Count the free transactions of each kind in a list.
-- The second argument indicates if the block contains a finalization record.
countFreeTransactions :: [BlockItem] -> Bool -> FreeTransactionCounts
countFreeTransactions bis hasFinRec = foldl' cft f0 bis
  where
    f0 = FreeTransactionCounts {
      countAccountCreation = 0,
      countUpdate = 0,
      countFinRecs = if hasFinRec then 1 else 0
    }
    cft f bi = case wmdData bi of
      CredentialDeployment{} -> let !c = countAccountCreation f + 1 in f {countAccountCreation = c}
      ChainUpdate{} -> let !c = countUpdate f + 1 in f {countUpdate = c}
      _ -> f

-- |Given 'CommissionRanges' and a 'BakerId', update the baker's commission rates to fall within
-- the ranges (at the closest rate to the existing rate).
putBakerCommissionsInRange ::
    (ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
    AccountVersionFor (MPV m) ~ 'AccountV1,
    BlockStateOperations m) =>
    CommissionRanges ->
    UpdatableBlockState m ->
    BakerId ->
    m (UpdatableBlockState m)
putBakerCommissionsInRange ranges bs (BakerId ai) = bsoConstrainBakerCommission bs ai ranges

-- |Execute a block from a given starting state.
-- Fail if any of the transactions fails, otherwise return the new 'BlockState' and the amount of energy used
-- during this block execution.
--
-- The slot number must exceed the slot of the parent block, and the seed state
-- must indicate the correct epoch of the block.
executeFrom :: forall m.
  (BlockPointerMonad m, TreeStateMonad m, MonadLogger m)
  => BlockHash -- ^Hash of the block we are executing. Used only for committing transactions.
  -> Slot -- ^Slot number of the block being executed.
  -> Timestamp -- ^Unix timestamp of the beginning of the slot.
  -> BlockPointerType m  -- ^Parent pointer from which to start executing
  -> BakerId -- ^Identity of the baker who should be rewarded.
  -> Maybe FinalizerInfo -- ^Parties to the finalization record in this block, if any
  -> SeedState -- ^New seed state
  -> [TVer.BlockItemWithStatus] -- ^Transactions on this block with their associated verification result.
  -> m (Either (Maybe FailureKind) (ExecutionResult m))
executeFrom blockHash slotNumber slotTime blockParent blockBaker mfinInfo newSeedState txs =
  let cm = ChainMetadata{..}
  in do
    bshandle0 <- thawBlockState =<< blockState blockParent
    oldChainParameters <- bsoGetChainParameters bshandle0
    let accountCreationLim = oldChainParameters ^. cpAccountCreationLimit
    let counts = countFreeTransactions (map fst txs) (isJust mfinInfo)
    if countAccountCreation counts > accountCreationLim
      then
          return $ Left (Just ExceedsMaxCredentialDeployments)
      else do
        -- process the update queues
        (updates, bshandle0a) <- bsoProcessUpdateQueues bshandle0 slotTime
        genData <- getGenesisData
        let updates' = (_1 %~ transactionTimeToSlot (gdGenesisTime genData) (gdSlotDuration genData))
                        <$> Map.toAscList updates
        ab <- bsoGetActiveBakers bshandle0a
        -- for each pool parameter update, go over all bakers and put their commissions inside
        -- the new commission ranges.
        bshandle0a' <- foldM (\bs uv -> case uv of
          UVPoolParameters PoolParametersV1{..} -> case protocolVersion @(MPV m) of
              SP4 -> foldM (putBakerCommissionsInRange _ppCommissionBounds) bs ab
          _ -> return bs) bshandle0a (Map.elems updates)
        -- unlock the amounts that have expired
        bshandle0b <- bsoProcessReleaseSchedule bshandle0a' slotTime
        -- update the bakers and seed state
        (isNewEpoch, bshandle1) <- updateBirkParameters newSeedState bshandle0b oldChainParameters updates'
        maxBlockEnergy <- gdMaxBlockEnergy <$> getGenesisData
        let context = ContextState{
              _chainMetadata = cm,
              _maxBlockEnergy = maxBlockEnergy,
              _accountCreationLimit = accountCreationLim
              }
        (res, finState) <- runBSM (Sch.runTransactions txs) context (mkInitialSS bshandle1 :: LogSchedulerState m)
        let usedEnergy = finState ^. schedulerEnergyUsed
        let bshandle2 = finState ^. schedulerBlockState
        case res of
            Left fk -> Left fk <$ dropUpdatableBlockState bshandle2
            Right outcomes -> do
                -- Record the transaction outcomes
                bshandle3 <- bsoSetTransactionOutcomes bshandle2 (map snd outcomes)
                -- Record transaction outcomes in the transaction table as well.
                zipWithM_ (commitTransaction slotNumber blockHash . fst) txs [0..]
                -- the main execution is now done. At this point we must mint new currency
                -- and reward the baker and other parties.
                bshandle4 <- mintAndReward bshandle3 blockParent slotNumber blockBaker (epoch newSeedState) isNewEpoch mfinInfo (finState ^. schedulerExecutionCosts) counts updates'
                finalbsHandle <- freezeBlockState bshandle4
                return (Right (ExecutionResult{_energyUsed = usedEnergy,
                                              _finalState = finalbsHandle,
                                              _transactionLog = finState ^. schedulerTransactionLog}))

-- |PRECONDITION: Focus block is the parent block of the block we wish to make,
-- hence the pending transaction table is correct for the new block.
-- EFFECTS: This function only updates the block state. It has no effects on the transaction table.
-- POSTCONDITION: The function always returns a list of transactions which make a valid block in `ftAdded`,
-- and also returns a list of transactions which failed, and a list of those which were not processed.
constructBlock :: forall m.
  (BlockPointerMonad m, TreeStateMonad m, MonadLogger m, TimeMonad m)
  => Slot -- ^Slot number of the block to bake
  -> Timestamp -- ^Unix timestamp of the beginning of the slot.
  -> BlockPointerType m -- ^Parent pointer from which to start executing
  -> BakerId -- ^The baker of the block.
  -> Maybe FinalizerInfo -- ^Parties to the finalization record in this block, if any
  -> SeedState -- ^New seed state
  -> m (Sch.FilteredTransactions, ExecutionResult m)
constructBlock slotNumber slotTime blockParent blockBaker mfinInfo newSeedState =
  let cm = ChainMetadata{..}
  in do
    -- when we start constructing the block
    startTime <- currentTime
    bshandle0 <- thawBlockState =<< blockState blockParent
    oldChainParameters <- bsoGetChainParameters bshandle0
    let accountCreationLim = oldChainParameters ^. cpAccountCreationLimit
    -- process the update queues
    (updates, bshandle0a) <- bsoProcessUpdateQueues bshandle0 slotTime
    genData <- getGenesisData
    let updates' = (_1 %~ transactionTimeToSlot (gdGenesisTime genData) (gdSlotDuration genData))
                    <$> Map.toAscList updates
    ab <- bsoGetActiveBakers bshandle0a
    -- for each pool parameter update, go over all bakers and put their commissions inside
    -- the new commission ranges.
    bshandle0a' <- foldM (\bs uv -> case uv of
      UVPoolParameters PoolParametersV1{..} -> case protocolVersion @(MPV m) of
          SP4 -> foldM (putBakerCommissionsInRange _ppCommissionBounds) bs ab
      _ -> return bs) bshandle0a (Map.elems updates)
    -- unlock the amounts that have expired
    bshandle0b <- bsoProcessReleaseSchedule bshandle0a' slotTime
    -- update the bakers and seed state
    (isNewEpoch, bshandle1) <- updateBirkParameters newSeedState bshandle0b oldChainParameters updates'
    pt <- getPendingTransactions

    -- Prioritise the block items for inclusion in a block.
    -- We do this by building a priority queue, keyed by arrival time,
    -- consisting of:
    -- - each credential, keyed by its arrival time
    -- - the pending transactions for each account with pending transactions,
    --   keyed by the lowest arrival time of a transaction with the lowest nonce.
    -- - the pending update instructions for each update type, keyed by the lowest
    --   arrival time of an update with the lowest sequence number.

    -- getCredential shouldn't return Nothing based on the transaction table invariants
    credentials <- mapM getCredential (HashSet.toList (pt ^. pttDeployCredential))
    let grouped0 = MinPQ.fromList [(wmdArrivalTime c, TGCredentialDeployment (c, Just verRes)) | Just (c, verRes) <- credentials]
    let groupAcctTxs groups (acc, (l, _)) = getAccountNonFinalized acc l <&> \case
          accTxs@((_, firstNonceTxs) : _) ->
            let txsList = concatMap (Map.toList . snd) accTxs
                minTime = minimum $ wmdArrivalTime <$> Map.keys firstNonceTxs
            in MinPQ.insert minTime (TGAccountTransactions $ map (_2 %~ Just) txsList) groups
          -- This should not happen since the pending transaction table should
          -- only have entries where there are actually transactions.
          [] -> groups
    grouped1 <- foldM groupAcctTxs grouped0 (HM.toList (pt ^. pttWithSender))
    let groupUpdates groups (uty, (l, _)) = getNonFinalizedChainUpdates uty l <&> \case
          uds@((_, firstSNUs) : _) ->
            let udsList = concatMap (Map.toList . snd) uds
                minTime = minimum $ wmdArrivalTime <$> Map.keys firstSNUs
            in MinPQ.insert minTime (TGUpdateInstructions $ map (_2 %~ Just) udsList) groups
          [] -> groups
    transactionGroups <- MinPQ.elems <$> foldM groupUpdates grouped1 (Map.toList (pt ^. pttUpdates))

    -- lookup the maximum block size as mandated by the runtime parameters
    maxSize <- rpBlockSize <$> getRuntimeParameters
    timeoutDuration <- rpBlockTimeout <$> getRuntimeParameters
    let timeout = addUTCTime (durationToNominalDiffTime timeoutDuration) startTime
    let maxBlockEnergy = gdMaxBlockEnergy genData
    let context = ContextState{
          _chainMetadata = cm,
          _maxBlockEnergy = maxBlockEnergy,
          _accountCreationLimit = accountCreationLim
          }
    (ft@Sch.FilteredTransactions{..}, finState) <-
        runBSM (Sch.filterTransactions (fromIntegral maxSize) timeout transactionGroups) context (mkInitialSS bshandle1 :: LogSchedulerState m)

    -- FIXME: At some point we should log things here using the same logging infrastructure as in consensus.

    let usedEnergy = finState ^. schedulerEnergyUsed
    let bshandle2 = finState ^. schedulerBlockState

    bshandle3 <- bsoSetTransactionOutcomes bshandle2 (map snd ftAdded)
    let counts = countFreeTransactions (map (fst . fst) ftAdded) (isJust mfinInfo)
    bshandle4 <- mintAndReward bshandle3 blockParent slotNumber blockBaker (epoch newSeedState) isNewEpoch mfinInfo (finState ^. schedulerExecutionCosts) counts updates'

    bshandleFinal <- freezeBlockState bshandle4
    endTime <- currentTime
    logEvent Scheduler LLInfo $ "Constructed a block in " ++ show (diffUTCTime endTime startTime)
    return (ft, ExecutionResult{_energyUsed = usedEnergy,
                                _finalState = bshandleFinal,
                                _transactionLog = finState ^. schedulerTransactionLog})
