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
import qualified Data.Set as Set
import qualified Data.Kind as DK
import qualified Data.PQueue.Prio.Min as MinPQ
import qualified Data.Vector as Vec
import Data.Foldable
import Data.Maybe
import Data.Word
import Control.Monad
import Concordium.TimeMonad
import Data.Time
import Data.Ratio

import Concordium.Types
import qualified Concordium.Types.Accounts as Types
import qualified Concordium.Types.Parameters as Types
import Concordium.Logger
import Concordium.GlobalState.Basic.BlockState.PoolRewards
import Concordium.GlobalState.TreeState
import qualified Concordium.GlobalState.BakerInfo as BI
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Parameters
    ( MintDistribution,
      PoolParameters(PoolParametersV1, _ppLeverageBound, _ppCapitalBound,
                     _ppMinimumFinalizationCapital, _ppMinimumEquityCapital,
                     _ppCommissionBounds, _ppLPoolCommissions),
      TimeParameters(_tpMintPerPayday),
      bakingCommissionRange,
      cpAccountCreationLimit,
      cpPoolParameters,
      cpTimeParameters,
      finalizationCommissionRange,
      mpsMintPerSlot,
      ppLPoolCommissions,
      tpMintPerPayday,
      transactionCommissionRange,
      CommissionRanges,
      HasGASRewards(gasFinalizationProof, gasBaker, gasAccountCreation,
                    gasChainUpdate),
      HasMintDistribution(mdFinalizationReward, mdMintPerSlot,
                          mdBakingReward),
      HasRewardParameters(rewardParameters, rpMintDistribution),
      HasTransactionFeeDistribution(tfdGASAccount, tfdBaker),
      BasicGenesisData(gdSlotDuration, gdMaxBlockEnergy, gdGenesisTime),
      UpdateValue(UVPoolParameters, UVTimeParameters,
                  UVMintDistribution),
      RuntimeParameters(rpBlockSize, rpBlockTimeout) )
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
import Concordium.Afgjort.Finalize.Types

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
    let rewardBaker (m,bs) (bid, blockCount) = do
          let brew = fromIntegral (blockCount :: Word64) * perBlock
          (macct,bs')  <- bsoRewardBaker bs bid brew
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
  let selectMaxSlotMintRate :: (Slot, MintRate) -> (Slot,  UpdateValue 'ChainParametersV1) -> (Slot, MintRate)
      selectMaxSlotMintRate (s2, mr2) (s1, UVTimeParameters tp) =
        if s1 <= s2 then (s1, _tpMintPerPayday tp) else (s2, mr2)
      selectMaxSlotMintRate a _ = a
      selectMaxSlotMintDistribution (s2, md2) (s1, UVMintDistribution md1) =
        if s1 <= s2 then (s1, md1) else (s2, md2)
      selectMaxSlotMintDistribution a _ = a
      newMintRate = snd $ foldl' selectMaxSlotMintRate (ps, mr) updates
      newMintDistribution = snd $ foldl' selectMaxSlotMintDistribution (ps, md) updates
  in doCalculatePaydayMintAmounts newMintDistribution newMintRate amt

-- |Mint for all slots since the last block, recording a
-- special transaction outcome for the minting.
doMinting :: (ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0, GlobalStateTypes m, BlockStateOperations m, BlockPointerMonad m)
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
doMintingPayday :: (ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1, GlobalStateTypes m, BlockStateOperations m, BlockPointerMonad m)
  => BlockPointerType m
  -- ^Parent block
  -> Epoch
  -- ^Payday epoch to mint for
  -> AccountAddress
  -- ^Current foundation account
  -> [(Slot, UpdateValue 'ChainParametersV1)]
  -- ^Ordered updates to the minting parameters
  -> UpdatableBlockState m
  -- ^Block state
  -> m (UpdatableBlockState m)
doMintingPayday blockParent paydayEpoch foundationAddr mintUpds bs0 = do
  parentUpdates <- getUpdates =<< blockState blockParent
  totGTU <- (^. totalGTU) <$> bsoGetBankStatus bs0
  seedstate <- bsoGetSeedState bs0
  let mint = calculatePaydayMintAmounts
              (parentUpdates ^. currentParameters . rpMintDistribution)
              (parentUpdates ^. currentParameters . cpTimeParameters . tpMintPerPayday)
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

-- |List of the parties in the finalization committee,
-- with their relative voting power.  Finalization rewards
-- are distributed in proportion to their power.
type FinalizerInfo = Vec.Vector (BakerId, VoterPower)

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
        let awardFinalizer (t, m, bs) (bkr, power) = do
              let amt = fromInteger $ toInteger finRew * toInteger power `div` toInteger totalPower
              (mbaddr, bs') <- bsoRewardBaker bs bkr amt
              case mbaddr of
                Nothing -> error $ "doFinalizationRewards: Finalizer BakerId (" ++ show bkr ++ ") is not valid."
                Just baddr -> return (t + amt, Map.insert baddr amt m, bs') 
        (totalAward, awardMap, bs1) <- foldM awardFinalizer (0, Map.empty, bs0) finInfo
        let remainder = finRew - totalAward
        bs2 <- bsoSetRewardAccounts bs1 (rewards & finalizationRewardAccount .~ remainder)
        bsoAddSpecialTransactionOutcome bs2 FinalizationRewards{
          stoFinalizationRewards = AccountAmounts awardMap,
          stoRemainder = remainder
        }
  where
    totalPower = sum (snd <$> finInfo)

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
doBlockReward transFees FreeTransactionCounts{..} bid foundationAddr bs0 = do
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
    (mbkr, bs3) <- bsoRewardBaker bs2 bid bakerOut
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

doBlockRewardP4 :: forall m. (BlockStateOperations m, MonadProtocolVersion m, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1, AccountVersionFor (MPV m) ~ 'AccountV1)
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
doBlockRewardP4 transFees FreeTransactionCounts{..} bid foundationAddr bs0 = do
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
    bs2 <- bsoAccrueFoundationAccount bs1 platformFees
    bs3 <- bsoUpdateAmountLPool bs2 (+ lPoolOut)
    bsoUpdateAmountBaker bs1 bid (+ poolOut)

distributeRewards
  :: forall m
   . (AccountVersionFor (MPV m) ~ 'AccountV1,
      ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1,
      BlockStateOperations m,
      TreeStateMonad m)
  => UpdatableBlockState m
  -> m (UpdatableBlockState m)
distributeRewards bs0 = do
  chainParameters <- bsoGetChainParameters bs0
  bankStatus <- bsoGetBankStatus bs0
  let rewardAccts = bankStatus ^. bankRewardAccounts
  lPoolAccrued <- bsoGetAmountLPool bs0
  capitalDistribution <- bsoGetCurrentCapitalDistribution bs0
  bakers <- bsoGetCurrentEpochBakers bs0
  let lPoolStake = sum $ dcDelegatorCapital <$> lPoolCapital capitalDistribution
      lPoolRelativeStake = lPoolStake % (BI.bakerTotalStake bakers + lPoolStake)
      lPoolFinalizationCommission = chainParameters ^. cpPoolParameters . ppLPoolCommissions . finalizationCommission
      lPoolFinalizationFraction = fractionToRational (complementAmountFraction lPoolFinalizationCommission) * toRational lPoolRelativeStake
      lPoolBakingCommission = chainParameters ^. cpPoolParameters . ppLPoolCommissions . bakingCommission
      lPoolBakingFraction = fractionToRational (complementAmountFraction lPoolBakingCommission) * toRational lPoolRelativeStake
      lPoolFinalizationReward = floor $ fromIntegral (rewardAccts ^. finalizationRewardAccount) * lPoolFinalizationFraction
      lPoolBakingReward = floor $ fromIntegral (rewardAccts ^. bakingRewardAccount) * lPoolBakingFraction
      lPoolTotalReward = lPoolFinalizationReward + lPoolBakingReward + lPoolAccrued
      lPoolTotalStake = Vec.foldl (\a dc -> a + dcDelegatorCapital dc) 0 (lPoolCapital capitalDistribution)
  (lPoolAccumReward, bs1) <- rewardDelegators bs0 lPoolTotalReward lPoolTotalStake (lPoolCapital capitalDistribution)
  let lPoolRemainder = lPoolTotalReward - lPoolAccumReward
  bs2 <- bsoUpdateAmountLPool bs1 (const lPoolRemainder)
  let bakerBakingRewardFactor = rewardAccts ^. bakingRewardAccount - lPoolBakingReward
  let bakerFinalizationFactor = rewardAccts ^. finalizationRewardAccount - lPoolFinalizationReward
  bs3 <- rewardBakers bs2 (bankStatus ^. totalGTU) bakers bakerBakingRewardFactor bakerFinalizationFactor (BI.bakerTotalStake bakers) (bakerPoolCapital capitalDistribution)
  -- TODO transaction outcome?
  undefined
    where
      rewardDelegators
        :: UpdatableBlockState m
        -> Amount
        -> Amount
        -> Vec.Vector DelegatorCapital
        -> m (Amount, UpdatableBlockState m)
      rewardDelegators bs totalReward totalStake dcs =
        foldM
          (\(accumReward, bs1) dc -> do
            let delegatorCapital = dcDelegatorCapital dc
                relativeStake = delegatorCapital % totalStake
                reward = floor $ relativeStake * fromIntegral totalReward
            -- It is an invariant that the delegator is a delegation account,
            -- so we ignore the "error part" of 'bsoRewardDelegator'.
            (_, bs2) <- bsoRewardDelegator bs1 (dcDelegatorId dc) reward
            -- TODO transaction outcome here?
            return (accumReward + reward, bs2)
          ) (0, bs) dcs

      rewardBakers
        :: UpdatableBlockState m
        -> Amount
        -> BI.FullBakers
        -> Amount
        -> Amount
        -> Amount
        -> Vec.Vector BakerCapital
        -> m (UpdatableBlockState m)
      rewardBakers bs gtuTotal bakers bakerBakingRewardFactor bakerFinalizationFactor totalStake bcs = do
        let bakerStakesMap =
              foldl
                (\m bi -> Map.insert (bi ^. BI.theBakerInfo . Types.bakerIdentity) (bi ^. BI.bakerStake) m)
                Map.empty
                (BI.fullBakerInfos bakers)
        paydayBlockCount <- bsoGetTotalRewardPeriodBlockCount bs
        finParams <- gdFinalizationParameters <$> getGenesisData
        let committee = makeFinalizationCommittee finParams gtuTotal bakers
        fst <$> foldM
          (\(bsIn, idx) bc -> do
            bprd <- bsoGetBakerPoolRewardDetails bsIn idx
            let accruedReward = transactionFeesAccrued bprd
                bakerBlockCount = blockCount bprd
                bakerBlockFraction = bakerBlockCount % paydayBlockCount
                bakerBakingReward = floor $ fromIntegral bakerBakingRewardFactor * bakerBlockFraction
                finalized = finalizationAwake bprd
                relativeStake = case Map.lookup (bcBakerId bc) bakerStakesMap of
                  Nothing ->
                    error "Invariant violation: baker from capital distribution is not an epoch baker"
                  Just s ->
                    if finalized
                    then s % fromIntegral (totalWeight committee)
                    else 0
                bakerFinalizationReward = floor $ fromIntegral bakerFinalizationFactor * relativeStake
                totalCapital = foldl (\a dc -> a + dcDelegatorCapital dc) (bcBakerEquityCapital bc) (bcDelegatorCapital bc)
                -- TODO: implement this (see paper):
                totalDelegatorReward = undefined
            (delegationReward, bsDel) <- rewardDelegators bsIn totalDelegatorReward totalCapital (bcDelegatorCapital bc)
            let bakerReward = accruedReward + bakerBakingReward + bakerFinalizationReward - delegationReward
            -- It is an invariant that the baker is a baker account,
            -- so we ignore the "error part" of 'bsoRewardBaker'.
            (_, bsBak) <- bsoRewardBaker bsDel (bcBakerId bc) bakerReward
            -- TODO transaction outcome here?
            return (bsBak, idx + 1)
          ) (bs, 0) bcs
  -- TODO: remove this:
  -- bi <- derefBakerInfo $ activeBakerInfoRef abi
  -- cO,P(i)=capitalO,P(i) /capitalP(i)
  -- We need R_{F,P}^i, R_{B,P}^i, R_{T,P}^i and µ_{F,P}, µ_{B,P}, µ_{T,P}
  -- R_{T,P}^i is the transactionFeesAccrued from BakerPoolRewardDetails
  -- R_{T,L}^i is the lPoolTransactionRewards in PoolRewards
  -- R_{F,L}^i = R_F^i*(1-µ_{F,L}) * s_L^i where 
  -- - s_L^i = stake_L^i / (stake^i + stake_L^i)
  -- - stake_L^i = 
  -- - stake^i = sum_P stake_P^i = _bakerTotalStake
  -- - µ_{F,L} = chain parameter
  -- R_{F,P}^i is (R_F^i - R_{F,L}^i) * fs_P^i, where
  -- - R_F^i is _finalizationRewardAccount from RewardAccounts
  -- - fs_P^i = fstake_P^i / fstake^i where fstake_P^i = stake_P^i = min(capital_P^i, beta_{P,max}*capital^i, lambda_P*capital_{O,P}^i) = _bakerStake = partyWeight in PartyInfo
  -- - fstake^i = \sum_P fstake_P^i = totalWeight from Finalization commitee
  -- getCurrentEpochBakers -> FullBakers -> FullBakerInfo -> _bakerStake

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
    -- ^New epoch
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
      -- First, check if next payday is in the future
      foundationAccount <- getAccountCanonicalAddress =<< bsoGetFoundationAccount bshandle
      nextPayday <- bsoGetPaydayEpoch bshandle
      bshandleFinRew <- if newEpoch < nextPayday
        then return bshandle
        else do
          -- 1. mint for first payday
          bshandleMint <- doMintingPayday blockParent nextPayday foundationAccount updates bshandle
          -- 2. distribute rewards
          bshandleRewards <- distributeRewards bshandleMint
          -- TODO: consider the case where blockParent is a genesis block
          -- 3. distributed accrued transaction fees
          -- 4. Mint for skipped paydays
          return bshandle

      -- accumulate fee
      doBlockRewardP4 transFees freeCounts bid foundationAccount bshandleFinRew

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
  -> m (Bool, UpdatableBlockState m)
updateBirkParameters newSeedState bs0 = case protocolVersion @(MPV m) of
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
            then do
              bsoTransitionEpochBakers bs0 (epoch newSeedState - 1)
            else return bs0
          bsoTransitionEpochBakers upToLast (epoch newSeedState)
        else
          return bs0
      (isNewEpoch,) <$> bsoSetSeedState bs1 newSeedState
    updateCPV1AccountV1 :: (AccountVersionFor (MPV m) ~ 'AccountV1, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1) => m (Bool, UpdatableBlockState m)
    updateCPV1AccountV1 = do
      oldSeedState <- bsoGetSeedState bs0
      let isNewEpoch = epoch oldSeedState /= epoch newSeedState
      nextPayday <- bsoGetPaydayEpoch bs0
      bs1 <- if isNewEpoch && epoch oldSeedState + 1 < nextPayday && epoch newSeedState + 1 > nextPayday
        then generateNextBakers bs0
        else
          return bs0
      (isNewEpoch,) <$> bsoSetSeedState bs1 newSeedState

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
  -> [BlockItem] -- ^Transactions on this block.
  -> m (Either (Maybe FailureKind) (ExecutionResult m))
executeFrom blockHash slotNumber slotTime blockParent blockBaker mfinInfo newSeedState txs =
  let cm = ChainMetadata{..}
  in do
    origBS <- blockState blockParent
    bshandle0 <- thawBlockState origBS
    chainParams <- bsoGetChainParameters bshandle0
    let counts = countFreeTransactions txs (isJust mfinInfo)
    if (countAccountCreation counts > chainParams ^. cpAccountCreationLimit)
      then
          return $ Left (Just ExceedsMaxCredentialDeployments)
      else do
        -- process the update queues
        (updates, bshandle0a) <- bsoProcessUpdateQueues bshandle0 slotTime
        sd <- gdSlotDuration <$> getGenesisData
        ab <- getActiveBakers origBS
        let isPoolParameterUpdate = \case
              UVPoolParameters _ -> True
              _ -> False
        -- take out pool parameter updates, ordered by timestamp
        let poolParameterUpdates = filter isPoolParameterUpdate $ Map.elems updates
        -- for each pool parameter update, go over all bakers and put their commissions inside
        -- the new commission ranges.
        bshandle0a' <- foldM (\bs uv -> case uv of
          UVPoolParameters PoolParametersV1{..} -> foldM (putBakerCommissionsInRange _ppCommissionBounds sd origBS) bs ab
          _ -> return bs) bshandle0a poolParameterUpdates
        -- unlock the amounts that have expired
        bshandle0b <- bsoProcessReleaseSchedule bshandle0a' slotTime
        -- update the bakers and seed state
        (isNewEpoch, bshandle1) <- updateBirkParameters newSeedState bshandle0b
        maxBlockEnergy <- gdMaxBlockEnergy <$> getGenesisData
        let context = ContextState{
              _chainMetadata = cm,
              _maxBlockEnergy = maxBlockEnergy,
              _accountCreationLimit = chainParams ^. cpAccountCreationLimit,
              _slotDuration = sd
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
                zipWithM_ (commitTransaction slotNumber blockHash) txs [0..]
                -- the main execution is now done. At this point we must mint new currency
                -- and reward the baker and other parties.
                genData <- getGenesisData
                let updates' = (_1 %~ transactionTimeToSlot (gdGenesisTime genData) (gdSlotDuration genData))
                                <$> Map.toAscList updates
                bshandle4 <- mintAndReward bshandle3 blockParent slotNumber blockBaker (epoch newSeedState) isNewEpoch mfinInfo (finState ^. schedulerExecutionCosts) counts updates'

                finalbsHandle <- freezeBlockState bshandle4
                return (Right (ExecutionResult{_energyUsed = usedEnergy,
                                              _finalState = finalbsHandle,
                                              _transactionLog = finState ^. schedulerTransactionLog}))
  where
    putBakerCommissionsInRange :: ChainParametersVersionFor (MPV m) ~ 'ChainParametersV1 => CommissionRanges -> Duration -> BlockState m -> UpdatableBlockState m -> BakerId -> m (UpdatableBlockState m)
    putBakerCommissionsInRange ranges sd origBS bs bid@(BakerId ai) = getBakerAccount origBS bid >>= \case
      Nothing -> error "Invariant violation: Active baker is not a baker."
      Just acc -> getAccountBaker acc >>= \case
        Nothing -> error "Invariant violation: Active baker is not a baker."
        Just ab -> case ab ^. Types.accountBakerInfo of
          Types.BakerInfoExV0 _ -> return bs
          Types.BakerInfoExV1 _ bpi -> do
            let fc = bpi ^. Types.poolCommissionRates . finalizationCommission
            let cfc = Types.closestInRange fc (ranges ^. finalizationCommissionRange)
            let bc = bpi ^. Types.poolCommissionRates . bakingCommission
            let cbc = Types.closestInRange fc (ranges ^. bakingCommissionRange)
            let tc = bpi ^. Types.poolCommissionRates . transactionCommission
            let ctc = Types.closestInRange fc (ranges ^. transactionCommissionRange)
            (result, newBS) <- bsoConfigureBaker bs ai BI.BakerConfigureUpdate{
              bcuSlotTimestamp = slotTime,
              bcuSlotDuration = sd,
              bcuKeys = Nothing,
              bcuCapital = Nothing,
              bcuRestakeEarnings = Nothing,
              bcuOpenForDelegation = Nothing,
              bcuMetadataURL = Nothing,
              bcuTransactionFeeCommission = if fc == cfc then Nothing else Just cfc,
              bcuBakingRewardCommission = if bc == cbc then Nothing else Just cbc,
              bcuFinalizationRewardCommission = if tc == ctc then Nothing else Just ctc}
            case result of
              BI.BCSuccess _ _ -> return newBS
              _ -> error "Failed to configure baker with new rates within ranges." -- this should never happen

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
    chainParams <- bsoGetChainParameters bshandle0
    -- process the update queues
    (updates, bshandle0a) <- bsoProcessUpdateQueues bshandle0 slotTime
    -- unlock the amounts that have expired
    bshandle0b <- bsoProcessReleaseSchedule bshandle0a slotTime
    -- update the bakers and seed state
    (isNewEpoch, bshandle1) <- updateBirkParameters newSeedState bshandle0b
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
    let grouped0 = MinPQ.fromList [(wmdArrivalTime c, TGCredentialDeployment c) | Just c <- credentials]
    let groupAcctTxs groups (acc, (l, _)) = getAccountNonFinalized acc l <&> \case
          accTxs@((_, firstNonceTxs) : _) ->
            let txsList = concatMap (Set.toList . snd) accTxs
                minTime = minimum $ wmdArrivalTime <$> Set.toList firstNonceTxs
            in MinPQ.insert minTime (TGAccountTransactions txsList) groups
          -- This should not happen since the pending transaction table should
          -- only have entries where there are actually transactions.
          [] -> groups
    grouped1 <- foldM groupAcctTxs grouped0 (HM.toList (pt ^. pttWithSender))
    let groupUpdates groups (uty, (l, _)) = getNonFinalizedChainUpdates uty l <&> \case
          uds@((_, firstSNUs) : _) ->
            let udsList = concatMap (Set.toList . snd) uds
                minTime = minimum $ wmdArrivalTime <$> Set.toList firstSNUs
            in MinPQ.insert minTime (TGUpdateInstructions udsList) groups
          [] -> groups
    transactionGroups <- MinPQ.elems <$> foldM groupUpdates grouped1 (Map.toList (pt ^. pttUpdates))

    -- lookup the maximum block size as mandated by the runtime parameters
    maxSize <- rpBlockSize <$> getRuntimeParameters
    timeoutDuration <- rpBlockTimeout <$> getRuntimeParameters
    let timeout = addUTCTime (durationToNominalDiffTime timeoutDuration) startTime
    genData <- getGenesisData
    let maxBlockEnergy = gdMaxBlockEnergy genData
    let context = ContextState{
          _chainMetadata = cm,
          _maxBlockEnergy = maxBlockEnergy,
          _accountCreationLimit = chainParams ^. cpAccountCreationLimit,
          _slotDuration = gdSlotDuration genData
          }
    (ft@Sch.FilteredTransactions{..}, finState) <-
        runBSM (Sch.filterTransactions (fromIntegral maxSize) timeout transactionGroups) context (mkInitialSS bshandle1 :: LogSchedulerState m)
    -- FIXME: At some point we should log things here using the same logging infrastructure as in consensus.

    let usedEnergy = finState ^. schedulerEnergyUsed
    let bshandle2 = finState ^. schedulerBlockState

    bshandle3 <- bsoSetTransactionOutcomes bshandle2 (map snd ftAdded)
    let counts = countFreeTransactions (map fst ftAdded) (isJust mfinInfo)
    let updates' = (_1 %~ transactionTimeToSlot (gdGenesisTime genData) (gdSlotDuration genData))
                    <$> Map.toAscList updates
    bshandle4 <- mintAndReward bshandle3 blockParent slotNumber blockBaker (epoch newSeedState) isNewEpoch mfinInfo (finState ^. schedulerExecutionCosts) counts updates'

    bshandleFinal <- freezeBlockState bshandle4
    endTime <- currentTime
    logEvent Scheduler LLInfo $ "Constructed a block in " ++ show (diffUTCTime endTime startTime)
    return (ft, ExecutionResult{_energyUsed = usedEnergy,
                                _finalState = bshandleFinal,
                                _transactionLog = finState ^. schedulerTransactionLog})
