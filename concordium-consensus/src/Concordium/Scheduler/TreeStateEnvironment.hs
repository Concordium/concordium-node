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

import Concordium.Types
import Concordium.Logger
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Parameters
import Concordium.Types.SeedState
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.Types.UpdateQueues
    (HasUpdates(currentParameters))
import Concordium.Scheduler.Types
import Concordium.Scheduler.Environment
import Concordium.Scheduler.EnvironmentImplementation
    (BSOMonadWrapper(..),
     ContextState(..),
     HasSchedulerState(..),
     schedulerBlockState, schedulerEnergyUsed
     )

import Control.Monad.RWS.Strict

import Lens.Micro.Platform

import qualified Concordium.Scheduler as Sch

newtype BlockStateMonad (pv :: ProtocolVersion) w state m a = BSM { _runBSM :: RWST ContextState w state m a}
    deriving (Functor, Applicative, Monad, MonadState state, MonadReader ContextState, MonadTrans, MonadWriter w, MonadLogger, TimeMonad)

deriving via (BSOMonadWrapper pv ContextState w state (MGSTrans (RWST ContextState w state) m))
    instance
        (SS state ~ UpdatableBlockState m, Monoid w, HasSchedulerState state,
        BlockStateOperations m, Footprint (ATIStorage m) ~ w)
             => StaticInformation (BlockStateMonad pv w state m)

instance (ATIStorage m ~ w, ATITypes m) => ATITypes (BlockStateMonad pv w state m) where
  type ATIStorage (BlockStateMonad pv w state m) = ATIStorage m

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

instance TreeStateMonad pv m => HasSchedulerState (LogSchedulerState m) where
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
    instance BlockStateTypes (BlockStateMonad pv w state m)

deriving via (MGSTrans (RWST ContextState w state) m)
    instance (Monoid w, AccountOperations m) => AccountOperations (BlockStateMonad pv w state m)

deriving via (BSOMonadWrapper pv ContextState w state (MGSTrans (RWST ContextState w state) m))
    instance (
              SS state ~ UpdatableBlockState m,
              Footprint (ATIStorage m) ~ w,
              HasSchedulerState state,
              TreeStateMonad pv m,
              MonadLogger m,
              BlockStateOperations m) => SchedulerMonad pv (BlockStateMonad pv w state m)

runBSM :: Monad m => BlockStateMonad pv w b m a -> ContextState -> b -> m (a, b)
runBSM m cm s = do
  (r, s', _) <- runRWST (_runBSM m) cm s
  return (r, s')

-- |Distribute the baking rewards for the last epoch to the bakers of
-- blocks in that epoch. This should be called in the first block of
-- a new epoch. This resets the list of blocks baked in the epoch.
rewardLastEpochBakers :: (BlockStateOperations m)
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
  -> MintDistribution
  -- ^Initial mint distribution
  -> [(Slot, MintDistribution)]
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
                  | s' <= e = let !a = mintAmount (md ^. mdMintPerSlot) t' in mintSupply (s'+1) (m+a) (t'+a)
                  | otherwise = (m, t')
              (newMint, newTotal) = mintSupply s 0 t
              mintBakingReward = takeFraction (md ^. mdBakingReward) newMint
              mintFinalizationReward = takeFraction (md ^. mdFinalizationReward) newMint
              mintDevelopmentCharge = newMint - (mintBakingReward + mintFinalizationReward)
          in (newTotal, MintAmounts{..})

-- |Mint for all slots since the last block, recording a
-- special transaction outcome for the minting.
doMinting :: (GlobalStateTypes m, BlockStateOperations m, BlockPointerMonad m)
  => BlockPointerType m
  -- ^Parent block
  -> Slot
  -- ^New slot
  -> AccountAddress
  -- ^Current foundation account
  -> [(Slot, MintDistribution)]
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
doBlockReward :: (BlockStateOperations m)
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
    rewardParams <- (^. rewardParameters) <$> bsoGetChainParameters bs0
    oldRewardAccts <- (^. rewardAccounts) <$> bsoGetBankStatus bs0
    let gasIn = oldRewardAccts ^. gasAccount
        bakerFees = takeFraction (rewardParams ^. tfdBaker) transFees
        gasFees = takeFraction (rewardParams ^. tfdGASAccount) transFees
        platformFees = transFees - (bakerFees + gasFees)
        -- Compute the GAS carried over. This is done at full precision and then
        -- rounded up (so that the payment to the baker is rounded up).
        gasGAS = ceiling $ toRational gasIn
                    * (fractionToRational . complementRewardFraction $ rewardParams ^. gasBaker)
                    * (fractionToRational . complementRewardFraction $ rewardParams ^. gasAccountCreation)^countAccountCreation
                    * (fractionToRational . complementRewardFraction $ rewardParams ^. gasChainUpdate)^countUpdate
                    * (fractionToRational . complementRewardFraction $ rewardParams ^. gasFinalizationProof)^countFinRecs
        bakerGAS = gasIn - gasGAS
        gasOut = gasFees + gasGAS
        bakerOut = bakerFees + bakerGAS
    bs1 <- bsoSetRewardAccounts bs0 (oldRewardAccts & gasAccount .~ gasOut)
    (mbkr, bs2) <- bsoRewardBaker bs1 bid bakerOut
    bkr <- case mbkr of
      Nothing -> error "Invalid baker account"
      Just bkr -> return bkr
    bs3 <- bsoRewardFoundationAccount bs2 platformFees
    bsoAddSpecialTransactionOutcome bs3 BlockReward{
      stoTransactionFees = transFees,
      stoOldGASAccount = gasIn,
      stoNewGASAccount = gasOut,
      stoBakerReward = bakerOut,
      stoFoundationCharge = platformFees,
      stoBaker = bkr,
      stoFoundationAccount = foundationAddr
    }


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
mintAndReward :: (BlockStateOperations m, BlockPointerMonad m)
    => UpdatableBlockState m
    -- ^Block state
    -> BlockPointerType m
    -- ^Parent block
    -> Slot
    -- ^Block slot
    -> BakerId
    -- ^Baker ID
    -> Bool
    -- ^Is a new epoch
    -> Maybe FinalizerInfo
    -- ^Info on finalization committee for included record, if any
    -> Amount
    -- ^Transaction fees
    -> FreeTransactionCounts
    -- ^Number of "free" transactions of each type
    -> [(Slot, UpdateValue)]
    -- ^Ordered chain updates since the last block
    -> m (UpdatableBlockState m)
mintAndReward bshandle blockParent slotNumber bid isNewEpoch mfinInfo transFees freeCounts updates = do
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
updateBirkParameters :: BlockStateOperations m
  => SeedState
  -- ^New seed state
  -> UpdatableBlockState m
  -- ^Block state
  -> m (Bool, UpdatableBlockState m)
updateBirkParameters newSeedState bs0 = do
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
executeFrom :: forall m pv.
  (BlockPointerMonad m, TreeStateMonad pv m, MonadLogger m)
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
    bshandle0 <- thawBlockState =<< blockState blockParent
    chainParams <- bsoGetChainParameters bshandle0
    let counts = countFreeTransactions txs (isJust mfinInfo)
    if (countAccountCreation counts > chainParams ^. cpAccountCreationLimit)
      then
          return $ Left (Just ExceedsMaxCredentialDeployments)
      else do
        -- process the update queues
        (updates, bshandle0a) <- bsoProcessUpdateQueues bshandle0 slotTime
        -- unlock the amounts that have expired
        bshandle0b <- bsoProcessReleaseSchedule bshandle0a slotTime
        -- update the bakers and seed state
        (isNewEpoch, bshandle1) <- updateBirkParameters newSeedState bshandle0b
        maxBlockEnergy <- gdMaxBlockEnergy <$> getGenesisData
        let context = ContextState{
              _chainMetadata = cm,
              _maxBlockEnergy = maxBlockEnergy,
              _accountCreationLimit = chainParams ^. cpAccountCreationLimit
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
                bshandle4 <- mintAndReward bshandle3 blockParent slotNumber blockBaker isNewEpoch mfinInfo (finState ^. schedulerExecutionCosts) counts updates'

                finalbsHandle <- freezeBlockState bshandle4
                return (Right (ExecutionResult{_energyUsed = usedEnergy,
                                              _finalState = finalbsHandle,
                                              _transactionLog = finState ^. schedulerTransactionLog}))

-- |PRECONDITION: Focus block is the parent block of the block we wish to make,
-- hence the pending transaction table is correct for the new block.
-- EFFECTS: This function only updates the block state. It has no effects on the transaction table.
-- POSTCONDITION: The function always returns a list of transactions which make a valid block in `ftAdded`,
-- and also returns a list of transactions which failed, and a list of those which were not processed.
constructBlock :: forall m pv.
  (BlockPointerMonad m, TreeStateMonad pv m, MonadLogger m, TimeMonad m)
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
          _accountCreationLimit = chainParams ^. cpAccountCreationLimit
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
    bshandle4 <- mintAndReward bshandle3 blockParent slotNumber blockBaker isNewEpoch mfinInfo (finState ^. schedulerExecutionCosts) counts updates'

    bshandleFinal <- freezeBlockState bshandle4
    endTime <- currentTime
    logEvent Scheduler LLInfo $ "Constructed a block in " ++ show (diffUTCTime endTime startTime)
    return (ft, ExecutionResult{_energyUsed = usedEnergy,
                                _finalState = bshandleFinal,
                                _transactionLog = finState ^. schedulerTransactionLog})
