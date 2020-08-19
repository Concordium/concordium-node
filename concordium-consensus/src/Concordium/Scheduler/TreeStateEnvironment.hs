{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeApplications #-}
{-# LANGUAGE DerivingVia, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.TreeStateEnvironment where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe
import qualified Data.Kind as DK
import Control.Monad

import Concordium.Types
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.AccountTransactionIndex
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

newtype BlockStateMonad w state m a = BSM { _runBSM :: RWST ContextState w state m a}
    deriving (Functor, Applicative, Monad, MonadState state, MonadReader ContextState, MonadTrans, MonadWriter w)

deriving via (BSOMonadWrapper ContextState w state (MGSTrans (RWST ContextState w state) m))
    instance (SS state ~ UpdatableBlockState m, Monoid w, HasSchedulerState state, BlockStateOperations m, Footprint (ATIStorage m) ~ w)
             => StaticInformation (BlockStateMonad w state m)

instance (ATIStorage m ~ w, ATITypes m) => ATITypes (BlockStateMonad w state m) where
  type ATIStorage (BlockStateMonad w state m) = ATIStorage m

data LogSchedulerState (m :: DK.Type -> DK.Type) = LogSchedulerState {
  _lssBlockState :: !(UpdatableBlockState m),
  _lssSchedulerEnergyUsed :: !Energy,
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
  type AccountTransactionLog (LogSchedulerState m) = ATIStorage m
  schedulerBlockState = lssBlockState
  schedulerEnergyUsed = lssSchedulerEnergyUsed
  nextIndex = lssNextIndex
  accountTransactionLog = lssSchedulerTransactionLog


mkInitialSS :: CanExtend (ATIStorage m) => UpdatableBlockState m -> LogSchedulerState m
mkInitialSS _lssBlockState =
  LogSchedulerState{_lssSchedulerEnergyUsed = 0,
                    _lssSchedulerTransactionLog = defaultValue,
                    _lssNextIndex = 0,
                    ..}

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
              BlockStateOperations m) => SchedulerMonad (BlockStateMonad w state m)

runBSM :: Monad m => BlockStateMonad w b m a -> ContextState -> b -> m (a, b)
runBSM m cm s = do
  (r, s', _) <- runRWST (_runBSM m) cm s
  return (r, s')

-- |Reward the baker, identity providers, ...
-- TODO: Currently the finalized pointer is not used. But the finalization committee
-- of that block might need to be rewarded if they have not been already.
-- Thus the argument is here for future use
mintAndReward :: (GlobalStateTypes m, BlockStateOperations m, BlockPointerMonad m)
                => UpdatableBlockState m -> BlockPointerType m -> BlockPointerType m -> Slot -> BakerId -> m (UpdatableBlockState m)
mintAndReward bshandle blockParent _lfPointer slotNumber bid = do

  -- First we mint new currency. This can be used in rewarding bakers. First get
  -- the inflation rate of the parent block (this might have changed in the
  -- current block), and compute how much to mint based on elapsed time.
  rewardStatus <- getRewardStatus =<< blockState blockParent
  let inflationRate = rewardStatus ^. mintedGTUPerSlot
  let mintedAmount = fromIntegral (slotNumber - blockSlot blockParent) * inflationRate
  (cbamount, bshandleMinted) <- bsoMint bshandle mintedAmount

  -- and now we can reward everybody
  -- first take half of the amount on the central bank and use it to reward the baker
  -- TODO: This is temporary POC. We need this fraction to be flexible.
  let bakingReward = cbamount `div` 2
  (_, bshandle1) <- bsoDecrementCentralBankGTU bshandleMinted bakingReward

  executionReward <- bsoGetExecutionCost bshandle1
  macc <- bsoGetEpochBakerAccount bshandle1 bid
  case macc of
    Nothing -> error "Precondition violated. Baker account does not exist."
    Just acc -> do
      addr <- getAccountAddress acc
      bshandle2 <- bsoModifyAccount bshandle1
         (emptyAccountUpdate addr & auAmount ?~ (amountToDelta (executionReward + bakingReward)))
      -- record the block reward transaction in the transaction outcomes for this block
      bsoAddSpecialTransactionOutcome bshandle2 (BakingReward bid addr (executionReward + bakingReward))


-- |Execute a block from a given starting state.
-- Fail if any of the transactions fails, otherwise return the new 'BlockState' and the amount of energy used
-- during this block execution.
executeFrom :: forall m .
  (GlobalStateTypes m, BlockPointerMonad m, TreeStateMonad m)
  => BlockHash -- ^Hash of the block we are executing. Used only for commiting transactions.
  -> Slot -- ^Slot number of the block being executed.
  -> Timestamp -- ^Unix timestamp of the beginning of the slot.
  -> BlockPointerType m  -- ^Parent pointer from which to start executing
  -> BlockPointerType m  -- ^Last finalized block pointer.
  -> BakerId -- ^Identity of the baker who should be rewarded.
  -> BirkParameters m
  -> [BlockItem] -- ^Transactions on this block.
  -> m (Either (Maybe FailureKind) (ExecutionResult m))
executeFrom blockHash slotNumber slotTime blockParent lfPointer blockBaker bps txs =
  let cm = let blockHeight = bpHeight blockParent + 1
               finalizedHeight = bpHeight lfPointer
           in ChainMetadata{..}
  in do
    bshandle0 <- thawBlockState =<< blockState blockParent
    -- update the block states parameters according to the slot of this block
    -- if the block is in a new epoch, the bakers are shifted and a new leadership election nonce is computed
    -- in most cases the block nonce is added to the seed state
    bshandle1 <- bsoUpdateBirkParameters bshandle0 bps
    genBetaAccounts <- HashSet.fromList . map (^. accountAddress) . genesisControlAccounts <$> getGenesisData
    maxBlockEnergy <- genesisMaxBlockEnergy <$> getGenesisData
    let context = ContextState{
          _specialBetaAccounts = genBetaAccounts,
          _chainMetadata = cm,
          _maxBlockEnergy = maxBlockEnergy
          }
    (res, finState) <- runBSM (Sch.runTransactions txs) context (mkInitialSS bshandle1 :: LogSchedulerState m)
    let usedEnergy = finState ^. schedulerEnergyUsed
    let bshandle2 = finState ^. schedulerBlockState
    case res of
        Left fk -> Left fk <$ (dropUpdatableBlockState bshandle2)
        Right outcomes -> do

            -- Record the transaction outcomes
            bshandle3 <- bsoSetTransactionOutcomes bshandle2 (map snd outcomes)
            -- Record transaction outcomes in the transaction table as well.
            zipWithM_ (commitTransaction slotNumber blockHash) txs [0..]
            -- the main execution is now done. At this point we must mint new currency
            -- and reward the baker and other parties.
            bshandle4 <- mintAndReward bshandle3 blockParent lfPointer slotNumber blockBaker

            finalbsHandle <- freezeBlockState bshandle4
            return (Right (ExecutionResult{_energyUsed = usedEnergy,
                                           _finalState = finalbsHandle,
                                           _transactionLog = finState ^. accountTransactionLog}))

-- |PRECONDITION: Focus block is the parent block of the block we wish to make,
-- hence the pending transaction table is correct for the new block.
-- EFFECTS: This function only updates the block state. It has no effects on the transaction table.
-- POSTCONDITION: The function always returns a list of transactions which make a valid block in `ftAdded`,
-- and also returns a list of transactions which failed, and a list of those which were not processed.
constructBlock :: forall m .
  (GlobalStateTypes m, BlockPointerMonad m, TreeStateMonad m)
  => Slot -- ^Slot number of the block to bake
  -> Timestamp -- ^Unix timestamp of the beginning of the slot.
  -> BlockPointerType m -- ^Parent pointer from which to start executing
  -> BlockPointerType m -- ^Last finalized block pointer.
  -> BakerId -- ^The baker of the block.
  -> BirkParameters m
  -> m (Sch.FilteredTransactions, ExecutionResult m)
constructBlock slotNumber slotTime blockParent lfPointer blockBaker bps =
  let cm = let blockHeight = bpHeight blockParent + 1
               finalizedHeight = bpHeight lfPointer
           in ChainMetadata{..}
  in do
    bshandle0 <- thawBlockState =<< blockState blockParent
    -- update the block states parameters according to the slot of this block
    -- if the block is in a new epoch, the bakers are shifted and a new leadership election nonce is computed
    -- in most cases the block nonce is added to the seed state
    bshandle1 <- bsoUpdateBirkParameters bshandle0 bps
    pt <- getPendingTransactions

    -- lookup the maximum block size as mandated by the tree state
    maxSize <- rpBlockSize <$> getRuntimeParameters

    let credentialHashes = HashSet.toList (pt ^. pttDeployCredential)
    -- NB: catMaybes is safe, but invariants should ensure that it is a no-op.
    orderedCredentials <- List.sortOn wmdArrivalTime . catMaybes <$> mapM getCredential credentialHashes

    -- now we get transactions for each of the pending accounts.
    txs <- forM (HM.toList (pt ^. pttWithSender)) $ \(acc, (l, _)) -> do
      accTxs <- getAccountNonFinalized acc l

      -- now find for each account the least arrival time of a transaction
      let txsList = concatMap (Set.toList . snd) accTxs
      let minTime = minimum $ map wmdArrivalTime txsList
      return (txsList, minTime)

    -- FIXME: This is inefficient and should be changed. Doing it only to get the integration working.
    -- Order the accounts by the arrival time of the earliest transaction.
    let orderedTxs = map fst $ List.sortOn snd txs
    genBetaAccounts <- HashSet.fromList . map (^. accountAddress) . genesisControlAccounts <$> getGenesisData
    maxBlockEnergy <- genesisMaxBlockEnergy <$> getGenesisData
    let context = ContextState{
          _specialBetaAccounts = genBetaAccounts,
          _chainMetadata = cm,
          _maxBlockEnergy = maxBlockEnergy
          }
    let grouped = GroupedTransactions{
          perAccountTransactions = orderedTxs,
          credentialDeployments = orderedCredentials
          }
    (ft@Sch.FilteredTransactions{..}, finState) <-
        runBSM (Sch.filterTransactions (fromIntegral maxSize) grouped) context (mkInitialSS bshandle1 :: LogSchedulerState m)
    -- FIXME: At some point we should log things here using the same logging infrastructure as in consensus.

    let usedEnergy = finState ^. schedulerEnergyUsed
    let bshandle2 = finState ^. schedulerBlockState

    bshandle3 <- bsoSetTransactionOutcomes bshandle2 (map snd ftAdded)
    bshandle4 <- mintAndReward bshandle3 blockParent lfPointer slotNumber blockBaker

    bshandleFinal <- freezeBlockState bshandle4
    return (ft, ExecutionResult{_energyUsed = usedEnergy,
                                _finalState = bshandleFinal,
                                _transactionLog = finState ^. accountTransactionLog})
