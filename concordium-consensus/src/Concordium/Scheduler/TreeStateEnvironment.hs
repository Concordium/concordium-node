{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.TreeStateEnvironment where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.List as List

import Control.Monad

import Concordium.Types
import Concordium.GlobalState.TreeState hiding (blockBaker)
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block(blockSlot)
import Concordium.GlobalState.TransactionLogs
import Concordium.GlobalState.Classes(MGSTrans)
import Concordium.Scheduler.Types
import Concordium.Scheduler.Environment
import Concordium.Scheduler.EnvironmentImplementation
    (BSOMonadWrapper(..),
     ContextState(..),
     HasSchedulerState(SS),
     SchedulerState, ssBlockState, ssSchedulerEnergyUsed,
     mkInitialSS)

import Control.Monad.RWS.Strict

import Lens.Micro.Platform

import qualified Acorn.Core as Core

import qualified Concordium.Scheduler as Sch

newtype BlockStateMonad state m a = BSM { _runBSM :: RWST ContextState () state m a}
    deriving (Functor, Applicative, Monad, MonadState state, MonadReader ContextState, MonadTrans)

deriving via MGSTrans (RWST ContextState () state) m instance TransactionLogger m => TransactionLogger (BlockStateMonad state m)

deriving via (BSOMonadWrapper ContextState state (MGSTrans (RWST ContextState () state) m))
    instance (SS state ~ UpdatableBlockState m, HasSchedulerState state, BlockStateOperations m)
             => StaticEnvironmentMonad Core.UA (BlockStateMonad state m)

deriving via (BSOMonadWrapper ContextState state (MGSTrans (RWST ContextState () state) m))
    instance (TransactionLogger m,
              SS state ~ UpdatableBlockState m,
              HasSchedulerState state,
              BlockStateOperations m) => SchedulerMonad (BlockStateMonad state m)

runBSM :: Monad m => BlockStateMonad b m a -> ContextState -> b -> m (a, b)
runBSM m cm s = do
  (r, s', ()) <- runRWST (_runBSM m) cm s
  return (r, s')

-- |Reward the baker, identity providers, ...
-- TODO: Currently the finalized pointer is not used. But the finalization committee
-- of that block might need to be rewarded if they have not been already.
-- Thus the argument is here for future use
mintAndReward :: (GlobalStateTypes m, BlockStateOperations m, BlockPointerMonad m)
                => UpdatableBlockState m -> BlockPointer m -> BlockPointer m -> Slot -> BakerId -> m (UpdatableBlockState m)
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
      bshandle2 <- bsoModifyAccount bshandle1
         (emptyAccountUpdate (acc ^. accountAddress) & auAmount ?~ (amountToDelta (executionReward + bakingReward)))
      -- record the block reward transaction in the transaction outcomes for this block
      bsoAddSpecialTransactionOutcome bshandle2 (BakingReward bid (acc ^. accountAddress) (executionReward + bakingReward))


-- |Execute a block from a given starting state.
-- Fail if any of the transactions fails, otherwise return the new 'BlockState' and the amount of energy used
-- during this block execution.
executeFrom :: forall m .
  (GlobalStateTypes m, BlockPointerMonad m, TreeStateMonad m)
  => BlockHash -- ^Hash of the block we are executing. Used only for commiting transactions.
  -> Slot -- ^Slot number of the block being executed.
  -> Timestamp -- ^Unix timestamp of the beginning of the slot.
  -> BlockPointer m  -- ^Parent pointer from which to start executing
  -> BlockPointer m  -- ^Last finalized block pointer.
  -> BakerId -- ^Identity of the baker who should be rewarded.
  -> BirkParameters
  -> [Transaction] -- ^Transactions on this block.
  -> m (Either (Maybe FailureKind) (BlockState m, Energy))
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
    genBetaAccounts <- HashSet.fromList . map _accountAddress . genesisSpecialBetaAccounts <$> getGenesisData
    maxBlockEnergy <- genesisMaxBlockEnergy <$> getGenesisData
    let context = ContextState{
          _specialBetaAccounts = genBetaAccounts,
          _chainMetadata = cm,
          _maxBlockEnergy = maxBlockEnergy
          }
    (res, finState) <- runBSM (Sch.runTransactions txs) context (mkInitialSS bshandle1 :: SchedulerState m)
    let usedEnergy = finState ^. ssSchedulerEnergyUsed
    let bshandle2 = finState ^. ssBlockState
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
            return (Right (finalbsHandle, usedEnergy))

-- |PRECONDITION: Focus block is the parent block of the block we wish to make,
-- hence the pending transaction table is correct for the new block.
-- EFFECTS: This function only updates the block state. It has no effects on the transaction table.
-- POSTCONDITION: The function always returns a list of transactions which make a valid block in `ftAdded`,
-- and also returns a list of transactions which failed, and a list of those which were not processed.
constructBlock :: forall m .
  (GlobalStateTypes m, BlockPointerMonad m, TreeStateMonad m)
  => Slot -- ^Slot number of the block to bake
  -> Timestamp -- ^Unix timestamp of the beginning of the slot.
  -> BlockPointer m -- ^Parent pointer from which to start executing
  -> BlockPointer m -- ^Last finalized block pointer.
  -> BakerId -- ^The baker of the block.
  -> BirkParameters
  -> m (Sch.FilteredTransactions Transaction, BlockState m, Energy)
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

    -- now we get transactions for each of the pending accounts.
    txs <- forM (HM.toList pt) $ \(acc, (l, _)) -> do
      accTxs <- getAccountNonFinalized acc l
      -- now find for each account the least arrival time of a transaction
      let txsList = concatMap (Set.toList . snd) accTxs
      let minTime = minimum $ map trArrivalTime txsList
      return (txsList, minTime)

    -- FIXME: This is inefficient and should be changed. Doing it only to get the integration working.
    -- Order the accounts by the arrival time of the earliest transaction.
    let orderedTxs = fst . unzip $ List.sortOn snd txs
    genBetaAccounts <- HashSet.fromList . map _accountAddress . genesisSpecialBetaAccounts <$> getGenesisData
    maxBlockEnergy <- genesisMaxBlockEnergy <$> getGenesisData
    let context = ContextState{
          _specialBetaAccounts = genBetaAccounts,
          _chainMetadata = cm,
          _maxBlockEnergy = maxBlockEnergy
          }
    (ft@Sch.FilteredTransactions{..}, finState) <-
        runBSM (Sch.filterTransactions (fromIntegral maxSize) orderedTxs) context (mkInitialSS bshandle1 :: SchedulerState m)
    -- FIXME: At some point we should log things here using the same logging infrastructure as in consensus.

    let usedEnergy = finState ^. ssSchedulerEnergyUsed
    let bshandle2 = finState ^. ssBlockState

    bshandle3 <- bsoSetTransactionOutcomes bshandle2 (map snd ftAdded)
    bshandle4 <- mintAndReward bshandle3 blockParent lfPointer slotNumber blockBaker

    bshandleFinal <- freezeBlockState bshandle4
    return (ft, bshandleFinal, usedEnergy)
