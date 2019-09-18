{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia, DerivingStrategies, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.TreeStateEnvironment where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.List as List

import Debug.Trace

import Control.Monad

import Concordium.Types
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block(blockSlot)
import Concordium.Scheduler.Types
import Concordium.Scheduler.Environment
import Concordium.Scheduler.EnvironmentImplementation (BSOMonadWrapper(..))

import Control.Monad.RWS.Strict

import Lens.Micro.Platform

import qualified Acorn.Core as Core

import qualified Concordium.Scheduler as Sch

newtype BlockStateMonad state m a = BSM { _runBSM :: RWST ChainMetadata () state m a}
    deriving (Functor, Applicative, Monad, MonadState state, MonadReader ChainMetadata, MonadTrans)

deriving via (BSOMonadWrapper ChainMetadata state (RWST ChainMetadata () state m))
    instance (UpdatableBlockState m ~ state, BlockStateOperations m) => StaticEnvironmentMonad Core.UA (BlockStateMonad state m)

deriving via (BSOMonadWrapper ChainMetadata state (RWST ChainMetadata () state m))
    instance (UpdatableBlockState m ~ state, BlockStateOperations m) => SchedulerMonad (BlockStateMonad state m)

runBSM :: Monad m => BlockStateMonad b m a -> ChainMetadata -> b -> m (a, b)
runBSM m cm s = do
  (r, s', ()) <- runRWST (_runBSM m) cm s
  return (r, s')

-- |Reward the baker, identity providers, ...
-- TODO: Currently the finalized pointer is not used. But the finalization committee
-- of that block might need to be rewarded if they have not been already.
-- Thus the argument is here for future use
mintAndReward :: TreeStateMonad m => UpdatableBlockState m -> BlockPointer m -> BlockPointer m -> Slot -> BakerId -> m (UpdatableBlockState m)
mintAndReward bshandle blockParent lfPointer slotNumber bid = do

  -- First we mint new currency. This can be used in rewarding bakers. First get
  -- the inflation rate of the parent block (this might have changed in the
  -- current block), and compute how much to mint based on elapsed time.
  rewardStatus <- getRewardStatus (bpState blockParent)
  let inflationRate = rewardStatus ^. mintedGTUPerSlot
  let mintedAmount = fromIntegral (slotNumber - blockSlot blockParent) * inflationRate
  (cbamount, bshandleMinted) <- bsoMint bshandle mintedAmount

  -- and now we can reward everybody
  -- first take half of the amount on the central bank and use it to reward the baker
  -- TODO: This is temporary POC. We need this fraction to be flexible.
  let bakingReward = cbamount `div` 2
  (_, bshandle1) <- bsoDecrementCentralBankGTU bshandleMinted bakingReward

  executionReward <- bsoGetExecutionCost bshandle1
  macc <- bsoGetBakerAccount bshandle1 bid
  case macc of
    Nothing -> error "Precondition violated. Baker account does not exist."
    Just acc -> do
      bshandle2 <- bsoModifyAccount bshandle1
         (emptyAccountUpdate (acc ^. accountAddress) & auAmount ?~ (amountToDelta (executionReward + bakingReward)))
      -- record the block reward transaction in the transaction outcomes for this block
      bsoAddSpecialTransactionOutcome bshandle2 (BakingReward (acc ^. accountAddress) (executionReward + bakingReward))

updateSeed :: TreeStateMonad m => UpdatableBlockState m -> Slot -> BlockNonce -> m (UpdatableBlockState m)
updateSeed bshandle slot blockNonce = do
  bshandle' <- bsoUpdateNonce bshandle slot blockNonce
  return bshandle' 


-- |Execute a block from a given starting state.
-- Fail if any of the transactions fails, otherwise return the new 'BlockState' and the amount of energy used
-- during this block execution.
executeFrom ::
  TreeStateMonad m
  => Slot -- ^Slot number of the block being executed.
  -> BlockPointer m  -- ^Parent pointer from which to start executing
  -> BlockPointer m  -- ^Last finalized block pointer.
  -> BakerId -- ^Identity of the baker who should be rewarded.
  -> BlockNonce
  -> [Transaction] -- ^Transactions on this block.
  -> m (Either FailureKind (BlockState m, Energy))
executeFrom slotNumber blockParent lfPointer blockBaker blockNonce txs =
  let cm = let blockHeight = bpHeight blockParent + 1
               finalizedHeight = bpHeight lfPointer
           in ChainMetadata{..}
  in do
    bshandle0 <- thawBlockState (bpState blockParent)
    (res, bshandle1) <- runBSM (Sch.runTransactions txs) cm bshandle0
    case res of
        Left fk -> Left fk <$ (purgeBlockState =<< freezeBlockState bshandle1)
        Right (outcomes, usedEnergy) -> do
            -- Record the transaction outcomes
            bshandle2 <- bsoSetTransactionOutcomes bshandle1 ((\(tr, o) -> (transactionHash tr, o)) <$> outcomes)
            -- the main execution is now done. At this point we must mint new currencty
            -- and reward the baker and other parties.
            bshandle3 <- mintAndReward bshandle2 blockParent lfPointer slotNumber blockBaker
            bshandle4 <- updateSeed bshandle3 slotNumber blockNonce

            finalbsHandle <- freezeBlockState bshandle4
            return (Right (finalbsHandle, usedEnergy))

-- |PRECONDITION: Focus block is the parent block of the block we wish to make,
-- hence the pending transaction table is correct for the new block.
-- EFFECTS: After execution all transactions that were not added to the block are purged from
-- the transaction table. If the purging is successful then the transaction is
-- also removed from the pending table. Moreover all transactions which were added to the block
-- are removed from the pending table.
-- POSTCONDITION: The function always returns a list of transactions which make a valid block.
constructBlock ::
  TreeStateMonad m
  => Slot -- ^Slot number of the block to bake
  -> BlockPointer m -- ^Parent pointer from which to start executing
  -> BlockPointer m -- ^Last finalized block pointer.
  -> BakerId -- ^The baker of the block.
  -> BlockNonce
  -> m ([Transaction], BlockState m, Energy)
constructBlock slotNumber blockParent lfPointer blockBaker blockNonce =
  let cm = let blockHeight = bpHeight blockParent + 1
               finalizedHeight = bpHeight lfPointer
           in ChainMetadata{..}
  in do
    bshandle0 <- thawBlockState (bpState blockParent)
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
    -- In the future we do not want to do concatMap since we gain advantage from grouping transactions
    -- by account (e.g., we only need to look up the account once).
    let orderedTxs = concatMap fst $ List.sortOn snd txs

    ((Sch.FilteredTransactions{..}, usedEnergy), bshandle1) <- runBSM (Sch.filterTransactions (fromIntegral maxSize) orderedTxs) cm bshandle0
    -- FIXME: At some point we should log things here using the same logging infrastructure as in consensus.

    bshandle2 <- bsoSetTransactionOutcomes bshandle1 ((\(tr,res) -> (transactionHash tr, res)) <$> ftAdded)
    bshandle3 <- mintAndReward bshandle2 blockParent lfPointer slotNumber blockBaker
    bshandle4 <- updateSeed bshandle3 slotNumber blockNonce

    -- We first commit all valid transactions to the current block slot to prevent them being purged.
    -- At the same time we construct the return blockTransactions to avoid an additional traversal
    ret <- mapM (\(tx, _) -> tx <$ commitTransaction slotNumber tx) ftAdded

    -- Now we need to try to purge each invalid transaction from the pending table.
    -- Moreover all transactions successfully added will be removed from the pending table.
    -- Or equivalently, only a subset of invalid transactions and all the
    -- transactions we have not touched and are small enough will remain in the
    -- pending table.
    let nextNonceFor addr = do
          macc <- bsoGetAccount bshandle4 addr
          case macc of
            Nothing -> return minNonce
            Just acc -> return $ acc ^. accountNonce
    -- construct a new pending transaction table adding back some failed transactions.
    let purgeFailed cpt (tx, _) = do
          b <- purgeTransaction tx
          if b then return cpt  -- if the transaction was purged don't put it back into the pending table
          else do
            -- but otherwise do
            nonce <- nextNonceFor (transactionSender tx)
            return $! checkedExtendPendingTransactionTable nonce tx cpt

    newpt <- foldM purgeFailed emptyPendingTransactionTable ftFailed

    -- additionally add in the unprocessed transactions which are sufficiently small (here meaning < maxSize)
    let purgeTooBig cpt tx =
          if transactionSize tx < maxSize then do
            nonce <- nextNonceFor (transactionSender tx)
            return $! checkedExtendPendingTransactionTable nonce tx cpt
          else do
            -- only purge a transaction from the table if it is too big **and**
            -- not already commited to a recent block. if it is in a currently
            -- live block then we must not purge it to maintain the invariant
            -- that all transactions in live blocks exist in the transaction
            -- table.
            b <- purgeTransaction tx
            if b then return cpt
            else do
              nonce <- nextNonceFor (transactionSender tx)
              return $! checkedExtendPendingTransactionTable nonce tx cpt

    newpt' <- foldM purgeTooBig newpt ftUnprocessed

    traceM $ "Unprocessed transactions: " ++ show (length ftUnprocessed)

    -- commit the new pending transactions to the tree state
    putPendingTransactions newpt'
    bshandleFinal <- freezeBlockState bshandle4
    return (ret, bshandleFinal, usedEnergy)
