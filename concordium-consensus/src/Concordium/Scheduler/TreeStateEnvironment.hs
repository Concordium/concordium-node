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

import Control.Monad

import Concordium.Types
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Rewards
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
  let mintedAmount = fromIntegral (slotNumber - blockSlot (bpBlock blockParent)) * inflationRate
  (cbamount, bshandleMinted) <- bsoMint bshandle mintedAmount

  -- and now we can reward everybody
  -- first take half of the amount on the central bank and use it to reward the baker
  -- TODO: This is temporary POC. We need this fraction to be flexible.
  let bakingReward = cbamount `div` 2
  (_, bshandle') <- bsoDecrementCentralBankGTU bshandleMinted bakingReward

  executionReward <- bsoGetExecutionCost bshandle'
  macc <- bsoGetBakerAccount bshandle' bid
  case macc of
    Nothing -> error "Precondition violated. Baker account does not exist."
    Just acc -> let balance = acc ^. accountAmount
                in bsoModifyAccount bshandle' (emptyAccountUpdate (acc ^. accountAddress) & auAmount ?~ (balance + executionReward + bakingReward))

-- |Execute a block from a given starting state.
-- Fail if any of the transactions fails, otherwise return the new 'BlockState'.
executeFrom ::
  TreeStateMonad m
  => Slot -- ^Slot number of the block being executed.
  -> BlockPointer m  -- ^Parent pointer from which to start executing
  -> BlockPointer m  -- ^Last finalized block pointer.
  -> BakerId -- ^Identity of the baker who should be rewarded.
  -> [Transaction] -- ^Transactions on this block.
  -> m (Either FailureKind (BlockState m))
executeFrom slotNumber blockParent lfPointer blockBaker txs =
  let cm = let blockHeight = bpHeight blockParent + 1
               finalizedHeight = bpHeight lfPointer
           in ChainMetadata{..}
  in do
    bshandle0 <- thawBlockState (bpState blockParent)
    (res, bshandle1) <- runBSM (Sch.runTransactions txs) cm bshandle0
    case res of
        Left fk -> Left fk <$ (purgeBlockState =<< freezeBlockState bshandle1)
        Right outcomes -> do
            -- Record the transaction outcomes
            bshandle2 <- bsoSetTransactionOutcomes bshandle1 ((\(tr, o) -> (transactionHash tr, o)) <$> outcomes)
            -- the main execution is now done. At this point we must mint new currencty
            -- and reward the baker and other parties.
            bshandle3 <- mintAndReward bshandle2 blockParent lfPointer slotNumber blockBaker

            finalbsHandle <- freezeBlockState bshandle3
            return (Right finalbsHandle)

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
  -> m ([Transaction], BlockState m)
constructBlock slotNumber blockParent lfPointer blockBaker =
  let cm = let blockHeight = bpHeight blockParent + 1
               finalizedHeight = bpHeight lfPointer
           in ChainMetadata{..}
  in do
    bshandle0 <- thawBlockState (bpState blockParent)
    pt <- getPendingTransactions
    -- now the set is ordered by accounts
    txSet <- mapM (\(acc, (l, _)) -> fmap snd <$> getAccountNonFinalized acc l) (HM.toList pt)
    -- FIXME: This is inefficient and should be changed. Doing it only to get the integration working.
    let txs = concatMap (concatMap Set.toList) txSet
    ((valid, invalid), bshandle1) <- runBSM (Sch.filterTransactions txs) cm bshandle0
    -- FIXME: At some point we should log things here using the same logging infrastructure as in consensus.

    bshandle2 <- bsoSetTransactionOutcomes bshandle1 ((\(tr,res) -> (transactionHash tr, res)) <$> valid)
    bshandle3 <- mintAndReward bshandle2 blockParent lfPointer slotNumber blockBaker

    -- We first commit all valid transactions to the current block slot to prevent them being purged.
    -- At the same time we construct the return blockTransactions to avoid an additional traversal
    ret <- mapM (\(tx, _) -> tx <$ commitTransaction slotNumber tx) valid
    
    -- Now we need to try to purge each invalid transaction from the pending table.
    -- Moreover all transactions successfully added will be removed from the pending table.
    -- Or equivalently, only a subset of invalid transactions will remain in the pending table.
    let nextNonceFor addr = do
          macc <- bsoGetAccount bshandle3 addr
          case macc of
            Nothing -> return minNonce
            Just acc -> return $ acc ^. accountNonce
    newpt <- foldM (\cpt (tx, _) -> do b <- purgeTransaction tx
                                       if b then return cpt  -- if the transaction was purged don't put it back into the pending table
                                       else do
                                           nonce <- nextNonceFor (transactionSender tx)
                                           return $! (checkedExtendPendingTransactionTable nonce tx cpt))  -- but otherwise do
                   emptyPendingTransactionTable
                   invalid
    -- commit the new pending transactions to the tree state
    putPendingTransactions newpt
    bshandleFinal <- freezeBlockState bshandle3
    return (ret, bshandleFinal)
