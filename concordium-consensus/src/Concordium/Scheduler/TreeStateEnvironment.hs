{-# LANGUAGE RecordWildCards #-}
module Concordium.Scheduler.TreeStateEnvironment where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import Control.Monad

import Concordium.Types
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockState
import Concordium.Scheduler.Types
import Concordium.Scheduler.EnvironmentImplementation

import Data.Maybe(fromMaybe)

import Lens.Micro.Platform

import qualified Concordium.Scheduler as Sch

-- |Execute a block from a given starting state.
-- Fail if any of the transactions fails, otherwise return the new 'BlockState'.
executeFrom ::
  Slot -- ^Slot number of the block being executed.
  -> BlockPointer  -- ^Parent pointer from which to start executing
  -> BlockPointer -- ^Last finalized block pointer.
  -> [Transaction] -- ^Transactions on this block.
  -> Either FailureKind BlockState
executeFrom slotNumber blockParent lfPointer txs =
  let cm = let blockHeight = bpHeight blockParent + 1
               finalizedHeight = bpHeight lfPointer
           in ChainMetadata{..}
      res = runSI (Sch.execTransactions txs) cm (bpState blockParent)
  in case res of
       (Right _, bs) -> Right bs
       (Left fk, _) -> Left fk

-- |PRECONDITION: Focus block is the parent block of the block we wish to make,
-- hence the pending transaction table is correct for the new block.
-- EFFECTS: After execution all transactions that were not added to the block are purged from
-- the transaction table. If the purging is successful then the transaction is
-- also removed from the pending table. Moreover all transactions which were added to the block
-- are removed from the pending table.
-- INVARIANT: The function always returns a list of transactions which make a valid block.
constructBlock ::
  TreeStateMonad m
  => Slot -- ^Slot number of the block to bake
  -> BlockPointer -- ^Parent pointer from which to start executing
  -> BlockPointer -- ^Last finalized block pointer.
  -> m ([Transaction], BlockState)
constructBlock slotNumber blockParent lfPointer =
  let cm = let blockHeight = bpHeight blockParent + 1
               finalizedHeight = bpHeight lfPointer
           in ChainMetadata{..}
  in do
    pt <- getPendingTransactions
    -- now the set is ordered by accounts
    txSet <- mapM (\(acc, (l, _)) -> fmap snd <$> getAccountNonFinalized acc l) (HM.toList pt)
    -- FIXME: This is inefficient and should be changed. Doing it only to get the integration working.
    let txs = concatMap (concatMap Set.toList) txSet
    let ((valid, invalid), bs) = runSI (Sch.filterTransactions txs) cm (bpState blockParent)
    -- FIXME: At some point we should log things here using the same logging infrastructure as in consensus.

    -- We first commit all valid transactions to the current block slot to prevent them being purged.
    -- At the same time we construct the return blockTransactions to avoid an additional traversal
    ret <- mapM (\(tx, _) -> tx <$ commitTransaction slotNumber tx) valid
    

    -- Now we need to try to purge each invalid transaction from the pending table.
    -- Moreover all transactions successfully added will be removed from the pending table.
    -- Or equivalently, only a subset of invalid transactions will remain in the pending table.
    let nextNonceFor acct = fromMaybe minNonce (bs ^? blockAccounts . ix acct . accountNonce)
    newpt <- foldM (\cpt (tx, _) -> do b <- purgeTransaction tx
                                       return $! if b then cpt  -- if the transaction was purged don't put it back into the pending table
                                                 else (extendPendingTransactionTable (nextNonceFor $ transactionSender tx) tx cpt))  -- but otherwise do
                   emptyPendingTransactionTable
                   invalid
    -- commit the new pending transactions to the tree state
    putPendingTransactions newpt

    return (ret, bs)
