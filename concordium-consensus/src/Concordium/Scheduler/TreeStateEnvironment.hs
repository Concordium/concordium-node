{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.TreeStateEnvironment where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import Control.Monad

import Concordium.Types
import Concordium.GlobalState.TreeState
import Concordium.Scheduler.Types
import Concordium.Scheduler.Environment
import qualified Concordium.GlobalState.Modules as Modules

import Control.Monad.RWS.Strict

import Lens.Micro.Platform

import qualified Concordium.Scheduler as Sch

newtype BlockStateMonad state m a = BSM { _runBSM :: RWST ChainMetadata () state m a}
    deriving (Functor, Applicative, Monad, MonadState state, MonadReader ChainMetadata, MonadTrans)

runBSM :: Monad m => BlockStateMonad b m a -> ChainMetadata -> b -> m (a, b)
runBSM m cm s = do
  (r, s', ()) <- runRWST (_runBSM m) cm s
  return (r, s')

instance (UpdatableBlockState m ~ state, BlockStateOperations m) => StaticEnvironmentMonad (BlockStateMonad state m) where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = ask

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces mref = do
    s <- get
    mmod <- lift (bsoGetModule s mref)
    case mmod of
      Nothing -> return Nothing
      Just m -> return (Just (Modules.moduleInterface m, Modules.moduleValueInterface m))

instance (UpdatableBlockState m ~ state, BlockStateOperations m) => SchedulerMonad (BlockStateMonad state m) where
  {-# INLINE getContractInstance #-}
  getContractInstance addr = lift . flip bsoGetInstance addr =<< get

  {-# INLINE getAccount #-}
  getAccount addr = lift . flip bsoGetAccount addr =<< get
  
  {-# INLINE putNewInstance #-}
  putNewInstance mkInstance = do
    (caddr, s') <- lift . flip bsoPutNewInstance mkInstance =<< get
    put s'
    return caddr

  putNewAccount account = do
    (res, s') <- lift . flip bsoPutNewAccount account =<< get
    put s'
    return res

  accountRegIdExists regid =
    lift . flip bsoRegIdExists regid =<< get

  commitModule mhash iface viface = do
    (res, s') <- lift . (\s -> bsoPutNewModule s mhash iface viface) =<< get
    put s'
    return res

  increaseAccountNonce addr = do
    s <- get
    macc <- lift $ bsoGetAccount s addr
    case macc of
      Nothing -> error "increaseAccountNonce precondition violated."
      Just acc ->
        let nonce = acc ^. accountNonce in do
        s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auNonce ?~ (nonce + 1)))
        put s'


  addAccountCredential addr cdi = do
    s <- get
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auCredential ?~ cdi))
    put s'

  addAccountEncryptionKey addr encKey = do
    s <- get
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auEncryptionKey ?~ encKey))
    put s'


  commitStateAndAccountChanges cs = do
    s <- get
    s' <- lift (foldM (\s' (addr, (amnt, val)) -> bsoModifyInstance s' addr amnt val)
                      s
                      (HM.toList (cs ^. instanceUpdates)))
    s'' <- lift (foldM bsoModifyAccount s' (cs ^. accountUpdates))
    put s''

-- |Execute a block from a given starting state.
-- Fail if any of the transactions fails, otherwise return the new 'BlockState'.
executeFrom ::
  TreeStateMonad m
  => Slot -- ^Slot number of the block being executed.
  -> BlockPointer m  -- ^Parent pointer from which to start executing
  -> BlockPointer m  -- ^Last finalized block pointer.
  -> [Transaction] -- ^Transactions on this block.
  -> m (Either FailureKind (BlockState m))
executeFrom slotNumber blockParent lfPointer txs =
  let cm = let blockHeight = bpHeight blockParent + 1
               finalizedHeight = bpHeight lfPointer
           in ChainMetadata{..}
  in do
    bshandle <- thawBlockState (bpState blockParent)
    (res, bshandle') <- runBSM (Sch.execTransactions txs) cm bshandle
    finalbsHandle <- freezeBlockState bshandle'
    case res of
      Left fk -> Left fk <$ purgeBlockState finalbsHandle
      Right () -> return (Right finalbsHandle)

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
  -> BlockPointer m -- ^Parent pointer from which to start executing
  -> BlockPointer m -- ^Last finalized block pointer.
  -> m ([Transaction], BlockState m)
constructBlock slotNumber blockParent lfPointer =
  let cm = let blockHeight = bpHeight blockParent + 1
               finalizedHeight = bpHeight lfPointer
           in ChainMetadata{..}
  in do
    bshandle <- thawBlockState (bpState blockParent)
    pt <- getPendingTransactions
    -- now the set is ordered by accounts
    txSet <- mapM (\(acc, (l, _)) -> fmap snd <$> getAccountNonFinalized acc l) (HM.toList pt)
    -- FIXME: This is inefficient and should be changed. Doing it only to get the integration working.
    let txs = concatMap (concatMap Set.toList) txSet
    ((valid, invalid), bshandle') <- runBSM (Sch.filterTransactions txs) cm bshandle
    -- FIXME: At some point we should log things here using the same logging infrastructure as in consensus.

    -- We first commit all valid transactions to the current block slot to prevent them being purged.
    -- At the same time we construct the return blockTransactions to avoid an additional traversal
    ret <- mapM (\(tx, _) -> tx <$ commitTransaction slotNumber tx) valid
    
    -- Now we need to try to purge each invalid transaction from the pending table.
    -- Moreover all transactions successfully added will be removed from the pending table.
    -- Or equivalently, only a subset of invalid transactions will remain in the pending table.
    let nextNonceFor addr = do
          macc <- bsoGetAccount bshandle' addr
          case macc of
            Nothing -> return minNonce
            Just acc -> return $ acc ^. accountNonce
    newpt <- foldM (\cpt (tx, _) -> do b <- purgeTransaction tx
                                       if b then return cpt  -- if the transaction was purged don't put it back into the pending table
                                       else do
                                           nonce <- nextNonceFor (transactionSender tx)
                                           return $! (extendPendingTransactionTable nonce tx cpt))  -- but otherwise do
                   emptyPendingTransactionTable
                   invalid
    -- commit the new pending transactions to the tree state
    putPendingTransactions newpt
    bshandleFinal <- freezeBlockState bshandle'
    return (ret, bshandleFinal)
