{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies, DerivingVia, StandaloneDeriving #-}
module Concordium.Scheduler.EnvironmentImplementation where

import Concordium.Scheduler.Environment

import Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Functor.Identity

import Lens.Micro.Platform

import Control.Monad.Reader
import Control.Monad.Trans.RWS.Strict hiding (ask, get, put)
import Control.Monad.State.Class

import Concordium.Scheduler.Types
import Concordium.GlobalState.BlockState hiding (BlockState)
import Concordium.GlobalState.Implementation.BlockState (BlockState, PureBlockStateMonad(..))
import qualified Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Bakers as Bakers

import qualified Acorn.Core as Core

newtype BSOMonadWrapper r s m a = BSOMonadWrapper (m a)
    deriving (Functor, Applicative, Monad, MonadReader r, MonadState s)

instance MonadTrans (BSOMonadWrapper r s) where
    {-# INLINE lift #-}
    lift a = BSOMonadWrapper a

-- |Chain metadata together with a set of special accounts which have special
-- rights during the beta phase.
type ContextState = (Set.HashSet AccountAddress, ChainMetadata)

instance (MonadReader ContextState m, UpdatableBlockState m ~ s, MonadState s m, BlockStateOperations m)
    => StaticEnvironmentMonad Core.UA (BSOMonadWrapper ContextState s m) where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = view _2

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces mref = do
    s <- get
    mmod <- lift (bsoGetModule s mref)
    return $ mmod <&> \m -> (Mod.moduleInterface m, Mod.moduleValueInterface m)

instance (MonadReader ContextState m, UpdatableBlockState m ~ state, MonadState state m, BlockStateOperations m)
         => SchedulerMonad (BSOMonadWrapper ContextState state m) where

  {-# INLINE getSpecialBetaAccounts #-}
  getSpecialBetaAccounts = view _1

  {-# INLINE getContractInstance #-}
  getContractInstance addr = lift . flip bsoGetInstance addr =<< get

  {-# INLINE getAccount #-}
  getAccount !addr = lift . flip bsoGetAccount addr =<< get

  {-# INLINE putNewInstance #-}
  putNewInstance !mkInstance = do
    (caddr, s') <- lift . flip bsoPutNewInstance mkInstance =<< get
    put s'
    return caddr

  {-# INLINE putNewAccount #-}
  putNewAccount !account = do
    (res, s') <- lift . flip bsoPutNewAccount account =<< get
    put s'
    return res

  {-# INLINE accountRegIdExists #-}
  accountRegIdExists !regid =
    lift . flip bsoRegIdExists regid =<< get

  {-# INLINE commitModule #-}
  commitModule !mhash !iface !viface !source = do
    (res, s') <- lift . (\s -> bsoPutNewModule s mhash iface viface source) =<< get
    put s'
    return res

  {-# INLINE smTryGetLinkedExpr #-}
  smTryGetLinkedExpr mref n = do
    s <- get
    lift (bsoTryGetLinkedExpr s mref n)

  {-# INLINE smPutLinkedExpr #-}
  smPutLinkedExpr mref n linked = do
    s <- get
    s' <- lift (bsoPutLinkedExpr s mref n linked)
    put s'

  {-# INLINE smTryGetLinkedContract #-}
  smTryGetLinkedContract mref n = do
    s <- get
    lift (bsoTryGetLinkedContract s mref n)

  {-# INLINE smPutLinkedContract #-}
  smPutLinkedContract mref n linked = do
    s <- get
    s' <- lift (bsoPutLinkedContract s mref n linked)
    put s'

  {-# INLINE increaseAccountNonce #-}
  increaseAccountNonce acc = do
    s <- get
    let nonce = acc ^. accountNonce
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auNonce ?~ (nonce + 1)))
    put s'

    where addr = acc ^. accountAddress

  {-# INLINE addAccountCredential #-}
  addAccountCredential !acc !cdi = do
    s <- get
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auCredential ?~ cdi))
    put s'

   where addr = acc ^. accountAddress

  {-# INLINE addAccountEncryptionKey #-}
  addAccountEncryptionKey !acc !encKey = do
    s <- get
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auEncryptionKey ?~ encKey))
    put s'
   where addr = acc ^. accountAddress

  {-# INLINE commitChanges #-}
  commitChanges !cs = do
    s <- get
    -- INVARIANT: the invariant which should hold at this point is that any
    -- changed instance must exist in the global state moreover all instances
    -- are distinct by the virtue of a HashMap being a function
    s' <- lift (foldM (\s' (addr, (amnt, val)) -> bsoModifyInstance s' addr amnt val)
                      s
                      (Map.toList (cs ^. instanceUpdates)))
    s'' <- lift (foldM bsoModifyAccount s' (cs ^. accountUpdates))
    -- store linked expressions into the cache, but only from successful transactions.
    -- if contract initialization failed, or if the receive function rejected the transaction
    -- we ignore the linked cache.
    s''' <- lift (foldM (\curs ((mref, n), linked) -> bsoPutLinkedExpr curs mref n linked) s'' (Map.toList (cs ^. linkedExprs)))
    s'''' <- lift (foldM (\curs ((mref, n), linked) -> bsoPutLinkedContract curs mref n linked) s''' (Map.toList (cs ^. linkedContracts)))
    put s''''

  -- FIXME: Make this variable base on block state
  {-# INLINE energyToGtu #-}
  energyToGtu = return . fromIntegral

  {-# INLINE notifyExecutionCost #-}
  notifyExecutionCost !amnt = do
    s <- get
    s' <- lift (bsoNotifyExecutionCost s amnt)
    put s'

  {-# INLINE notifyIdentityProviderCredential #-}
  notifyIdentityProviderCredential !idk = do
    s <- get
    s' <- lift (bsoNotifyIdentityIssuerCredential s idk)
    put s'

  {-# INLINE getBakerInfo #-}
  getBakerInfo bid = do
    s <- get
    lift (bsoGetBakerInfo s bid)

  {-# INLINE addBaker #-}
  addBaker binfo = do
    s <- get
    (bid, s') <- lift (bsoAddBaker s binfo)
    put s'
    return bid

  {-# INLINE removeBaker #-}
  removeBaker bid = do
    s <- get
    (_, s') <- lift (bsoRemoveBaker s bid)
    put s'

  {-# INLINE updateBakerSignKey #-}
  updateBakerSignKey bid signKey = do
    s <- get
    s' <- lift (bsoUpdateBaker s (emptyBakerUpdate bid & buSignKey ?~ signKey))
    put s'

  {-# INLINE updateBakerAccount #-}
  updateBakerAccount bid bacc = do
    s <- get
    s' <- lift (bsoUpdateBaker s (emptyBakerUpdate bid & buAccount ?~ bacc))
    put s'

  {-# INLINE delegateStake #-}
  delegateStake acc bid = do
    s <- get
    (r, s') <- lift (bsoDelegateStake s acc bid)
    put s'
    return r

  {-# INLINE getIPInfo #-}
  getIPInfo ipId = do
    s <- get
    lift (bsoGetIdentityProvider s ipId)

  {-# INLINE getCrypoParams #-}
  getCrypoParams = lift . bsoGetCryptoParams =<< get

newtype SchedulerImplementation a = SchedulerImplementation { _runScheduler :: RWST ContextState () BlockState (PureBlockStateMonad Identity) a }
    deriving (Functor, Applicative, Monad, MonadReader ContextState, MonadState BlockState)
    deriving (StaticEnvironmentMonad Core.UA) via (BSOMonadWrapper ContextState BlockState (RWST ContextState () BlockState (PureBlockStateMonad Identity)))
    deriving SchedulerMonad via (BSOMonadWrapper ContextState BlockState (RWST ContextState () BlockState (PureBlockStateMonad Identity)))

runSI :: SchedulerImplementation a -> SpecialBetaAccounts -> ChainMetadata -> BlockState -> (a, BlockState)
runSI sc gd cd gs = let (a, s, _) = runIdentity $ runPureBlockStateMonad $ runRWST (_runScheduler sc) (gd, cd) gs in (a, s)

execSI :: SchedulerImplementation a -> SpecialBetaAccounts -> ChainMetadata -> BlockState -> BlockState
execSI sc gd cd gs = fst (runIdentity $ runPureBlockStateMonad $ execRWST (_runScheduler sc) (gd, cd) gs)

evalSI :: SchedulerImplementation a -> SpecialBetaAccounts -> ChainMetadata -> BlockState -> a
evalSI sc gd cd gs = fst (runIdentity $ runPureBlockStateMonad $ evalRWST (_runScheduler sc) (gd, cd) gs)
