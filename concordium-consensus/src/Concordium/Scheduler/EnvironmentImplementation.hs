{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
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
import Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.TransactionLogs
import Concordium.GlobalState.Basic.BlockState (BlockState, PureBlockStateMonad(..))
import Concordium.GlobalState.TreeState hiding (BlockState)
import Concordium.GlobalState.Bakers as Bakers

import qualified Acorn.Core as Core

-- |Chain metadata together with a set of special accounts which have special
-- rights during the beta phase, as well as 
data ContextState = ContextState{
  _specialBetaAccounts :: !(Set.HashSet AccountAddress),
  _chainMetadata :: !ChainMetadata,
  _maxBlockEnergy :: !Energy
  }

makeLenses ''ContextState

-- Doing it manually because otherwise the generated instance definition
-- seems to be wrong (m has kind *).
class HasSchedulerState a where
  type SS a
  blockState :: Lens' a (SS a)
  schedulerEnergyUsed :: Lens' a Energy

data SchedulerState (m :: * -> *)= SchedulerState {
  _ssBlockState :: !(UpdatableBlockState m),
  _ssSchedulerEnergyUsed :: !Energy
  }

mkInitialSS :: UpdatableBlockState m -> SchedulerState m
mkInitialSS _ssBlockState = SchedulerState{_ssSchedulerEnergyUsed = 0,..}

makeLenses ''SchedulerState

instance HasSchedulerState (SchedulerState m) where
  type SS (SchedulerState m) = UpdatableBlockState m
  blockState = ssBlockState
  schedulerEnergyUsed = ssSchedulerEnergyUsed

newtype BSOMonadWrapper r state m a = BSOMonadWrapper (m a)
    deriving (Functor, Applicative, Monad, MonadReader r, MonadState state)

instance MonadTrans (BSOMonadWrapper r s) where
    {-# INLINE lift #-}
    lift = BSOMonadWrapper

instance TransactionLogger m => TransactionLogger (BSOMonadWrapper r s m) where
  {-# INLINE tlNotifyAccountEffect #-}
  tlNotifyAccountEffect txHash = lift . tlNotifyAccountEffect txHash

instance (MonadReader ContextState m,
          SS state ~ UpdatableBlockState m,
          HasSchedulerState state,
          MonadState state m,
          BlockStateOperations m)
    => StaticEnvironmentMonad Core.UA (BSOMonadWrapper ContextState state m) where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = view chainMetadata

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces mref = do
    s <- use blockState
    mmod <- lift (bsoGetModule s mref)
    return $! mmod <&> \m -> (BS.moduleInterface m, BS.moduleValueInterface m)

instance (MonadReader ContextState m,
          SS state ~ UpdatableBlockState m,
          HasSchedulerState state,
          MonadState state m,
          BlockStateOperations m,
          TransactionLogger m)
         => SchedulerMonad (BSOMonadWrapper ContextState state m) where

  {-# INLINE markEnergyUsed #-}
  markEnergyUsed energy = schedulerEnergyUsed += energy

  {-# INLINE getUsedEnergy #-}
  getUsedEnergy = use schedulerEnergyUsed

  {-# INLINE getMaxBlockEnergy #-}
  getMaxBlockEnergy = view maxBlockEnergy

  {-# INLINE getSpecialBetaAccounts #-}
  getSpecialBetaAccounts = view specialBetaAccounts

  {-# INLINE getContractInstance #-}
  getContractInstance addr = lift . flip bsoGetInstance addr =<< use blockState

  {-# INLINE getAccount #-}
  getAccount !addr = lift . flip bsoGetAccount addr =<< use blockState

  {-# INLINE putNewInstance #-}
  putNewInstance !mkInstance = do
    (caddr, s') <- lift . flip bsoPutNewInstance mkInstance =<< use blockState
    blockState .= s'
    return caddr

  {-# INLINE putNewAccount #-}
  putNewAccount !account = do
    (res, s') <- lift . flip bsoPutNewAccount account =<< use blockState
    blockState .= s'
    return res

  {-# INLINE accountRegIdExists #-}
  accountRegIdExists !regid =
    lift . flip bsoRegIdExists regid =<< use blockState

  {-# INLINE commitModule #-}
  commitModule !mhash !iface !viface !source = do
    (res, s') <- lift . (\s -> bsoPutNewModule s mhash iface viface source) =<< use blockState
    blockState .= s'
    return res

  {-# INLINE smTryGetLinkedExpr #-}
  smTryGetLinkedExpr mref n = do
    s <- use blockState
    lift (bsoTryGetLinkedExpr s mref n)

  {-# INLINE smPutLinkedExpr #-}
  smPutLinkedExpr mref n linked = do
    s <- use blockState
    s' <- lift (bsoPutLinkedExpr s mref n linked)
    blockState .= s'

  {-# INLINE smTryGetLinkedContract #-}
  smTryGetLinkedContract mref n = do
    s <- use blockState
    lift (bsoTryGetLinkedContract s mref n)

  {-# INLINE smPutLinkedContract #-}
  smPutLinkedContract mref n linked = do
    s <- use blockState
    s' <- lift (bsoPutLinkedContract s mref n linked)
    blockState .= s'

  {-# INLINE increaseAccountNonce #-}
  increaseAccountNonce acc = do
    s <- use blockState
    let nonce = acc ^. accountNonce
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auNonce ?~ (nonce + 1)))
    blockState .= s'

    where addr = acc ^. accountAddress

  {-# INLINE addAccountCredential #-}
  addAccountCredential !acc !cdi = do
    s <- use blockState
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auCredential ?~ cdi))
    blockState .= s'

   where addr = acc ^. accountAddress

  {-# INLINE commitChanges #-}
  commitChanges !cs = do
    let txHash = cs ^. affectedTx
    s <- use blockState
    -- ASSUMPTION: the property which should hold at this point is that any
    -- changed instance must exist in the global state moreover all instances
    -- are distinct by the virtue of a HashMap being a function
    s' <- lift (foldM (\s' (addr, (amnt, val)) -> bsoModifyInstance s' addr amnt val)
                      s
                      (Map.toList (cs ^. instanceUpdates)))
    -- Notify account transfers, but also 
    s'' <- lift (foldM (\curState accUpdate -> do
                           tlNotifyAccountEffect txHash (accUpdate ^. auAddress)
                           bsoModifyAccount curState accUpdate
                       )
                  s'
                  (cs ^. accountUpdates))
    -- store linked expressions into the cache, but only from successful transactions.
    -- if contract initialization failed, or if the receive function rejected the transaction
    -- we ignore the linked cache.
    s''' <- lift (foldM (\curs ((mref, n), linked) -> bsoPutLinkedExpr curs mref n linked) s'' (Map.toList (cs ^. linkedExprs)))
    s'''' <- lift (foldM (\curs ((mref, n), linked) -> bsoPutLinkedContract curs mref n linked) s''' (Map.toList (cs ^. linkedContracts)))
    blockState .= s''''

  -- FIXME: Make this variable based on block state
  {-# INLINE energyToGtu #-}
  energyToGtu = return . fromIntegral

  {-# INLINE notifyExecutionCost #-}
  notifyExecutionCost !amnt = do
    s <- use blockState
    s' <- lift (bsoNotifyExecutionCost s amnt)
    blockState .= s'

  {-# INLINE notifyIdentityProviderCredential #-}
  notifyIdentityProviderCredential !idk = do
    s <- use blockState
    s' <- lift (bsoNotifyIdentityIssuerCredential s idk)
    blockState .= s'

  {-# INLINE getBakerInfo #-}
  getBakerInfo bid = do
    s <- use blockState
    lift (bsoGetBakerInfo s bid)

  {-# INLINE addBaker #-}
  addBaker binfo = do
    s <- use blockState
    (bid, s') <- lift (bsoAddBaker s binfo)
    blockState .= s'
    return bid

  {-# INLINE removeBaker #-}
  removeBaker bid = do
    s <- use blockState
    (_, s') <- lift (bsoRemoveBaker s bid)
    blockState .= s'

  {-# INLINE updateBakerSignKey #-}
  updateBakerSignKey bid signKey = do
    s <- use blockState
    (r, s') <- lift (bsoUpdateBaker s (emptyBakerUpdate bid & buSignKey ?~ signKey))
    blockState .= s'
    return r

  {-# INLINE updateBakerAccount #-}
  updateBakerAccount bid bacc = do
    s <- use blockState
    (_, s') <- lift (bsoUpdateBaker s (emptyBakerUpdate bid & buAccount ?~ bacc))
    -- updating the account cannot fail, so we ignore the return value.
    blockState .= s'

  {-# INLINE delegateStake #-}
  delegateStake acc bid = do
    s <- use blockState
    (r, s') <- lift (bsoDelegateStake s acc bid)
    blockState .= s'
    return r

  {-# INLINE getIPInfo #-}
  getIPInfo ipId = do
    s <- use blockState
    lift (bsoGetIdentityProvider s ipId)

  {-# INLINE getCrypoParams #-}
  getCrypoParams = lift . bsoGetCryptoParams =<< use blockState

-- Pure block state scheduler state
type PBSSS = SchedulerState (PureBlockStateMonad Identity)
type RWSTBS m a = RWST ContextState () PBSSS m a

-- |Basic implementation of the scheduler that does no transaction logging.
newtype SchedulerImplementation a = SchedulerImplementation { _runScheduler :: RWSTBS (PureBlockStateMonad Identity) a }
    deriving (Functor, Applicative, Monad, MonadReader ContextState, MonadState PBSSS)
    deriving TransactionLogger via NoTransactionLogger (RWST ContextState () PBSSS (PureBlockStateMonad Identity))
    deriving (StaticEnvironmentMonad Core.UA)
      via (BSOMonadWrapper ContextState PBSSS (MGSTrans (RWST ContextState () PBSSS) (PureBlockStateMonad Identity)))
    deriving SchedulerMonad via (BSOMonadWrapper ContextState PBSSS (MGSTrans (RWST ContextState () PBSSS) (PureBlockStateMonad Identity)))

runSI :: SchedulerImplementation a -> SpecialBetaAccounts -> ChainMetadata -> Energy -> BlockState -> (a, PBSSS)
runSI sc gd cd energy gs =
  let (a, s, _) = runIdentity $ runPureBlockStateMonad $ runRWST (_runScheduler sc) (ContextState gd cd energy) (SchedulerState gs 0) in (a, s)

execSI :: SchedulerImplementation a -> SpecialBetaAccounts -> ChainMetadata -> Energy -> BlockState -> PBSSS
execSI sc gd cd energy gs = fst (runIdentity $ runPureBlockStateMonad $ execRWST (_runScheduler sc) (ContextState gd cd energy) (SchedulerState gs 0))

evalSI :: SchedulerImplementation a -> SpecialBetaAccounts -> ChainMetadata -> Energy -> BlockState -> a
evalSI sc gd cd energy gs = fst (runIdentity $ runPureBlockStateMonad $ evalRWST (_runScheduler sc) (ContextState gd cd energy) (SchedulerState gs 0))
