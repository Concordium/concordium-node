{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
module Concordium.Scheduler.EnvironmentImplementation where

import Concordium.Scheduler.Environment

import Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Functor.Identity

import Lens.Micro.Platform

import Control.Monad.Reader
import Control.Monad.Writer.Strict(MonadWriter, tell, censor, listen)
import Control.Monad.Trans.RWS.Strict hiding (ask, get, put, tell, censor, listen)
import Control.Monad.State.Class

import Concordium.Scheduler.Types
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.BlockState as BS
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

-- Doing it manually because otherwise the generated class definition
-- seems to be wrong (m has kind *).
class CanExtend (AccountTransactionLog a) => HasSchedulerState a where
  type SS a
  type AccountTransactionLog a

  schedulerBlockState :: Lens' a (SS a)
  schedulerEnergyUsed :: Lens' a Energy
  accountTransactionLog :: Lens' a (AccountTransactionLog a)

data NoLogSchedulerState (m :: * -> *)= NoLogSchedulerState {
  _ssBlockState :: !(UpdatableBlockState m),
  _ssSchedulerEnergyUsed :: !Energy
  }

mkInitialSS :: UpdatableBlockState m -> NoLogSchedulerState m
mkInitialSS _ssBlockState = NoLogSchedulerState{_ssSchedulerEnergyUsed = 0,..}

makeLenses ''NoLogSchedulerState

instance HasSchedulerState (NoLogSchedulerState m) where
  type SS (NoLogSchedulerState m) = UpdatableBlockState m
  type AccountTransactionLog (NoLogSchedulerState m) = ()
  schedulerBlockState = ssBlockState
  schedulerEnergyUsed = ssSchedulerEnergyUsed
  accountTransactionLog f s = (const s) <$> (f ())

newtype BSOMonadWrapper r w state m a = BSOMonadWrapper (m a)
    deriving (Functor,
              Applicative,
              Monad,
              MonadReader r,
              MonadState state,
              MonadWriter w)

instance MonadTrans (BSOMonadWrapper r w s) where
    {-# INLINE lift #-}
    lift = BSOMonadWrapper

instance (ATITypes m, ATIStorage m ~ w) => ATITypes (BSOMonadWrapper r w s m) where
  type ATIStorage (BSOMonadWrapper r w s m) = ATIStorage m

instance (MonadReader ContextState m,
          SS state ~ UpdatableBlockState m,
          HasSchedulerState state,
          MonadState state m,
          BlockStateOperations m
          )
    => StaticEnvironmentMonad Core.UA (BSOMonadWrapper ContextState w state m) where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = view chainMetadata

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces mref = do
    s <- use schedulerBlockState
    mmod <- lift (bsoGetModule s mref)
    return $! mmod <&> \m -> (BS.moduleInterface m, BS.moduleValueInterface m)

instance (MonadReader ContextState m,
          SS state ~ UpdatableBlockState m,
          HasSchedulerState state,
          MonadState state m,
          BlockStateOperations m,
          CanRecordFootprint w,
          CanExtend (ATIStorage m),
          Footprint (ATIStorage m) ~ w,
          MonadWriter w m
         )
         => SchedulerMonad (BSOMonadWrapper ContextState w state m) where

  {-# INLINE tlNotifyAccountEffect #-}
  tlNotifyAccountEffect items summary = do
    traverseOutcomes items $ \addr -> do
      accountTransactionLog %= extendRecord addr summary

  {-# INLINE markEnergyUsed #-}
  markEnergyUsed energy = schedulerEnergyUsed += energy

  {-# INLINE getUsedEnergy #-}
  getUsedEnergy = use schedulerEnergyUsed

  {-# INLINE getMaxBlockEnergy #-}
  getMaxBlockEnergy = view maxBlockEnergy

  {-# INLINE getSpecialBetaAccounts #-}
  getSpecialBetaAccounts = view specialBetaAccounts

  {-# INLINE getContractInstance #-}
  getContractInstance addr = lift . flip bsoGetInstance addr =<< use schedulerBlockState

  {-# INLINE getAccount #-}
  getAccount !addr = lift . flip bsoGetAccount addr =<< use schedulerBlockState

  {-# INLINE putNewInstance #-}
  putNewInstance !mkInstance = do
    (caddr, s') <- lift . flip bsoPutNewInstance mkInstance =<< use schedulerBlockState
    schedulerBlockState .= s'
    return caddr

  {-# INLINE putNewAccount #-}
  putNewAccount !account = do
    (res, s') <- lift . flip bsoPutNewAccount account =<< use schedulerBlockState
    schedulerBlockState .= s'
    return res

  {-# INLINE accountRegIdExists #-}
  accountRegIdExists !regid =
    lift . flip bsoRegIdExists regid =<< use schedulerBlockState

  {-# INLINE commitModule #-}
  commitModule !mhash !iface !viface !source = do
    (res, s') <- lift . (\s -> bsoPutNewModule s mhash iface viface source) =<< use schedulerBlockState
    schedulerBlockState .= s'
    return res

  {-# INLINE smTryGetLinkedExpr #-}
  smTryGetLinkedExpr mref n = do
    s <- use schedulerBlockState
    lift (bsoTryGetLinkedExpr s mref n)

  {-# INLINE smPutLinkedExpr #-}
  smPutLinkedExpr mref n linked = do
    s <- use schedulerBlockState
    s' <- lift (bsoPutLinkedExpr s mref n linked)
    schedulerBlockState .= s'

  {-# INLINE smTryGetLinkedContract #-}
  smTryGetLinkedContract mref n = do
    s <- use schedulerBlockState
    lift (bsoTryGetLinkedContract s mref n)

  {-# INLINE smPutLinkedContract #-}
  smPutLinkedContract mref n linked = do
    s <- use schedulerBlockState
    s' <- lift (bsoPutLinkedContract s mref n linked)
    schedulerBlockState .= s'

  {-# INLINE increaseAccountNonce #-}
  increaseAccountNonce acc = do
    s <- use schedulerBlockState
    let nonce = acc ^. accountNonce
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auNonce ?~ (nonce + 1)))
    schedulerBlockState .= s'

    where addr = acc ^. accountAddress

  {-# INLINE addAccountCredential #-}
  addAccountCredential !acc !cdi = do
    s <- use schedulerBlockState
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auCredential ?~ cdi))
    schedulerBlockState .= s'

   where addr = acc ^. accountAddress

  {-# INLINE commitChanges #-}
  commitChanges !cs = do
    s <- use schedulerBlockState
    -- ASSUMPTION: the property which should hold at this point is that any
    -- changed instance must exist in the global state and moreover all instances
    -- are distinct by the virtue of a HashMap being a function
    s' <- lift (foldM (\s' (addr, (amnt, val)) -> bsoModifyInstance s' addr amnt val)
                      s
                      (Map.toList (cs ^. instanceUpdates)))
    -- Notify account transfers, but also 
    s'' <- lift (foldM (\curState accUpdate -> do
                           tell (logAccount (accUpdate ^. auAddress))
                           bsoModifyAccount curState accUpdate
                       )
                  s'
                  (cs ^. accountUpdates))
    -- store linked expressions into the cache, but only from successful transactions.
    -- if contract initialization failed, or if the receive function rejected the transaction
    -- we ignore the linked cache.
    s''' <- lift (foldM (\curs ((mref, n), linked) -> bsoPutLinkedExpr curs mref n linked) s'' (Map.toList (cs ^. linkedExprs)))
    s'''' <- lift (foldM (\curs ((mref, n), linked) -> bsoPutLinkedContract curs mref n linked) s''' (Map.toList (cs ^. linkedContracts)))
    schedulerBlockState .= s''''

  -- Observe a single transaction footprint.
  {-# INLINE observeTransactionFootprint #-}
  observeTransactionFootprint c =
    censor (const mempty) (listen c)

  -- FIXME: Make this variable based on block state
  {-# INLINE energyToGtu #-}
  energyToGtu = return . fromIntegral

  {-# INLINE notifyExecutionCost #-}
  notifyExecutionCost !amnt = do
    s <- use schedulerBlockState
    s' <- lift (bsoNotifyExecutionCost s amnt)
    schedulerBlockState .= s'

  {-# INLINE notifyIdentityProviderCredential #-}
  notifyIdentityProviderCredential !idk = do
    s <- use schedulerBlockState
    s' <- lift (bsoNotifyIdentityIssuerCredential s idk)
    schedulerBlockState .= s'

  {-# INLINE getBakerInfo #-}
  getBakerInfo bid = do
    s <- use schedulerBlockState
    lift (bsoGetBakerInfo s bid)

  {-# INLINE addBaker #-}
  addBaker binfo = do
    s <- use schedulerBlockState
    (bid, s') <- lift (bsoAddBaker s binfo)
    schedulerBlockState .= s'
    return bid

  {-# INLINE removeBaker #-}
  removeBaker bid = do
    s <- use schedulerBlockState
    (_, s') <- lift (bsoRemoveBaker s bid)
    schedulerBlockState .= s'

  {-# INLINE updateBakerSignKey #-}
  updateBakerSignKey bid signKey = do
    s <- use schedulerBlockState
    (r, s') <- lift (bsoUpdateBaker s (emptyBakerUpdate bid & buSignKey ?~ signKey))
    schedulerBlockState .= s'
    return r

  {-# INLINE updateBakerAccount #-}
  updateBakerAccount bid bacc = do
    s <- use schedulerBlockState
    (_, s') <- lift (bsoUpdateBaker s (emptyBakerUpdate bid & buAccount ?~ bacc))
    -- updating the account cannot fail, so we ignore the return value.
    schedulerBlockState .= s'

  {-# INLINE delegateStake #-}
  delegateStake acc bid = do
    s <- use schedulerBlockState
    (r, s') <- lift (bsoDelegateStake s acc bid)
    schedulerBlockState .= s'
    return r

  {-# INLINE getIPInfo #-}
  getIPInfo ipId = do
    s <- use schedulerBlockState
    lift (bsoGetIdentityProvider s ipId)

  {-# INLINE getCrypoParams #-}
  getCrypoParams = lift . bsoGetCryptoParams =<< use schedulerBlockState

-- Pure block state scheduler state
type PBSSS = NoLogSchedulerState (PureBlockStateMonad Identity)
type RWSTBS m a = RWST ContextState () PBSSS m a

-- |Basic implementation of the scheduler that does no transaction logging.
newtype SchedulerImplementation a = SchedulerImplementation { _runScheduler :: RWSTBS (PureBlockStateMonad Identity) a }
    deriving (Functor, Applicative, Monad, MonadReader ContextState, MonadState PBSSS)
    deriving (StaticEnvironmentMonad Core.UA)
      via (BSOMonadWrapper ContextState () PBSSS (MGSTrans (RWST ContextState () PBSSS) (PureBlockStateMonad Identity)))

deriving via (BSOMonadWrapper ContextState () PBSSS (MGSTrans (RWST ContextState () PBSSS) (PureBlockStateMonad Identity))) instance
  SchedulerMonad SchedulerImplementation

instance ATITypes SchedulerImplementation where
  type ATIStorage SchedulerImplementation = ()

runSI :: SchedulerImplementation a -> SpecialBetaAccounts -> ChainMetadata -> Energy -> BlockState -> (a, PBSSS)
runSI sc gd cd energy gs =
  let (a, s, !_) =
        runIdentity $
        runPureBlockStateMonad $
        runRWST (_runScheduler sc) (ContextState gd cd energy) (NoLogSchedulerState gs 0)
  in (a, s)

execSI :: SchedulerImplementation a -> SpecialBetaAccounts -> ChainMetadata -> Energy -> BlockState -> PBSSS
execSI sc gd cd energy gs =
  fst (runIdentity $
       runPureBlockStateMonad $
       execRWST (_runScheduler sc) (ContextState gd cd energy) (NoLogSchedulerState gs 0))

evalSI :: SchedulerImplementation a -> SpecialBetaAccounts -> ChainMetadata -> Energy -> BlockState -> a
evalSI sc gd cd energy gs =
  fst (runIdentity $
       runPureBlockStateMonad $
       evalRWST (_runScheduler sc) (ContextState gd cd energy) (NoLogSchedulerState gs 0))
