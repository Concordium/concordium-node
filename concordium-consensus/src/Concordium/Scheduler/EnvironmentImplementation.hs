{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
module Concordium.Scheduler.EnvironmentImplementation where

import Concordium.Scheduler.Environment

import qualified Data.Kind as DK
import Data.HashMap.Strict as Map
import qualified Data.Map.Strict as OrdMap
import Data.HashSet as Set
import Data.Functor.Identity

import Lens.Micro.Platform

import Control.Monad.Reader
import Control.Monad.Writer.Strict(MonadWriter, tell, censor, listen, pass)
import Control.Monad.Trans.RWS.Strict hiding (ask, get, put, tell, censor, listen)
import Control.Monad.State.Class

import Concordium.Scheduler.Types
import Concordium.Logger
import Concordium.GlobalState.Account
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Basic.BlockState (PureBlockStateMonad(..), BlockState)
import Concordium.GlobalState.TreeState
    ( BlockStateTypes(UpdatableBlockState), MGSTrans(..) )
import qualified Concordium.GlobalState.Types as GS

-- |Chain metadata together with the maximum allowed block energy.
data ContextState = ContextState{
  _chainMetadata :: !ChainMetadata,
  _maxBlockEnergy :: !Energy,
  _accountCreationLimit :: !CredentialsPerBlockLimit
  }

makeLenses ''ContextState

-- Doing it manually because otherwise the generated class definition
-- seems to be wrong (m has kind *).
class CanExtend (TransactionLog a) => HasSchedulerState a where
  type SS a
  type TransactionLog a

  schedulerBlockState :: Lens' a (SS a)
  schedulerEnergyUsed :: Lens' a Energy
  -- |The running total of execution costs for this block.
  schedulerExecutionCosts :: Lens' a Amount
  schedulerTransactionLog :: Lens' a (TransactionLog a)
  nextIndex :: Lens' a TransactionIndex

data NoLogSchedulerState (m :: DK.Type -> DK.Type)= NoLogSchedulerState {
  _ssBlockState :: !(UpdatableBlockState m),
  _ssSchedulerEnergyUsed :: !Energy,
  _ssSchedulerExecutionCosts :: !Amount,
  _ssNextIndex :: !TransactionIndex
  }

mkInitialSS :: UpdatableBlockState m -> NoLogSchedulerState m
mkInitialSS _ssBlockState = NoLogSchedulerState{
    _ssSchedulerEnergyUsed = 0,
    _ssSchedulerExecutionCosts = 0,
    _ssNextIndex = 0,
    ..
  }

makeLenses ''NoLogSchedulerState

instance HasSchedulerState (NoLogSchedulerState m) where
  type SS (NoLogSchedulerState m) = UpdatableBlockState m
  type TransactionLog (NoLogSchedulerState m) = ()
  schedulerBlockState = ssBlockState
  schedulerEnergyUsed = ssSchedulerEnergyUsed
  schedulerExecutionCosts = ssSchedulerExecutionCosts
  nextIndex = ssNextIndex
  schedulerTransactionLog f s = s <$ f ()

newtype BSOMonadWrapper r w state m a = BSOMonadWrapper (m a)
    deriving (Functor,
              Applicative,
              Monad,
              MonadReader r,
              MonadState state,
              MonadWriter w,
              MonadLogger)

instance MonadTrans (BSOMonadWrapper r w s) where
    {-# INLINE lift #-}
    lift = BSOMonadWrapper

instance (ATITypes m, ATIStorage m ~ w) => ATITypes (BSOMonadWrapper r w s m) where
  type ATIStorage (BSOMonadWrapper r w s m) = ATIStorage m

instance (MonadReader ContextState m,
          SS state ~ UpdatableBlockState m,
          HasSchedulerState state,
          MonadState state m,
          BlockStateOperations m,
          Footprint (ATIStorage m) ~ w,
          MonadWriter w m
         )
         => StaticInformation (BSOMonadWrapper ContextState w state m) where

  {-# INLINE getMaxBlockEnergy #-}
  getMaxBlockEnergy = view maxBlockEnergy

  {-# INLINE getChainMetadata #-}
  getChainMetadata = view chainMetadata

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces mref = do
    s <- use schedulerBlockState
    lift (bsoGetModule s mref)

  {-# INLINE getAccountCreationLimit #-}
  getAccountCreationLimit = view accountCreationLimit

instance (MonadReader ContextState m,
          SS state ~ UpdatableBlockState m,
          HasSchedulerState state,
          MonadState state m,
          BS.BlockStateOperations m,
          CanRecordFootprint w,
          CanExtend (ATIStorage m),
          Footprint (ATIStorage m) ~ w,
          MonadWriter w m,
          MonadLogger m
         )
         => SchedulerMonad (BSOMonadWrapper ContextState w state m) where

  {-# INLINE tlNotifyAccountEffect #-}
  tlNotifyAccountEffect items summary = do
    traverseAccountOutcomes items $ \addr -> do
      schedulerTransactionLog %= extendAccountRecord addr summary
    traverseContractOutcomes items $ \addr -> do
      schedulerTransactionLog %= extendContractRecord addr summary

  {-# INLINE markEnergyUsed #-}
  markEnergyUsed energy = schedulerEnergyUsed += energy

  {-# INLINE getUsedEnergy #-}
  getUsedEnergy = use schedulerEnergyUsed

  {-# INLINE bumpTransactionIndex #-}
  bumpTransactionIndex = nextIndex <<%= (+1)

  {-# INLINE getContractInstance #-}
  getContractInstance addr = lift . flip bsoGetInstance addr =<< use schedulerBlockState

  {-# INLINE getAccount #-}
  getAccount !addr = lift . flip bsoGetAccount addr =<< use schedulerBlockState

  {-# INLINE getAccountIndex #-}
  getAccountIndex addr = lift  . flip bsoGetAccountIndex addr =<< use schedulerBlockState

  {-# INLINE putNewInstance #-}
  putNewInstance !mkInstance = do
    (caddr, s') <- lift . flip bsoPutNewInstance mkInstance =<< use schedulerBlockState
    schedulerBlockState .= s'
    return caddr

  {-# INLINE createAccount #-}
  createAccount cparams akeys addr credential = do
    s <- use schedulerBlockState
    (res, s') <- lift (bsoCreateAccount s cparams akeys addr credential)
    schedulerBlockState .= s'
    return res

  {-# INLINE accountRegIdExists #-}
  accountRegIdExists !regid =
    lift . flip bsoRegIdExists regid =<< use schedulerBlockState

  {-# INLINE commitModule #-}
  commitModule !iface = do
    (res, s') <- lift . (\s -> bsoPutNewModule s iface) =<< use schedulerBlockState
    schedulerBlockState .= s'
    return res

  {-# INLINE increaseAccountNonce #-}
  increaseAccountNonce acc = do
    s <- use schedulerBlockState
    nonce <- getAccountNonce acc
    addr <- getAccountAddress acc
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auNonce ?~ (nonce + 1)))
    schedulerBlockState .= s'

  {-# INLINE addAccountCredential #-}
  addAccountCredential !acc !cdi = do
    s <- use schedulerBlockState
    addr <- getAccountAddress acc
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate addr & auCredential ?~ cdi))
    schedulerBlockState .= s'

  {-# INLINE commitChanges #-}
  commitChanges !cs = do
    s <- use schedulerBlockState
    -- ASSUMPTION: the property which should hold at this point is that any
    -- changed instance must exist in the global state and moreover all instances
    -- are distinct by the virtue of a HashMap being a function
    s' <- lift (foldM (\s' (addr, (amnt, val)) -> do
                         tell (logContract addr)
                         bsoModifyInstance s' addr amnt val)
                      s
                      (Map.toList (cs ^. instanceUpdates)))
    -- log the initialized instances too
    lift (mapM_ (tell . logContract)
                      (Set.toList (cs ^. instanceInits)))
    -- Notify account transfers, but also log the affected accounts.
    s'' <- lift (foldM (\curState accUpdate -> do
                           tell (logAccount (accUpdate ^. auAddress))
                           bsoModifyAccount curState accUpdate
                       )
                  s'
                  (cs ^. accountUpdates))
    s''' <- lift (bsoAddReleaseSchedule s'' (OrdMap.toList $ cs ^. addedReleaseSchedules))
    schedulerBlockState .= s'''

  -- Observe a single transaction footprint.
  {-# INLINE observeTransactionFootprint #-}
  observeTransactionFootprint c =
    censor (const mempty) (listen c)

  {-# INLINE energyToGtu #-}
  energyToGtu v = do
    s <- use schedulerBlockState
    rate <- lift (bsoGetEnergyRate s)
    return (computeCost rate v)

  {-# INLINE notifyExecutionCost #-}
  notifyExecutionCost !amnt = schedulerExecutionCosts += amnt

  {-# INLINE notifyEncryptedBalanceChange #-}
  notifyEncryptedBalanceChange !amntDiff = do
    s <- use schedulerBlockState
    s' <- lift (bsoNotifyEncryptedBalanceChange s amntDiff)
    schedulerBlockState .= s'

  {-# INLINE addBaker #-}
  addBaker acct badd = do
    s <- use schedulerBlockState
    (ret, s') <- lift (bsoAddBaker s acct badd)
    schedulerBlockState .= s'
    return ret

  {-# INLINE removeBaker #-}
  removeBaker badd = do
    s <- use schedulerBlockState
    (ret, s') <- lift (bsoRemoveBaker s badd)
    schedulerBlockState .= s'
    return ret

  {-# INLINE updateBakerKeys #-}
  updateBakerKeys badd keyUpd = do
    s <- use schedulerBlockState
    (r, s') <- lift (bsoUpdateBakerKeys s badd keyUpd)
    schedulerBlockState .= s'
    return r

  {-# INLINE updateBakerStake #-}
  updateBakerStake badd bsu = do
    s <- use schedulerBlockState
    (r, s') <- lift (bsoUpdateBakerStake s badd bsu)
    schedulerBlockState .= s'
    return r

  {-# INLINE updateBakerRestakeEarnings #-}
  updateBakerRestakeEarnings badd bre = do
    s <- use schedulerBlockState
    (r, s') <- lift (bsoUpdateBakerRestakeEarnings s badd bre)
    schedulerBlockState .= s'
    return r

  {-# INLINE updateAccountKeys #-}
  updateAccountKeys accAddr newKeys = do
    s <- use schedulerBlockState
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate accAddr & auKeysUpdate ?~ SetKeys newKeys))
    schedulerBlockState .= s'

  {-# INLINE addAccountKeys #-}
  addAccountKeys accAddr newKeys threshold = do
    s <- use schedulerBlockState
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate accAddr & auKeysUpdate ?~ SetKeys newKeys
                                                               & auSignThreshold .~ threshold))
    schedulerBlockState .= s'

  {-# INLINE removeAccountKeys #-}
  removeAccountKeys accAddr keyIdxs threshold = do
    s <- use schedulerBlockState
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate accAddr & auKeysUpdate ?~ RemoveKeys keyIdxs
                                                               & auSignThreshold .~ threshold))
    schedulerBlockState .= s'

  {-# INLINE getIPInfo #-}
  getIPInfo ipId = do
    s <- use schedulerBlockState
    lift (bsoGetIdentityProvider s ipId)

  {-# INLINE getArInfos #-}
  getArInfos arIds = do
    s <- use schedulerBlockState
    lift (bsoGetAnonymityRevokers s arIds)

  {-# INLINE getCryptoParams #-}
  getCryptoParams = lift . bsoGetCryptoParams =<< use schedulerBlockState

  {-# INLINE getUpdateAuthorizations #-}
  getUpdateAuthorizations = lift . bsoGetCurrentAuthorizations =<< use schedulerBlockState

  {-# INLINE getNextUpdateSequenceNumber #-}
  getNextUpdateSequenceNumber uty = do
    s <- use schedulerBlockState
    lift (bsoGetNextUpdateSequenceNumber s uty)

  {-# INLINE enqueueUpdate #-}
  enqueueUpdate tt p = do
    s <- use schedulerBlockState
    s' <- lift (bsoEnqueueUpdate s tt p)
    schedulerBlockState .= s'

deriving instance GS.BlockStateTypes (BSOMonadWrapper r w state m)

deriving instance AccountOperations m => AccountOperations (BSOMonadWrapper r w state m)

-- Pure block state scheduler state
type PBSSS = NoLogSchedulerState (PureBlockStateMonad Identity)
-- newtype wrapper to forget the automatic writer instance so we can repurpose it for logging.
newtype RWSTBS m a = RWSTBS {_runRWSTBS :: RWST ContextState [(LogSource, LogLevel, String)] PBSSS m a}
  deriving (Functor, Applicative, Monad, MonadReader ContextState, MonadState PBSSS, MonadTrans)

instance Monad m => MonadWriter () (RWSTBS m) where
  tell _ = return ()
  listen = fmap (,())
  pass = fmap fst

-- |Basic implementation of the scheduler that does no transaction logging.
newtype SchedulerImplementation a = SchedulerImplementation { _runScheduler :: RWSTBS (PureBlockStateMonad Identity) a }
    deriving (Functor, Applicative, Monad, MonadReader ContextState, MonadState PBSSS)
    deriving (StaticInformation, AccountOperations, MonadLogger)
      via (BSOMonadWrapper ContextState () PBSSS (MGSTrans RWSTBS (PureBlockStateMonad Identity)))

instance Monad m => MonadLogger (RWSTBS m) where
  logEvent source level event = RWSTBS (RWST (\_ s -> return ((), s, [(source, level, event)])))

deriving via (PureBlockStateMonad Identity) instance GS.BlockStateTypes SchedulerImplementation

deriving via (BSOMonadWrapper ContextState () PBSSS (MGSTrans RWSTBS (PureBlockStateMonad Identity))) instance
  SchedulerMonad SchedulerImplementation

instance ATITypes SchedulerImplementation where
  type ATIStorage SchedulerImplementation = ()

runSI :: SchedulerImplementation a -> ChainMetadata -> Energy -> CredentialsPerBlockLimit -> BlockState -> (a, PBSSS)
runSI sc cd energy maxCreds gs =
  let (a, s, !_) =
        runIdentity $
        runPureBlockStateMonad $
        runRWST (_runRWSTBS . _runScheduler $ sc) (ContextState cd energy maxCreds) (mkInitialSS gs)
  in (a, s)

-- |Same as the previous method, but retain the logs of the run.
runSIWithLogs :: SchedulerImplementation a -> ChainMetadata -> Energy -> CredentialsPerBlockLimit -> BlockState -> (a, PBSSS, [(LogSource, LogLevel, String)])
runSIWithLogs sc cd energy maxCreds gs =
  runIdentity $
  runPureBlockStateMonad $
  runRWST (_runRWSTBS . _runScheduler $ sc) (ContextState cd energy maxCreds) (mkInitialSS gs)


execSI :: SchedulerImplementation a -> ChainMetadata -> Energy -> CredentialsPerBlockLimit -> BlockState -> PBSSS
execSI sc cd energy maxCreds gs =
  fst (runIdentity $
       runPureBlockStateMonad $
       execRWST (_runRWSTBS . _runScheduler $ sc) (ContextState cd energy maxCreds) (mkInitialSS gs))

evalSI :: SchedulerImplementation a -> ChainMetadata -> Energy -> CredentialsPerBlockLimit -> BlockState -> a
evalSI sc cd energy maxCreds gs =
  fst (runIdentity $
       runPureBlockStateMonad $
       evalRWST (_runRWSTBS . _runScheduler $ sc) (ContextState cd energy maxCreds) (mkInitialSS gs))
