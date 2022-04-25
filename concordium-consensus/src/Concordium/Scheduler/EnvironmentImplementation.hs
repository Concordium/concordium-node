{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- FIXME: This is to suppress compiler warnings for derived instances of SchedulerMonad.
-- This may be fixed in GHC 9.0.1.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Concordium.Scheduler.EnvironmentImplementation where

import Concordium.Scheduler.Environment

import qualified Data.Kind as DK
import Data.HashMap.Strict as Map
import qualified Data.Map.Strict as OrdMap
import Data.HashSet as Set
import Data.Functor.Identity

import Concordium.TimeMonad
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
    ( BlockStateTypes(UpdatableBlockState), MGSTrans(..))
import qualified Concordium.GlobalState.Types as GS
import qualified Concordium.TransactionVerification as TVer

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

data NoLogSchedulerState (m :: DK.Type -> DK.Type) = NoLogSchedulerState {
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

deriving instance GS.MonadProtocolVersion m => GS.MonadProtocolVersion (BSOMonadWrapper r w s m)

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

  {-# INLINE getContractInstance #-}
  getContractInstance addr = lift . flip bsoGetInstance addr =<< use schedulerBlockState

  {-# INLINE getStateAccount #-}
  getStateAccount !addr = lift . flip bsoGetAccount addr =<< use schedulerBlockState

instance (SS state ~ UpdatableBlockState m,
          HasSchedulerState state,
          MonadState state m,
          BlockStateOperations m,
          MonadReader ContextState m,
          GS.MonadProtocolVersion m
         )
         => TVer.TransactionVerifier (BSOMonadWrapper ContextState w state m) where
  {-# INLINE registrationIdExists #-}
  registrationIdExists !regid =
    lift . flip bsoRegIdExists regid =<< use schedulerBlockState
  {-# INLINE getIdentityProvider #-}
  getIdentityProvider !ipId = do
    s <- use schedulerBlockState
    lift (bsoGetIdentityProvider s ipId)
  {-# INLINE getAnonymityRevokers #-}
  getAnonymityRevokers !arIds = do
    s <- use schedulerBlockState
    lift (bsoGetAnonymityRevokers s arIds)
  {-# INLINE getCryptographicParameters #-}
  getCryptographicParameters = lift . bsoGetCryptoParams =<< use schedulerBlockState
  {-# INLINE getAccount #-}
  getAccount !aaddr = do
    s <- use schedulerBlockState
    lift (fmap snd <$> bsoGetAccount s aaddr)
  {-# INLINE getNextUpdateSequenceNumber #-}
  getNextUpdateSequenceNumber uType = lift . flip bsoGetNextUpdateSequenceNumber uType =<< use schedulerBlockState
  {-# INLINE getUpdateKeysCollection #-}
  getUpdateKeysCollection = lift . bsoGetUpdateKeyCollection =<< use schedulerBlockState
  {-# INLINE getAccountAvailableAmount #-}
  getAccountAvailableAmount = lift . getAccountAvailableAmount
  {-# INLINE getNextAccountNonce #-}
  getNextAccountNonce = lift . getAccountNonce
  {-# INLINE getAccountVerificationKeys #-}
  getAccountVerificationKeys = lift . getAccountVerificationKeys
  {-# INLINE energyToCcd #-}
  energyToCcd v =  do
    s <- use schedulerBlockState
    rate <- lift (bsoGetEnergyRate s)
    return (computeCost rate v)
  {-# INLINE getMaxBlockEnergy #-}
  getMaxBlockEnergy = do
    ctx <- ask
    let maxEnergy = ctx ^. maxBlockEnergy
    return maxEnergy
  {-# INLINE checkExactNonce #-}
  checkExactNonce = pure True
  
instance (MonadReader ContextState m,
          SS state ~ UpdatableBlockState m,
          HasSchedulerState state,
          MonadState state m,
          BlockStateOperations m,
          CanRecordFootprint w,
          CanExtend (ATIStorage m),
          Footprint (ATIStorage m) ~ w,
          MonadWriter w m,
          MonadLogger m,
          GS.MonadProtocolVersion m
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

  {-# INLINE getAccountIndex #-}
  getAccountIndex addr = lift  . flip bsoGetAccountIndex addr =<< use schedulerBlockState

  {-# INLINE putNewInstance #-}
  putNewInstance !mkInstance = do
    (caddr, s') <- lift . flip bsoPutNewInstance mkInstance =<< use schedulerBlockState
    schedulerBlockState .= s'
    return caddr

  {-# INLINE createAccount #-}
  createAccount cparams addr credential = do
    s <- use schedulerBlockState
    (res, s') <- lift (bsoCreateAccount s cparams addr credential)
    schedulerBlockState .= s'
    return res

  {-# INLINE addressWouldClash #-}
  addressWouldClash !addr =
    lift . flip bsoAddressWouldClash addr =<< use schedulerBlockState

  {-# INLINE commitModule #-}
  commitModule !iface = do
    (res, s') <- lift . (\s -> bsoPutNewModule s iface) =<< use schedulerBlockState
    schedulerBlockState .= s'
    return res

  {-# INLINE increaseAccountNonce #-}
  increaseAccountNonce (ai, acc) = do
    s <- use schedulerBlockState
    nonce <- getAccountNonce acc
    addr <- getAccountCanonicalAddress acc
    s' <- lift (bsoModifyAccount s (emptyAccountUpdate ai addr & auNonce ?~ (nonce + 1)))
    schedulerBlockState .= s'

  {-# INLINE updateAccountCredentials #-}
  updateAccountCredentials !ai !idcs !creds !threshold  = do
    s <- use schedulerBlockState
    s' <- lift (bsoUpdateAccountCredentials s ai idcs creds threshold)
    schedulerBlockState .= s'

  {-# INLINE commitChanges #-}
  commitChanges !cs = do
    s <- use schedulerBlockState
    -- ASSUMPTION: the property which should hold at this point is that any
    -- changed instance must exist in the global state and moreover all instances
    -- are distinct by the virtue of a HashMap being a function
    s1 <- lift (foldM (\s' (addr, (_, amnt, val)) -> do
                          tell (logContract addr)
                          bsoModifyInstance s' addr amnt val)
                      s
                      (Map.toList (cs ^. instanceV0Updates)))
    -- since V0 and V1 instances are disjoint, the order in which we do updates does not matter.
    s2 <- lift (foldM (\s' (addr, (_, amnt, val)) -> do
                          tell (logContract addr)
                          bsoModifyInstance s' addr amnt val)
                      s1
                      (Map.toList (cs ^. instanceV1Updates)))
    -- log the initialized instances too
    lift (mapM_ (tell . logContract)
                      (Set.toList (cs ^. instanceInits)))
    -- Notify account transfers, but also log the affected accounts. Since the
    -- changeset is meant to contain the canonical address of the account we
    -- always log affected accounts by the canonical address.
    s3 <- lift (foldM (\curState accUpdate -> do
                           tell (logAccount (accUpdate ^. auAddress))
                           bsoModifyAccount curState accUpdate
                       )
                  s2
                  (cs ^. accountUpdates))
    s4 <- lift (bsoAddReleaseSchedule s3 (OrdMap.toList $ cs ^. addedReleaseSchedules))
    schedulerBlockState .= s4

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
  addBaker ai badd = do
    s <- use schedulerBlockState
    (ret, s') <- lift (bsoAddBaker s ai badd)
    schedulerBlockState .= s'
    return ret

  {-# INLINE configureBaker #-}
  configureBaker ai bconfig = do
    s <- use schedulerBlockState
    (ret, s') <- lift (bsoConfigureBaker s ai bconfig)
    schedulerBlockState .= s'
    return ret

  {-# INLINE configureDelegation #-}
  configureDelegation ai dconfig = do
    s <- use schedulerBlockState
    (ret, s') <- lift (bsoConfigureDelegation s ai dconfig)
    schedulerBlockState .= s'
    return ret

  {-# INLINE removeBaker #-}
  removeBaker ai = do
    s <- use schedulerBlockState
    (ret, s') <- lift (bsoRemoveBaker s ai)
    schedulerBlockState .= s'
    return ret

  {-# INLINE updateBakerKeys #-}
  updateBakerKeys ai keyUpd = do
    s <- use schedulerBlockState
    (r, s') <- lift (bsoUpdateBakerKeys s ai keyUpd)
    schedulerBlockState .= s'
    return r

  {-# INLINE updateBakerStake #-}
  updateBakerStake bi bsu = do
    s <- use schedulerBlockState
    (r, s') <- lift (bsoUpdateBakerStake s bi bsu)
    schedulerBlockState .= s'
    return r

  {-# INLINE updateBakerRestakeEarnings #-}
  updateBakerRestakeEarnings bi bre = do
    s <- use schedulerBlockState
    (r, s') <- lift (bsoUpdateBakerRestakeEarnings s bi bre)
    schedulerBlockState .= s'
    return r

  {-# INLINE updateCredentialKeys #-}
  updateCredentialKeys accIndex credIndex newKeys = do
    s <- use schedulerBlockState
    s' <- lift (bsoSetAccountCredentialKeys s accIndex credIndex newKeys)
    schedulerBlockState .= s'

  {-# INLINE getUpdateKeyCollection #-}
  getUpdateKeyCollection = lift . bsoGetUpdateKeyCollection =<< use schedulerBlockState

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
deriving instance ContractStateOperations m => ContractStateOperations (BSOMonadWrapper r w state m)

-- Pure block state scheduler state
type PBSSS pv = NoLogSchedulerState (PureBlockStateMonad pv Identity)
-- newtype wrapper to forget the automatic writer instance so we can repurpose it for logging.
newtype RWSTBS pv m a = RWSTBS {_runRWSTBS :: RWST ContextState [(LogSource, LogLevel, String)] (PBSSS pv) m a}
  deriving (Functor, Applicative, Monad, MonadReader ContextState, MonadState (PBSSS pv), MonadTrans)

instance Monad m => MonadWriter () (RWSTBS pv m) where
  tell _ = return ()
  listen = fmap (,())
  pass = fmap fst

-- |Basic implementation of the scheduler that does no transaction logging.
newtype SchedulerImplementation pv a = SchedulerImplementation { _runScheduler :: RWSTBS pv (PureBlockStateMonad pv Identity) a }
    deriving (Functor, Applicative, Monad, MonadReader ContextState, MonadState (PBSSS pv))
    deriving (StaticInformation, AccountOperations, ContractStateOperations, MonadLogger)
      via (BSOMonadWrapper ContextState () (PBSSS pv) (MGSTrans (RWSTBS pv) (PureBlockStateMonad pv Identity)))

instance IsProtocolVersion pv => GS.MonadProtocolVersion (SchedulerImplementation pv) where
  type MPV (SchedulerImplementation pv) = pv

--Dummy implementation of TimeMonad, for testing. 
instance TimeMonad (SchedulerImplementation pv) where
  currentTime = return $ read "1970-01-01 13:27:13.257285424 UTC"

instance Monad m => MonadLogger (RWSTBS pv m) where
  logEvent source level event = RWSTBS (RWST (\_ s -> return ((), s, [(source, level, event)])))

deriving via (PureBlockStateMonad pv Identity) instance GS.BlockStateTypes (SchedulerImplementation pv)

deriving via (BSOMonadWrapper ContextState () (PBSSS pv) (MGSTrans (RWSTBS pv) (PureBlockStateMonad pv Identity))) instance
  (IsProtocolVersion pv) => SchedulerMonad (SchedulerImplementation pv)
deriving via (BSOMonadWrapper ContextState () (PBSSS pv) (MGSTrans (RWSTBS pv) (PureBlockStateMonad pv Identity))) instance
  (IsProtocolVersion pv) => TVer.TransactionVerifier (SchedulerImplementation pv)

instance ATITypes (SchedulerImplementation pv) where
  type ATIStorage (SchedulerImplementation pv) = ()

runSI :: SchedulerImplementation pv a -> ChainMetadata -> Energy -> CredentialsPerBlockLimit -> BlockState pv -> (a, PBSSS pv)
runSI sc cd energy maxCreds gs =
  let (a, s, !_) =
        runIdentity $
        runPureBlockStateMonad $
        runRWST (_runRWSTBS . _runScheduler $ sc) (ContextState cd energy maxCreds) (mkInitialSS gs)
  in (a, s)

-- |Same as the previous method, but retain the logs of the run.
runSIWithLogs :: SchedulerImplementation pv a -> ChainMetadata -> Energy -> CredentialsPerBlockLimit -> BlockState pv -> (a, PBSSS pv, [(LogSource, LogLevel, String)])
runSIWithLogs sc cd energy maxCreds gs =
  runIdentity $
  runPureBlockStateMonad $
  runRWST (_runRWSTBS . _runScheduler $ sc) (ContextState cd energy maxCreds) (mkInitialSS gs)


execSI :: SchedulerImplementation pv a -> ChainMetadata -> Energy -> CredentialsPerBlockLimit ->  BlockState pv -> PBSSS pv
execSI sc cd energy maxCreds gs =
  fst (runIdentity $
       runPureBlockStateMonad $
       execRWST (_runRWSTBS . _runScheduler $ sc) (ContextState cd energy maxCreds) (mkInitialSS gs))

evalSI :: SchedulerImplementation pv a -> ChainMetadata -> Energy -> CredentialsPerBlockLimit -> BlockState pv -> a
evalSI sc cd energy maxCreds gs =
  fst (runIdentity $
       runPureBlockStateMonad $
       evalRWST (_runRWSTBS . _runScheduler $ sc) (ContextState cd energy maxCreds) (mkInitialSS gs))
