{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains the implementation for running the scheduler computations.
module Concordium.Scheduler.EnvironmentImplementation where

import Control.Monad.RWS.Strict
import Data.HashMap.Strict as Map
import qualified Data.Kind as DK
import Lens.Micro.Platform

import Concordium.GlobalState.Account
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.TreeState
import Concordium.Logger
import Concordium.Scheduler.Environment
import Concordium.Scheduler.Types
import Concordium.TimeMonad
import qualified Concordium.TransactionVerification as TVer

-- |Context for executing a scheduler computation.
data ContextState = ContextState
    { -- |Chain metadata
      _chainMetadata :: !ChainMetadata,
      -- |Maximum allowed block energy.
      _maxBlockEnergy :: !Energy,
      -- |Maximum number of accounts to be created in the same block.
      _accountCreationLimit :: !CredentialsPerBlockLimit
    }

makeLenses ''ContextState

-- |State accumulated during execution of a scheduler computation.
data SchedulerState (m :: DK.Type -> DK.Type) = SchedulerState
    { -- | Current block state.
      _ssBlockState :: !(UpdatableBlockState m),
      -- | Energy used so far.
      _ssEnergyUsed :: !Energy,
      -- | The total execution costs so far.
      _ssExecutionCosts :: !Amount,
      -- | The next available transaction index.
      _ssNextIndex :: !TransactionIndex
    }

makeLenses ''SchedulerState

-- |Create an initial state for running a scheduler computation.
makeInitialSchedulerState :: UpdatableBlockState m -> SchedulerState m
makeInitialSchedulerState _ssBlockState =
    SchedulerState
        { _ssEnergyUsed = 0,
          _ssExecutionCosts = 0,
          _ssNextIndex = 0,
          ..
        }

-- | Alias for the internal type used in @SchedulerT@.
type InternalSchedulerT m = RWST ContextState () (SchedulerState m)

-- |Scheduler monad transformer. Extends a monad with the ability to execute scheduler computations.
-- Use @runSchedulerT@ to run the computation.
newtype SchedulerT (m :: DK.Type -> DK.Type) (a :: DK.Type) = SchedulerT
    { _runSchedulerT :: InternalSchedulerT m m a
    }
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadState (SchedulerState m),
          MonadReader ContextState,
          MonadLogger,
          TimeMonad
        )

instance MonadTrans SchedulerT where
    {-# INLINE lift #-}
    lift = SchedulerT . lift

deriving via
    (MGSTrans (InternalSchedulerT m) m)
    instance
        BlockStateTypes (SchedulerT m)

instance (BS.BlockStateOperations m) => StaticInformation (SchedulerT m) where
    {-# INLINE getMaxBlockEnergy #-}
    getMaxBlockEnergy = view maxBlockEnergy

    {-# INLINE getChainMetadata #-}
    getChainMetadata = view chainMetadata

    {-# INLINE getModuleInterfaces #-}
    getModuleInterfaces mref = do
        s <- use ssBlockState
        lift (BS.bsoGetModule s mref)

    {-# INLINE getAccountCreationLimit #-}
    getAccountCreationLimit = view accountCreationLimit

    {-# INLINE getContractInstance #-}
    getContractInstance addr = lift . flip BS.bsoGetInstance addr =<< use ssBlockState

    {-# INLINE getStateAccount #-}
    getStateAccount !addr = lift . flip BS.bsoGetAccount addr =<< use ssBlockState

    {-# INLINE getExchangeRates #-}
    getExchangeRates = lift . BS.bsoGetExchangeRates =<< use ssBlockState

deriving via
    (MGSTrans (InternalSchedulerT m) m)
    instance
        (MonadProtocolVersion m) => MonadProtocolVersion (SchedulerT m)

deriving via
    (MGSTrans (InternalSchedulerT m) m)
    instance
        (BS.AccountOperations m) => BS.AccountOperations (SchedulerT m)

deriving via
    (MGSTrans (InternalSchedulerT m) m)
    instance
        (BS.ContractStateOperations m) => BS.ContractStateOperations (SchedulerT m)

deriving via
    (MGSTrans (InternalSchedulerT m) m)
    instance
        (BS.ModuleQuery m) => BS.ModuleQuery (SchedulerT m)

instance
    (BS.BlockStateOperations m, MonadProtocolVersion m) =>
    TVer.TransactionVerifier (SchedulerT m)
    where
    {-# INLINE registrationIdExists #-}
    registrationIdExists !regid =
        lift . flip BS.bsoRegIdExists regid =<< use ssBlockState
    {-# INLINE getIdentityProvider #-}
    getIdentityProvider !ipId = do
        s <- use ssBlockState
        lift (BS.bsoGetIdentityProvider s ipId)
    {-# INLINE getAnonymityRevokers #-}
    getAnonymityRevokers !arIds = do
        s <- use ssBlockState
        lift (BS.bsoGetAnonymityRevokers s arIds)
    {-# INLINE getCryptographicParameters #-}
    getCryptographicParameters = lift . BS.bsoGetCryptoParams =<< use ssBlockState
    {-# INLINE getAccount #-}
    getAccount !aaddr = do
        s <- use ssBlockState
        lift (fmap snd <$> BS.bsoGetAccount s aaddr)
    {-# INLINE getNextUpdateSequenceNumber #-}
    getNextUpdateSequenceNumber uType = lift . flip BS.bsoGetNextUpdateSequenceNumber uType =<< use ssBlockState
    {-# INLINE getUpdateKeysCollection #-}
    getUpdateKeysCollection = lift . BS.bsoGetUpdateKeyCollection =<< use ssBlockState
    {-# INLINE getAccountAvailableAmount #-}
    getAccountAvailableAmount = lift . BS.getAccountAvailableAmount
    {-# INLINE getNextAccountNonce #-}
    getNextAccountNonce = lift . BS.getAccountNonce
    {-# INLINE getAccountVerificationKeys #-}
    getAccountVerificationKeys = lift . BS.getAccountVerificationKeys
    {-# INLINE energyToCcd #-}
    energyToCcd v = do
        s <- use ssBlockState
        rate <- lift $ _erEnergyRate <$> BS.bsoGetExchangeRates s
        return (computeCost rate v)
    {-# INLINE getMaxBlockEnergy #-}
    getMaxBlockEnergy = do
        ctx <- ask
        let maxEnergy = ctx ^. maxBlockEnergy
        return maxEnergy
    {-# INLINE checkExactNonce #-}
    checkExactNonce = pure True

instance
    ( BS.BlockStateOperations m,
      MonadLogger m,
      MonadProtocolVersion m
    ) =>
    SchedulerMonad (SchedulerT m)
    where
    {-# INLINE markEnergyUsed #-}
    markEnergyUsed energy = ssEnergyUsed += energy

    {-# INLINE getUsedEnergy #-}
    getUsedEnergy = use ssEnergyUsed

    {-# INLINE bumpTransactionIndex #-}
    bumpTransactionIndex = ssNextIndex <<%= (+ 1)

    {-# INLINE getAccountIndex #-}
    getAccountIndex addr = lift . flip BS.bsoGetAccountIndex addr =<< use ssBlockState

    {-# INLINE putNewInstance #-}
    putNewInstance !mkInstance = do
        (caddr, s') <- lift . flip BS.bsoPutNewInstance mkInstance =<< use ssBlockState
        ssBlockState .= s'
        return caddr

    {-# INLINE createAccount #-}
    createAccount cparams addr credential = do
        s <- use ssBlockState
        (res, s') <- lift (BS.bsoCreateAccount s cparams addr credential)
        ssBlockState .= s'
        return res

    {-# INLINE addressWouldClash #-}
    addressWouldClash !addr =
        lift . flip BS.bsoAddressWouldClash addr =<< use ssBlockState

    {-# INLINE commitModule #-}
    commitModule !iface = do
        (res, s') <- lift . (\s -> BS.bsoPutNewModule s iface) =<< use ssBlockState
        ssBlockState .= s'
        return res

    {-# INLINE increaseAccountNonce #-}
    increaseAccountNonce (ai, acc) = do
        s <- use ssBlockState
        nonce <- BS.getAccountNonce acc
        s' <- lift (BS.bsoModifyAccount s (emptyAccountUpdate ai & auNonce ?~ (nonce + 1)))
        ssBlockState .= s'

    {-# INLINE updateAccountCredentials #-}
    updateAccountCredentials !ai !idcs !creds !threshold = do
        s <- use ssBlockState
        s' <- lift (BS.bsoUpdateAccountCredentials s ai idcs creds threshold)
        ssBlockState .= s'

    {-# INLINE commitChanges #-}
    commitChanges !cs = do
        s <- use ssBlockState
        -- ASSUMPTION: the property which should hold at this point is that any
        -- changed instance must exist in the global state and moreover all instances
        -- are distinct by the virtue of a HashMap being a function
        s1 <-
            lift
                ( foldM
                    ( \s' (addr, (modIdx, amnt, val)) ->
                        if modIdx /= 0 then BS.bsoModifyInstance s' addr amnt val Nothing else return s'
                    )
                    s
                    (Map.toList (cs ^. instanceV0Updates))
                )
        -- since V0 and V1 instances are disjoint, the order in which we do updates does not matter.
        s2 <-
            lift
                ( foldM
                    ( \s' (addr, InstanceV1Update{..}) ->
                        BS.bsoModifyInstance s' addr amountChange newState newInterface
                    )
                    s1
                    (Map.toList (cs ^. instanceV1Updates))
                )
        -- Notify account transfers.
        -- This also updates the release schedule.
        s3 <-
            lift
                ( foldM
                    BS.bsoModifyAccount
                    s2
                    (cs ^. accountUpdates)
                )
        ssBlockState .= s3

    {-# INLINE energyToGtu #-}
    energyToGtu v = do
        s <- use ssBlockState
        rate <- lift $ _erEnergyRate <$> BS.bsoGetExchangeRates s
        return $! computeCost rate v

    {-# INLINE notifyExecutionCost #-}
    notifyExecutionCost !amnt = ssExecutionCosts += amnt

    {-# INLINE notifyEncryptedBalanceChange #-}
    notifyEncryptedBalanceChange !amntDiff = do
        s <- use ssBlockState
        s' <- lift (BS.bsoNotifyEncryptedBalanceChange s amntDiff)
        ssBlockState .= s'

    {-# INLINE addBaker #-}
    addBaker ai badd = do
        s <- use ssBlockState
        (ret, s') <- lift (BS.bsoAddBaker s ai badd)
        ssBlockState .= s'
        return ret

    {-# INLINE configureBaker #-}
    configureBaker ai bconfig = do
        s <- use ssBlockState
        (ret, s') <- lift (BS.bsoConfigureBaker s ai bconfig)
        ssBlockState .= s'
        return ret

    {-# INLINE configureDelegation #-}
    configureDelegation ai dconfig = do
        s <- use ssBlockState
        (ret, s') <- lift (BS.bsoConfigureDelegation s ai dconfig)
        ssBlockState .= s'
        return ret

    {-# INLINE removeBaker #-}
    removeBaker ai = do
        s <- use ssBlockState
        (ret, s') <- lift (BS.bsoRemoveBaker s ai)
        ssBlockState .= s'
        return ret

    {-# INLINE updateBakerKeys #-}
    updateBakerKeys ai keyUpd = do
        s <- use ssBlockState
        (r, s') <- lift (BS.bsoUpdateBakerKeys s ai keyUpd)
        ssBlockState .= s'
        return r

    {-# INLINE updateBakerStake #-}
    updateBakerStake bi bsu = do
        s <- use ssBlockState
        (r, s') <- lift (BS.bsoUpdateBakerStake s bi bsu)
        ssBlockState .= s'
        return r

    {-# INLINE updateBakerRestakeEarnings #-}
    updateBakerRestakeEarnings bi bre = do
        s <- use ssBlockState
        (r, s') <- lift (BS.bsoUpdateBakerRestakeEarnings s bi bre)
        ssBlockState .= s'
        return r

    {-# INLINE updateCredentialKeys #-}
    updateCredentialKeys accIndex credIndex newKeys = do
        s <- use ssBlockState
        s' <- lift (BS.bsoSetAccountCredentialKeys s accIndex credIndex newKeys)
        ssBlockState .= s'

    {-# INLINE getUpdateKeyCollection #-}
    getUpdateKeyCollection = lift . BS.bsoGetUpdateKeyCollection =<< use ssBlockState

    {-# INLINE getNextUpdateSequenceNumber #-}
    getNextUpdateSequenceNumber uty = do
        s <- use ssBlockState
        lift (BS.bsoGetNextUpdateSequenceNumber s uty)

    {-# INLINE enqueueUpdate #-}
    enqueueUpdate tt p = do
        s <- use ssBlockState
        s' <- lift (BS.bsoEnqueueUpdate s tt p)
        ssBlockState .= s'

-- | Execute the computation using the provided context and scheduler state.
-- The return value is the value produced by the computation and the updated state of the scheduler.
runSchedulerT ::
    Monad m =>
    SchedulerT m a ->
    ContextState ->
    SchedulerState m ->
    m (a, SchedulerState m)
runSchedulerT computation contextState initialState = do
    (value, resultingState, ()) <- runRWST (_runSchedulerT computation) contextState initialState
    return (value, resultingState)
