{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- FIXME: This is to suppress compiler warnings for derived instances of SchedulerMonad.
-- This may be fixed in GHC 9.0.1.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Concordium.Scheduler.EnvironmentImplementation where

import Concordium.Scheduler.Environment
import Data.HashMap.Strict as Map
import qualified Data.Kind as DK

import Lens.Micro.Platform

import Control.Monad.Reader
import Control.Monad.State.Class

import Concordium.GlobalState.Account
import Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.TreeState (
    BlockStateTypes (UpdatableBlockState),
 )
import qualified Concordium.GlobalState.Types as GS
import Concordium.Logger
import Concordium.Scheduler.Types
import qualified Concordium.TransactionVerification as TVer

-- |Chain metadata together with the maximum allowed block energy.
data ContextState = ContextState
    { _chainMetadata :: !ChainMetadata,
      _maxBlockEnergy :: !Energy,
      _accountCreationLimit :: !CredentialsPerBlockLimit
    }

makeLenses ''ContextState

-- Doing it manually because otherwise the generated class definition
-- seems to be wrong (m has kind *).
class HasSchedulerState a where
    type SS a

    schedulerBlockState :: Lens' a (SS a)
    schedulerEnergyUsed :: Lens' a Energy

    -- |The running total of execution costs for this block.
    schedulerExecutionCosts :: Lens' a Amount

    nextIndex :: Lens' a TransactionIndex

newtype BSOMonadWrapper (r :: DK.Type) (state :: DK.Type) (m :: DK.Type -> DK.Type) (a :: DK.Type) = BSOMonadWrapper (m a)
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadReader r,
          MonadState state,
          MonadLogger
        )

deriving instance GS.MonadProtocolVersion m => GS.MonadProtocolVersion (BSOMonadWrapper r s m)

instance MonadTrans (BSOMonadWrapper r s) where
    {-# INLINE lift #-}
    lift = BSOMonadWrapper

instance
    ( MonadReader ContextState m,
      SS state ~ UpdatableBlockState m,
      HasSchedulerState state,
      MonadState state m,
      BlockStateOperations m
    ) =>
    StaticInformation (BSOMonadWrapper ContextState state m)
    where
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

    {-# INLINE getExchangeRates #-}
    getExchangeRates = lift . bsoGetExchangeRates =<< use schedulerBlockState

instance
    ( SS state ~ UpdatableBlockState m,
      HasSchedulerState state,
      MonadState state m,
      BlockStateOperations m,
      MonadReader ContextState m,
      GS.MonadProtocolVersion m
    ) =>
    TVer.TransactionVerifier (BSOMonadWrapper ContextState state m)
    where
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
    energyToCcd v = do
        s <- use schedulerBlockState
        rate <- lift $ _erEnergyRate <$> bsoGetExchangeRates s
        return (computeCost rate v)
    {-# INLINE getMaxBlockEnergy #-}
    getMaxBlockEnergy = do
        ctx <- ask
        let maxEnergy = ctx ^. maxBlockEnergy
        return maxEnergy
    {-# INLINE checkExactNonce #-}
    checkExactNonce = pure True

instance
    ( MonadReader ContextState m,
      SS state ~ UpdatableBlockState m,
      HasSchedulerState state,
      MonadState state m,
      BlockStateOperations m,
      MonadLogger m,
      GS.MonadProtocolVersion m
    ) =>
    SchedulerMonad (BSOMonadWrapper ContextState state m)
    where
    {-# INLINE markEnergyUsed #-}
    markEnergyUsed energy = schedulerEnergyUsed += energy

    {-# INLINE getUsedEnergy #-}
    getUsedEnergy = use schedulerEnergyUsed

    {-# INLINE bumpTransactionIndex #-}
    bumpTransactionIndex = nextIndex <<%= (+ 1)

    {-# INLINE getAccountIndex #-}
    getAccountIndex addr = lift . flip bsoGetAccountIndex addr =<< use schedulerBlockState

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
        s' <- lift (bsoModifyAccount s (emptyAccountUpdate ai & auNonce ?~ (nonce + 1)))
        schedulerBlockState .= s'

    {-# INLINE updateAccountCredentials #-}
    updateAccountCredentials !ai !idcs !creds !threshold = do
        s <- use schedulerBlockState
        s' <- lift (bsoUpdateAccountCredentials s ai idcs creds threshold)
        schedulerBlockState .= s'

    {-# INLINE commitChanges #-}
    commitChanges !cs = do
        s <- use schedulerBlockState
        -- ASSUMPTION: the property which should hold at this point is that any
        -- changed instance must exist in the global state and moreover all instances
        -- are distinct by the virtue of a HashMap being a function
        s1 <-
            lift
                ( foldM
                    ( \s' (addr, (_, amnt, val)) ->
                        bsoModifyInstance s' addr amnt val Nothing
                    )
                    s
                    (Map.toList (cs ^. instanceV0Updates))
                )
        -- since V0 and V1 instances are disjoint, the order in which we do updates does not matter.
        s2 <-
            lift
                ( foldM
                    ( \s' (addr, InstanceV1Update{..}) ->
                        bsoModifyInstance s' addr amountChange newState newInterface
                    )
                    s1
                    (Map.toList (cs ^. instanceV1Updates))
                )
        -- Notify account transfers.
        -- This also updates the release schedule.
        s3 <-
            lift
                ( foldM
                    bsoModifyAccount
                    s2
                    (cs ^. accountUpdates)
                )
        schedulerBlockState .= s3

    {-# INLINE energyToGtu #-}
    energyToGtu v = do
        s <- use schedulerBlockState
        rate <- lift $ _erEnergyRate <$> bsoGetExchangeRates s
        return $! computeCost rate v

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

deriving instance GS.BlockStateTypes (BSOMonadWrapper r state m)

deriving instance AccountOperations m => AccountOperations (BSOMonadWrapper r state m)
deriving instance ContractStateOperations m => ContractStateOperations (BSOMonadWrapper r state m)
deriving instance ModuleQuery m => ModuleQuery (BSOMonadWrapper r state m)
