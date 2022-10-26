{-| This module provides a way to invoke contract entrypoints directly, without
going through a transaction and the scheduler.

The main function is 'invokeContract' which executes the required contract
entrypoint in the desired context. Currently it is only possible to execute a
contract in the state at the end of a given block, this might be relaxed in the
future.

The main use-case of this functionality are "view-functions", which is a way to
inspect the state of a contract off-chain to enable integrations of off-chain
services with smart contracts. 'invokeContract' is exposed via the
InvokeContract API entrypoint.

In the future this should be expanded to allow "dry-run" execution of every
transaction, and to allow execution in a more precise state context. 
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.Scheduler.InvokeContract (invokeContract) where

import Lens.Micro.Platform
import Control.Monad.Reader

import qualified Data.FixedByteString as FBS
import qualified Concordium.ID.Types as ID
import Concordium.Logger
import qualified Concordium.Wasm as Wasm
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.TreeState (MGSTrans(..))
import Concordium.Types.InvokeContract (ContractContext(..), InvokeContractResult(..))

import Concordium.Scheduler.Environment
import Concordium.Scheduler.Types
import Concordium.Scheduler.EnvironmentImplementation (ContextState(..), maxBlockEnergy, chainMetadata, accountCreationLimit)
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import Concordium.Scheduler

-- |A wrapper that provides enough instances so that transactions can be executed. In particular
-- this is aimed towards execution of `handleContractUpdate`.
-- This type is equipped with (in particular)
-- 
-- - BlockStateTypes
-- - AccountOperations
-- - StaticInformation
-- 
-- It is then used together with the LocalT transformer to be able to execute
-- transactions without the context of the scheduler. This is achieved via (the
-- only) instance of TransactionMonad for the LocalT transformer.
newtype InvokeContractMonad m a = InvokeContractMonad {_runInvokeContract :: ReaderT (ContextState, BlockState m) m a}
    deriving (Functor,
              Applicative,
              Monad,
              MonadLogger)

deriving instance (Monad m, r ~ BlockState m) => MonadReader (ContextState, r) (InvokeContractMonad m)

instance MonadTrans InvokeContractMonad where
    {-# INLINE lift #-}
    lift = InvokeContractMonad . lift

deriving via (MGSTrans InvokeContractMonad m) instance MonadProtocolVersion m => MonadProtocolVersion (InvokeContractMonad m)
deriving via (MGSTrans InvokeContractMonad m) instance BlockStateTypes (InvokeContractMonad m)
deriving via (MGSTrans InvokeContractMonad m) instance BS.AccountOperations m => BS.AccountOperations (InvokeContractMonad m)
deriving via (MGSTrans InvokeContractMonad m) instance BS.ContractStateOperations m => BS.ContractStateOperations (InvokeContractMonad m)
deriving via (MGSTrans InvokeContractMonad m) instance BS.ModuleQuery m => BS.ModuleQuery (InvokeContractMonad m)

instance (Monad m, BS.BlockStateQuery m) => StaticInformation (InvokeContractMonad m) where

  {-# INLINE getMaxBlockEnergy #-}
  getMaxBlockEnergy = view (_1 . maxBlockEnergy)

  {-# INLINE getChainMetadata #-}
  getChainMetadata = view (_1 . chainMetadata)

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces mref = do
    s <- view _2
    lift (BS.getModuleInterface s mref)

  {-# INLINE getAccountCreationLimit #-}
  getAccountCreationLimit = view (_1 . accountCreationLimit)

  {-# INLINE getContractInstance #-}
  getContractInstance addr = lift . flip BS.getContractInstance addr =<< view _2

  {-# INLINE getStateAccount #-}
  getStateAccount !addr = lift . flip BS.getAccount addr =<< view _2

  {-# INLINE getEuroPerEnergy #-}
  getEuroPerEnergy = lift . BS.getEuroPerEnergy =<< view _2

  {-# INLINE getAmountPerEuro #-}
  getAmountPerEuro = lift . BS.getAmountPerEuro =<< view _2

-- |Invoke the contract in the given context.
invokeContract :: forall m . (MonadProtocolVersion m, BS.BlockStateQuery m)
    => ContractContext -- ^Context in which to invoke the contract.
    -> ChainMetadata -- ^Chain metadata corresponding to the block state.
    -> BlockState m -- ^The block state in which to invoke the contract.
    -> m InvokeContractResult
invokeContract ContractContext{..} cm bs = do
  -- construct an invoker. Since execution of a contract might depend on this
  -- it is necessary to provide some value. However since many contract entrypoints will
  -- not depend on this it is useful to default to a dummy value if the value is not provided.
  let getInvoker :: InvokeContractMonad m
                   (Either
                     (Maybe RejectReason) -- Invocation failed because the relevant contract/account does not exist.
                     ( -- Check that the requested account or contract has enough balance.
                       Amount -> LocalT r (InvokeContractMonad m) (Address, [ID.RawAccountCredential], Either (Wasm.WasmVersion, ContractAddress) IndexedAccountAddress),
                       AccountAddress, -- Address of the invoker account, or of its owner if the invoker is a contract.
                       AccountIndex -- And its index.
                     ))
      getInvoker =
        case ccInvoker of
          Nothing -> -- if the invoker is not supplied create a dummy one with no credentials
            let zeroAddress = AccountAddress . FBS.pack . replicate 32 $ 0
                maxIndex = maxBound
            in return (Right (const (return (AddressAccount zeroAddress, [], Right (maxIndex, zeroAddress))), zeroAddress, maxIndex))
          -- if the invoker is an address make sure it exists
          Just (AddressAccount accInvoker) -> getStateAccount accInvoker >>= \case
            Nothing -> return (Left (Just (InvalidAccountReference accInvoker)))
            Just acc -> return (Right (checkAndGetBalanceAccountV0 accInvoker acc, accInvoker, fst acc))
          Just (AddressContract contractInvoker) -> getContractInstance contractInvoker >>= \case
            Nothing -> return (Left (Just (InvalidContractAddress contractInvoker)))
            Just (BS.InstanceInfoV0 i) -> do
              let ownerAccountAddress = instanceOwner (BS.iiParameters i)
              getStateAccount ownerAccountAddress >>= \case
                -- the first case should really never happen, since a valid instance should always have a valid account.
                Nothing -> return (Left (Just $ InvalidAccountReference ownerAccountAddress))
                Just acc -> return (Right (checkAndGetBalanceInstanceV0 acc i {BS.iiState = Frozen (BS.iiState i)}, ownerAccountAddress, fst acc))
            Just (BS.InstanceInfoV1 i) -> do
              let ownerAccountAddress = instanceOwner (BS.iiParameters i)
              getStateAccount ownerAccountAddress >>= \case
                -- the first case should really never happen, since a valid instance should always have a valid account.
                Nothing -> return (Left (Just $ InvalidAccountReference ownerAccountAddress))
                Just acc -> return (Right (checkAndGetBalanceInstanceV0 acc i {BS.iiState = Frozen (BS.iiState i)}, ownerAccountAddress, fst acc))
  let runContractComp = 
        getInvoker >>= \case
          Left err -> return (Left err, ccEnergy)
          Right (invoker, addr, ai) -> do
            let comp = do
                  let onV0 i = handleContractUpdateV0 addr i invoker ccAmount ccMethod ccParameter
                  let onV1 i = handleContractUpdateV1 addr i (fmap Right . invoker) ccAmount ccMethod ccParameter
                  istance <- getCurrentContractInstanceTicking ccContract
                  result <- case istance of
                             BS.InstanceInfoV0 i -> Left <$> onV0 i
                             BS.InstanceInfoV1 i -> Right <$> onV1 i
                  -- charge for storage of V1 contracts, so that the cost that
                  -- will be returned matches the cost that will be charged by
                  -- the transaction (minus the signature checking cost)
                  chargeV1Storage
                  return result
            (r, cs) <- runLocalT comp ccAmount ai ccEnergy ccEnergy
            return (r, _energyLeft cs)
      contextState = ContextState{
          _maxBlockEnergy = ccEnergy, _accountCreationLimit = 0, _chainMetadata = cm
          }
  runReaderT (_runInvokeContract runContractComp) (contextState, bs) >>= \case
    -- cannot happen (this would mean out of block energy, and we set block energy no lower than energy),
    -- but this is safe to do and not wrong
    (Left Nothing, re) -> 
      return Failure{rcrReason = OutOfEnergy, rcrReturnValue=Nothing, rcrUsedEnergy = ccEnergy - re}
    -- Contract execution of a V0 contract failed with the given reason.
    (Left (Just rcrReason), re) ->
      return Failure{rcrUsedEnergy = ccEnergy - re,rcrReturnValue=Nothing,..}
    -- Contract execution of a V0 contract succeeded with the given list of events
    (Right (Left rcrEvents), re) ->
      return Success{rcrReturnValue=Nothing,
                     rcrUsedEnergy = ccEnergy - re,
                     ..}
    -- Contract execution of a V1 contract failed with the given reason and potentially a return value 
    (Right (Right (Left cf)), re) ->
      return (Failure{
                 rcrReason = WasmV1.cerToRejectReasonReceive ccContract ccMethod ccParameter cf,
                 rcrReturnValue = WasmV1.returnValueToByteString <$> WasmV1.ccfToReturnValue cf,
                 rcrUsedEnergy = ccEnergy - re})
    -- Contract execution of a V1 contract succeeded with the given return value.
    (Right (Right (Right (rv, reversedEvents))), re) -> -- handleUpdateContractV1 returns events in reverse order
      return Success{rcrReturnValue=Just (WasmV1.returnValueToByteString rv),
                     rcrUsedEnergy = ccEnergy - re,
                     rcrEvents = reverse reversedEvents,
                     ..}
