{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.Scheduler.InvokeContract where

import Lens.Micro.Platform
import Control.Monad.Reader

import qualified Data.FixedByteString as FBS
import qualified Concordium.ID.Types as ID
import Concordium.Logger
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.TreeState (MGSTrans(..))
import Concordium.Types.InvokeContract (ContractContext(..), InvokeContractResult(..))

import Concordium.Scheduler.Environment
import Concordium.Scheduler.Types
import Concordium.Scheduler.EnvironmentImplementation (ContextState(..), maxBlockEnergy, chainMetadata, accountCreationLimit)
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import Concordium.Scheduler
import Concordium.GlobalState.BlockState (InstanceInfoTypeV(..))

newtype InvokeContractMonad (pv :: ProtocolVersion) m a = InvokeContractMonad {_runInvokeContract :: ReaderT (ContextState, BlockState m) m a}
    deriving (Functor,
              Applicative,
              Monad,
              MonadLogger)

deriving instance (Monad m, r ~ BlockState m) => MonadReader (ContextState, r) (InvokeContractMonad pv m)

instance MonadTrans (InvokeContractMonad pv) where
    {-# INLINE lift #-}
    lift = InvokeContractMonad . lift

deriving via (MGSTrans (InvokeContractMonad pv) m) instance BlockStateTypes (InvokeContractMonad pv m)
deriving via (MGSTrans (InvokeContractMonad pv) m) instance BS.AccountOperations m => BS.AccountOperations (InvokeContractMonad pv m)
deriving via (MGSTrans (InvokeContractMonad pv) m) instance BS.ContractStateOperations m => BS.ContractStateOperations (InvokeContractMonad pv m)

instance (Monad m, BS.BlockStateQuery m) => StaticInformation (InvokeContractMonad pv m) where

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

-- |Invoke the contract in the given context.
invokeContract :: forall pv m . (IsProtocolVersion pv, BS.BlockStateQuery m) =>
    SProtocolVersion pv -- ^An argument to fix the protocol version, to make the type non-ambiguous.
    -> ContractContext -- ^Context in which to invoke the contract.
    -> ChainMetadata -- ^Chain metadata corresponding to the block state.
    -> BlockState m -- ^The block state in which to invoke the contract.
    -> m InvokeContractResult
invokeContract _ ContractContext{..} cm bs = do
  -- construct an invoker. Since execution of a contract might depend on this
  -- it is necessary to provide some value. However since many contract entrypoints will
  -- not depend on this it is useful to default to a dummy value if the value is not provided.
  let getInvoker :: InvokeContractMonad pv m
                   (Either
                     (Maybe RejectReason) -- Invocation failed because the relevant contract/account does not exist.
                     ( -- Check that the requested account or contract has enough balance.
                       Amount -> LocalT pv r (InvokeContractMonad pv m) (Address, [ID.AccountCredential], Either ContractAddress IndexedAccountAddress),
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
              let ownerAccountAddress = instanceOwner (iiParameters i)
              getStateAccount ownerAccountAddress >>= \case
                Nothing -> return (Left (Just $ InvalidAccountReference ownerAccountAddress))
                Just acc -> return (Right (checkAndGetBalanceInstanceV0 acc i {iiState = Frozen (iiState i)}, ownerAccountAddress, fst acc))
            Just (BS.InstanceInfoV1 i) -> do
              let ownerAccountAddress = instanceOwner (iiParameters i)
              getStateAccount ownerAccountAddress >>= \case
                Nothing -> return (Left (Just $ InvalidAccountReference ownerAccountAddress))
                Just acc -> return (Right (checkAndGetBalanceInstanceV0 acc i {iiState = Frozen (iiState i)}, ownerAccountAddress, fst acc))
  let runContractComp = 
        getInvoker >>= \case
          Left err -> return (Left err, ccEnergy)
          Right (invoker, addr, ai) -> do
            let comp = do
                  istance <- getContractInstance ccContract `rejectingWith` InvalidContractAddress ccContract
                  case istance of
                    BS.InstanceInfoV0 i -> Left <$> handleContractUpdateV0 addr i {iiState = Frozen (iiState i)} invoker ccAmount ccMethod ccParameter
                    BS.InstanceInfoV1 i -> Right <$> handleContractUpdateV1 addr i {iiState = Frozen (iiState i)} (fmap Right . invoker) ccAmount ccMethod ccParameter
            (r, cs) <- runLocalT @pv comp ccAmount ai ccEnergy ccEnergy
            return (r, _energyLeft cs)
      contextState = ContextState{_maxBlockEnergy = ccEnergy, _accountCreationLimit = 0, _chainMetadata = cm}
  runReaderT (_runInvokeContract runContractComp) (contextState, bs) >>= \case
    (Left Nothing, re) -> -- cannot happen (this would mean out of block energy), but this is safe to do and not wrong
        return Failure{rcrReason = OutOfEnergy, rcrUsedEnergy = ccEnergy - re}
    (Left (Just rcrReason), re) ->
      return Failure{rcrUsedEnergy = ccEnergy - re,..}
    (Right (Left rcrEvents), re) ->
      return Success{rcrReturnValue=Nothing,
                     rcrUsedEnergy = ccEnergy - re,
                     ..}
    (Right (Right (Left cf)), re) ->
      return (Failure{
                 rcrReason = WasmV1.cerToRejectReasonReceive ccContract ccMethod ccParameter cf,
                 rcrUsedEnergy = ccEnergy - re})
    (Right (Right (Right (rv, reversedEvents))), re) -> -- handleUpdateContractV1 returns events in reverse order
      return Success{rcrReturnValue=Just (WasmV1.returnValueToByteString rv),
                     rcrUsedEnergy = ccEnergy - re,
                     rcrEvents = reverse reversedEvents,
                     ..}
