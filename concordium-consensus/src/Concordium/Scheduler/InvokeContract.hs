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

import qualified Data.Aeson as AE
import Lens.Micro.Platform
import Control.Monad.Reader

import qualified Data.FixedByteString as FBS
import qualified Concordium.Wasm as Wasm
import Concordium.Logger
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Instance as Instance
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.TreeState (MGSTrans(..))

import Concordium.Scheduler.Environment
import Concordium.Scheduler.Types
import Concordium.Scheduler.EnvironmentImplementation (ContextState(..), maxBlockEnergy, chainMetadata, accountCreationLimit)
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import Concordium.Scheduler (handleContractUpdateV0, handleContractUpdateV1, checkAndGetBalanceInstance, checkAndGetBalanceAccount)

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

  {-# INLINE getAccount #-}
  getAccount !addr = lift . flip BS.getAccount addr =<< view _2

data ContractContext = ContractContext {
  -- |Invoker of the contract. If this is not supplied then the contract will be
  -- invoked, by an account with address 0, no credentials and sufficient amount
  -- of CCD to cover the transfer amount. If given, the relevant address must
  -- exist in the blockstate.
  ccInvoker :: !(Maybe Address),
  -- |Contract to invoke.
  ccContract :: !ContractAddress,
  -- |Amount to invoke the contract with.
  ccAmount :: !Amount,
  -- |Which entrypoint to invoke.
  ccMethod :: !Wasm.ReceiveName,
  -- |And with what parameter.
  ccParameter :: !Wasm.Parameter,
  -- |And what amount of energy to allow for execution.
  ccEnergy :: !Energy
  }

-- |This FromJSON instance defaults a number of values if they are not given
-- - energy defaults to maximum possible
-- - amount defaults to 0
-- - parameter defaults to the empty one
instance AE.FromJSON ContractContext where
  parseJSON = AE.withObject "ContractContext" $ \obj -> do
    ccInvoker <- obj AE..:? "invoker"
    ccContract <- obj AE..: "contract"
    ccAmount <- obj AE..:? "amount" AE..!= 0
    ccMethod <- obj AE..: "method"
    ccParameter <- obj AE..:? "parameter" AE..!= Wasm.emptyParameter
    ccEnergy <- obj AE..:? "energy" AE..!= maxBound
    return ContractContext{..}

data InvokeContractResult =
  -- |Contract execution failed for the given reason.
  Failure {
      rcrReason :: !RejectReason,
      -- |Energy used by the execution.
      rcrUsedEnergy :: !Energy
      }
  -- |Contract execution succeeded.
  | Success {
      -- |If invoking a V0 contract this is Nothing, otherwise it is
      -- the return value produced by the call.
      rcrReturnValue :: !(Maybe WasmV1.ReturnValue),
      -- |Events produced by contract execution.
      rcrEvents :: ![Event],
      -- |Energy used by the execution.
      rcrUsedEnergy :: !Energy
      }

instance AE.ToJSON InvokeContractResult where
  toJSON Failure{..} = AE.object [
    "tag" AE..= AE.String "failure",
    "reason" AE..= rcrReason,
    "usedEnergy" AE..= rcrUsedEnergy
    ]
  toJSON Success{..} = AE.object [
    "tag" AE..= AE.String "success",
    "returnValue" AE..= rcrReturnValue,
    "events" AE..= rcrEvents,
    "usedEnergy" AE..= rcrUsedEnergy
    ]

-- |Invoke the contract in the given context.
invokeContract :: forall pv m . (IsProtocolVersion pv, BS.BlockStateQuery m) =>
    SProtocolVersion pv -- An argument to fix the protocol version, to make the type non-ambiguous.
    -> ContractContext -- ^Context in which to invoke the contract.
    -> ChainMetadata -- ^Chain metadata corresponding to the block state.
    -> BlockState m -- ^The block state in which to invoke the contract.
    -> m InvokeContractResult
invokeContract _ ContractContext{..} cm bs = do
  let getInvoker =
        case ccInvoker of
          Nothing -> -- if the invoker is not supplied create a dummy one
            let zeroAddress = AccountAddress . FBS.pack . replicate 32 $ 0
                maxIndex = maxBound
            in return (Right (const (return (AddressAccount zeroAddress, [], Right (maxIndex, zeroAddress))), zeroAddress, maxIndex))
          Just (AddressAccount accInvoker) -> getAccount accInvoker >>= \case
            Nothing -> return (Left (Just (InvalidAccountReference accInvoker)))
            Just acc -> return (Right (checkAndGetBalanceAccount accInvoker acc, accInvoker, (fst acc)))
          Just (AddressContract contractInvoker) -> getContractInstance contractInvoker >>= \case
            Nothing -> return (Left (Just (InvalidContractAddress contractInvoker)))
            Just (Instance.InstanceV0 i@Instance.InstanceV{..}) -> do
              let ownerAccountAddress = instanceOwner _instanceVParameters
              getAccount ownerAccountAddress >>= \case
                Nothing -> return (Left (Just $ InvalidAccountReference ownerAccountAddress))
                Just acc -> return (Right (checkAndGetBalanceInstance acc i, ownerAccountAddress, (fst acc)))
            Just (Instance.InstanceV1 i@Instance.InstanceV{..}) -> do
              let ownerAccountAddress = instanceOwner _instanceVParameters
              getAccount ownerAccountAddress >>= \case
                Nothing -> return (Left (Just $ InvalidAccountReference ownerAccountAddress))
                Just acc -> return (Right (checkAndGetBalanceInstance acc i, ownerAccountAddress, (fst acc)))
  let runContractComp = 
        getInvoker >>= \case
          Left err -> return (Left err, ccEnergy)
          Right (invoker, addr, ai) -> do
            let comp = do
                  istance <- getContractInstance ccContract `rejectingWith` InvalidContractAddress ccContract
                  case istance of
                    InstanceV0 i -> Left <$> handleContractUpdateV0 addr i invoker ccAmount ccMethod ccParameter
                    InstanceV1 i -> Right <$> handleContractUpdateV1 addr i invoker ccAmount ccMethod ccParameter
            (r, cs) <- runLocalT @pv comp ccAmount ai ccEnergy ccEnergy
            return (r, _energyLeft cs)
      contextState = ContextState{_maxBlockEnergy = ccEnergy, _accountCreationLimit = 0, _chainMetadata = cm}
  runReaderT (_runInvokeContract runContractComp) (contextState, bs) >>= \case
    (Left Nothing, re) -> -- cannot happen, but this is safe to do and not wrong
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
    (Right (Right (Right (rv, rcrEvents))), re) ->
      return Success{rcrReturnValue=Just rv,
                     rcrUsedEnergy = ccEnergy - re,
                     ..}
