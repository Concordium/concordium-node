{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-| This module tests that the correct self-balance is exposed to V1 contracts.
    In essence that the self-balance is updated by the invoke.
-}
module SchedulerTests.SmartContracts.V1.SelfBalance (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual, Assertion)

import Control.Monad.Reader
import Data.Serialize
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash

import Concordium.Types.SeedState (initialSeedState)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.Wasm
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Types.InvokeContract as InvokeContract
import qualified Concordium.Scheduler.InvokeContract as InvokeContract

import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import Concordium.GlobalState.DummyData

import SchedulerTests.TestUtils
import SchedulerTests.SmartContracts.V1.InvokeHelpers (ContextM)
import qualified SchedulerTests.SmartContracts.V1.InvokeHelpers as InvokeHelpers

-- empty state, no accounts, no modules, no instances
initialBlockState :: ContextM (HashedPersistentBlockState PV4)
initialBlockState = initialPersistentState
                    (initialSeedState (Hash.hash "") 1000)
                    dummyCryptographicParameters
                    [mkAccount alesVK alesAccount 1000]
                    dummyIdentityProviders
                    dummyArs
                    dummyKeyCollection
                    dummyChainParameters

transferSourceFile :: FilePath
transferSourceFile = "./testdata/contracts/v1/self-balance.wasm"

deployModule1 :: PersistentBlockState PV4 -> ContextM ((GSWasm.ModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1), PersistentBlockState PV4)
deployModule1 = InvokeHelpers.deployModuleV1 transferSourceFile

-- Initialize a contract with 0 CCD in its balance.
initContract1 :: PersistentBlockState PV4 -> (GSWasm.ModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1) -> ContextM (Types.ContractAddress, PersistentBlockState PV4)
initContract1 = InvokeHelpers.initContractV1 alesAccount (InitName "init_transfer") emptyParameter 0

-- |Invoke an entrypoint and transfer to ourselves.
-- The before and after self-balances are the same.
invokeContract1 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract1 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord32le 1 -- instruction
          putWord64le 0 -- contract index
          putWord64le 0 -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "accept"))
          putByteString "accept" -- entrypoint name
          putWord64le 100 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 123,
        ccMethod = ReceiveName "transfer.forward",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs

-- |Invoke an entrypoint and transfer to another instance. The before and after
-- self-balances are different. The key difference from invokeContract1 test is
-- that the address (the contract index) in the parameter is different.
invokeContract2 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract2 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord32le 1 -- instruction
          putWord64le 1 -- contract index
          putWord64le 0 -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "accept"))
          putByteString "accept" -- entrypoint name
          putWord64le 100 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 123,
        ccMethod = ReceiveName "transfer.forward",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs


-- |Invoke an entrypoint and transfer to an account.
-- The before and after balances are different.
invokeContract3 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract3 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord32le 0 -- instruction
          put alesAccount
          putWord64le 100 -- amount to transfer
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 123,
        ccMethod = ReceiveName "transfer.forward",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs


checkSuccess :: MonadIO m
             => String -- ^ Custom error message.
             -> Types.Amount -- ^ Expected balance before the transfer.
             -> Types.Amount -- ^ Expected balance after the transfer.
             -> InvokeContract.InvokeContractResult -> m ()
checkSuccess msg expectBefore expectAfter icr = liftIO $
  case icr of
    InvokeContract.Failure{..} -> assertFailure $ "Invocation failed ( " ++ show msg ++ "): " ++ show rcrReason
    InvokeContract.Success{..} ->
      case rcrReturnValue of
        Nothing -> assertFailure "Invoking a V1 contract must produce a return value."
        Just rv -> assertEqual msg
                  (BS.unpack (runPut $ (putWord64le . Types._amount $ expectBefore) <> (putWord64le . Types._amount $ expectAfter)))
                  (BS.unpack rv)

runTransferTests :: Assertion
runTransferTests = do
  runBlobStoreTemp "." . runPersistentBlockStateMonad $ do
    initState <- thawBlockState =<< initialBlockState
    (mod1, bsWithMod) <- deployModule1 initState
    (addr1, stateWithContract1) <- initContract1 bsWithMod mod1
    (_another, stateWithBothContracts') <- initContract1 stateWithContract1 mod1
    stateWithBothContracts <- freezeBlockState stateWithBothContracts'
    invokeContract1 addr1 stateWithBothContracts >>= checkSuccess "Self transfer" 123 123
    invokeContract2 addr1 stateWithBothContracts >>= checkSuccess "Transfer to another instance" 123 23
    invokeContract3 addr1 stateWithBothContracts >>= checkSuccess "Transfer to account" 123 23

tests :: Spec
tests = describe "V1: Self balance" $ do
  specify "Transfer contract" runTransferTests
