{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-| This module tests invoking a contract directly using invokeContract.
-}
module SchedulerTests.SmartContracts.Invoke (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual, Assertion)

import Control.Monad.Reader
import Data.Serialize
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as OrdMap
import qualified Data.Set as Set

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash

import Concordium.Types.SeedState (initialSeedState)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.Wasm
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Types.InvokeContract as InvokeContract
import qualified Concordium.Scheduler.InvokeContract as InvokeContract

import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import Concordium.GlobalState.DummyData

import SchedulerTests.TestUtils

type ContextM = PersistentBlockStateMonad PV4 BlobStoreContext (ReaderT BlobStoreContext IO)

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

counterSourceFile :: FilePath
counterSourceFile = "./testdata/contracts/v1/call-counter.wasm"

deployModule :: ContextM (PersistentBlockState PV4, GSWasm.ModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1)
deployModule = do
  wasmSource <- liftIO $ BS.readFile counterSourceFile
  let wm = WasmModuleV (ModuleSource wasmSource)
  case WasmV1.processModule wm of
    Nothing -> liftIO $ assertFailure "Invalid counter module."
    Just miv -> do
      (_, modState) <- flip bsoPutNewModule (miv, wm) . hpbsPointers =<< initialBlockState
      return (modState, miv, wm)

initContract :: (PersistentBlockState PV4, GSWasm.ModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1) -> ContextM (Types.ContractAddress, HashedPersistentBlockState PV4)
initContract (bs, miv, _) = do
  let cm = Types.ChainMetadata 0
  let senderAddress = alesAccount
  let initContext = InitContext{
        initOrigin = senderAddress,
        icSenderPolicies = []
        }
  let initName = InitName "init_counter"
  let initParam = emptyParameter
  let initAmount = 0
  let initInterpreterEnergy = 1_000_000_000
  (cbk, _) <- getCallBacks
  case WasmV1.applyInitFun cbk miv cm initContext initName initParam initAmount initInterpreterEnergy of
    Nothing -> -- out of energy
      liftIO $ assertFailure "Initialization ran out of energy."
    Just (Left failure, _) ->
      liftIO $ assertFailure $ "Initialization failed: " ++ show failure
    Just (Right WasmV1.InitSuccess{..}, _) -> do
      let receiveMethods = OrdMap.findWithDefault Set.empty initName (GSWasm.miExposedReceive miv)
      initialState <- fromForeignReprV1 irdNewState
      let ins = NewInstanceData{
            nidInitName = initName,
            nidEntrypoints = receiveMethods,
            nidInterface = miv,
            nidInitialState = initialState,
            nidInitialAmount = initAmount,
            nidOwner = senderAddress
            }
      (addr, instState) <- bsoPutNewInstance bs ins
      (addr,) <$> freezeBlockState instState

-- |Invoke the contract without an invoker expecting success.
invokeContract1 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract1 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "counter.inc",
        ccParameter = emptyParameter,
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs

-- |Invoke an entrypoint that calls other entrypoints, and expects a parameter.
-- This entrypoint does not return anything, meaning the return value is an empty byte array.
invokeContract2 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract2 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord64le 0 -- contract index
          putWord64le 0 -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "inc"))
          putByteString "inc" -- entrypoint name
          putWord64le 0 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "counter.inc10",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs


-- |Same as 2, but a wrong parameter is passed.
-- Expects runtime failure
invokeContract3 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract3 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "counter.inc10",
        ccEnergy = 1_000_000_000,
        ccParameter = emptyParameter,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs


-- |Same as 2, but with an invoker.
invokeContract4 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract4 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord64le 0 -- contract index
          putWord64le 0 -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "inc"))
          putByteString "inc" -- entrypoint name
          putWord64le 0 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Just (Types.AddressContract (Types.ContractAddress 0 0)),
        ccAmount = 0,
        ccMethod = ReceiveName "counter.inc10",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs


runCounterTests :: Assertion
runCounterTests = do
  runBlobStoreTemp "." . runPersistentBlockStateMonad $ do
    bsWithMod <- deployModule
    (addr, stateWithContract) <- initContract bsWithMod
    invokeContract1 addr stateWithContract >>= \case
      InvokeContract.Failure{..} -> liftIO $ assertFailure $ "Invocation failed: " ++ show rcrReason
      InvokeContract.Success{..} ->
        case rcrReturnValue of
          Nothing -> liftIO $ assertFailure "Invoking a V1 contract must produce a return value."
          Just rv -> liftIO $ assertEqual "Invoking a counter in initial state should return 1" [1,0,0,0,0,0,0,0] (BS.unpack rv)

    invokeContract2 addr stateWithContract >>= \case
      InvokeContract.Failure{..} -> liftIO $ assertFailure $ "Invocation failed: " ++ show rcrReason
      InvokeContract.Success{..} ->
        case rcrReturnValue of
          Nothing -> liftIO $ assertFailure "Invoking a V1 contract must produce a return value."
          Just rv -> liftIO $ assertEqual "Invoking a counter in initial state should return nothing" [] (BS.unpack rv)

    invokeContract3 addr stateWithContract >>= \case
      InvokeContract.Failure{..} -> liftIO $ assertEqual "Invocation should fail: " Types.RuntimeFailure rcrReason
      InvokeContract.Success{} -> liftIO $ assertFailure "Invocation succeeded, but should fail."

    invokeContract4 addr stateWithContract >>= \case
      InvokeContract.Failure{..} -> liftIO $ assertFailure $ "Invocation failed: " ++ show rcrReason
      InvokeContract.Success{..} ->
        case rcrReturnValue of
          Nothing -> liftIO $ assertFailure "Invoking a V1 contract must produce a return value."
          Just rv -> liftIO $ assertEqual "Invoking a counter in initial state should return nothing." [] (BS.unpack rv)


tests :: Spec
tests = describe "Invoke contract" $ do
  specify "V1: Counter contract" runCounterTests
