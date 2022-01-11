{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-| This module tests invoking a contract directly using invokeContract.
-}
module SchedulerTests.SmartContracts.Invoke (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual, Assertion)

import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as OrdMap
import qualified Data.Set as Set
import Data.Word

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash

import Concordium.Types.SeedState (initialSeedState)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Instance
import Concordium.Wasm
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.InvokeContract as InvokeContract

import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import Concordium.GlobalState.DummyData

import SchedulerTests.TestUtils

type ContextM = PersistentBlockStateMonad PV4 BlobStore (ReaderT BlobStore IO)

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

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: Word32
wasmModVersion = 1

deployModule :: ContextM (PersistentBlockState PV4, GSWasm.ModuleInterfaceV GSWasm.V1, WasmModule)
deployModule = do
  wasmSource <- liftIO $ BS.readFile counterSourceFile
  let wm = WasmModule wasmModVersion (ModuleSource wasmSource)
  case WasmV1.processModule wm of
    Nothing -> liftIO $ assertFailure "Invalid counter module."
    Just miv -> do
      let mi = GSWasm.ModuleInterfaceV1 miv
      (_, modState) <- flip bsoPutNewModule (mi, wm) . hpbsPointers =<< initialBlockState
      return (modState, miv, wm)

initContract :: (PersistentBlockState PV4, GSWasm.ModuleInterfaceV GSWasm.V1, WasmModule) -> ContextM (Types.ContractAddress, HashedPersistentBlockState PV4)
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
  case WasmV1.applyInitFun miv cm initContext initName initParam initAmount initInterpreterEnergy of
    Nothing -> -- out of energy
      liftIO $ assertFailure "Initialization ran out of energy."
    Just (Left failure, _) ->
      liftIO $ assertFailure $ "Initialization failed: " ++ show failure
    Just (Right WasmV1.InitSuccess{..}, _) -> do
      let receiveMethods = OrdMap.findWithDefault Set.empty initName (GSWasm.miExposedReceive miv)
      let mkInstance = makeInstance initName receiveMethods miv irdNewState initAmount senderAddress
      (addr, instState) <- bsoPutNewInstance bs mkInstance
      (addr,) <$> freezeBlockState instState

invokeContract :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract ccContract bs = do
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

runCounterTests :: Assertion
runCounterTests = do
  invokeResult <- runBlobStoreTemp "." . runPersistentBlockStateMonad $ do
    bsWithMod <- deployModule
    (addr, stateWithContract) <- initContract bsWithMod
    invokeContract addr stateWithContract
  case invokeResult of
    InvokeContract.Failure{..} -> assertFailure $ "Invocation failed: " ++ show rcrReason
    InvokeContract.Success{..} ->
      case rcrReturnValue of
        Nothing -> assertFailure $ "Invoking a V1 contract must produce a return value."
        Just rv -> assertEqual "Invoking a counter in initial state should return 1" (BS.unpack (WasmV1.returnValueToByteString rv)) [1,0,0,0,0,0,0,0]

tests :: Spec
tests = describe "Invoke contract" $ do
  specify "Counter contract" $ runCounterTests
