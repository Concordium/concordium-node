{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-| This module tests invoking a V1 contract which invokes an operation which fails.
    The test is to make sure error codes are correctly returned to the contract.
-}
module SchedulerTests.SmartContracts.V1.ErrorCodes (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual, Assertion)

import Control.Monad.Reader
import Data.Serialize
import qualified Data.ByteString.Short as BSS
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
import qualified Concordium.Scheduler.WasmIntegration as WasmV0
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Types.InvokeContract as InvokeContract
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

callerSourceFile :: FilePath
callerSourceFile = "./testdata/contracts/v1/caller.wasm"

emptyContractSourceFile :: FilePath
emptyContractSourceFile = "./testdata/contracts/empty.wasm"

deployModule1 :: PersistentBlockState PV4 -> ContextM ((GSWasm.ModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1), PersistentBlockState PV4)
deployModule1 bs = do
  wasmSource <- liftIO $ BS.readFile callerSourceFile
  let wm = WasmModuleV (ModuleSource wasmSource)
  case WasmV1.processModule wm of
    Nothing -> liftIO $ assertFailure "Invalid caller module."
    Just miv -> do
      (_, modState) <- bsoPutNewModule bs (miv, wm)
      return ((miv, wm), modState)

initContract1 :: PersistentBlockState PV4 -> (GSWasm.ModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1) -> ContextM (Types.ContractAddress, PersistentBlockState PV4)
initContract1 bs (miv, _) = do
  let cm = Types.ChainMetadata 0
  let senderAddress = alesAccount
  let initContext = InitContext{
        initOrigin = senderAddress,
        icSenderPolicies = []
        }
  let initName = InitName "init_caller"
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
      bsoPutNewInstance bs mkInstance

deployModule0 :: PersistentBlockState PV4 -> ContextM ((GSWasm.ModuleInterfaceV GSWasm.V0, WasmModuleV GSWasm.V0), PersistentBlockState PV4)
deployModule0 bs = do
  wasmSource <- liftIO $ BS.readFile emptyContractSourceFile
  let wm = WasmModuleV (ModuleSource wasmSource)
  case WasmV0.processModule wm of
    Nothing -> liftIO $ assertFailure "Invalid caller module."
    Just miv -> do
      (_, modState) <- bsoPutNewModule bs (miv, wm)
      return ((miv, wm), modState)

initContract0 :: PersistentBlockState PV4 -> (GSWasm.ModuleInterfaceV GSWasm.V0, WasmModuleV GSWasm.V0) -> ContextM (Types.ContractAddress, PersistentBlockState PV4)
initContract0 bs (miv, _) = do
  let cm = Types.ChainMetadata 0
  let senderAddress = alesAccount
  let initContext = InitContext{
        initOrigin = senderAddress,
        icSenderPolicies = []
        }
  let initName = InitName "init_empty"
  let initParam = emptyParameter
  let initAmount = 0
  let initInterpreterEnergy = 1_000_000_000
  case WasmV0.applyInitFun miv cm initContext initName initParam initAmount initInterpreterEnergy of
    Nothing -> -- out of energy
      liftIO $ assertFailure "Initialization ran out of energy."
    Just (Left failure, _) ->
      liftIO $ assertFailure $ "Initialization failed: " ++ show failure
    Just (Right SuccessfulResultData{..}, _) -> do
      let receiveMethods = OrdMap.findWithDefault Set.empty initName (GSWasm.miExposedReceive miv)
      let mkInstance = makeInstance initName receiveMethods miv newState initAmount senderAddress
      bsoPutNewInstance bs mkInstance


-- |Invoke an entrypoint that calls the "fail" entrypoint.
-- The expected return code is
-- 0x0100_ffff_ffef
-- because
-- - the return value is pushed (hence 01)
-- - the call to "fail" fails with a "logic error" (hence the 00)
-- - the return value is -17 (which when converted with two's complement i32 is ffff_ffef)
invokeContract0 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract0 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord32le 1 -- instruction
          putWord64le 0 -- contract index
          putWord64le 0 -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "fail"))
          putByteString "fail" -- entrypoint name
          putWord64le 0 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "caller.call",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs

-- |Invoke an entrypoint that tries to transfer an amount that it does not have via contract invoke.
-- The expected return code is
-- 0x0001_0000_0000
-- because
-- - there is no return value (hence 00)
-- - the call fails with "insufficient funds" (hence 01)
-- - the remaining is set to 0 since there is no logic error
invokeContract1 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract1 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord32le 1 -- instruction
          putWord64le 0 -- contract index
          putWord64le 0 -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "fail"))
          putByteString "fail" -- entrypoint name
          putWord64le 10000 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "caller.call",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs

-- |Invoke an entrypoint that tries to invoke a non-existing contract.
-- The expected return code is
-- 0x0003_0000_0000
-- because
-- - there is no return value (hence 00)
-- - the call fails with "missing contract" (hence 03)
-- - the remaining is set to 0 since there is no logic error
invokeContract3 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract3 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord32le 1 -- instruction
          putWord64le 1232 -- contract index, must not exist in the state
          putWord64le 0 -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "fail"))
          putByteString "fail" -- entrypoint name
          putWord64le 0 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "caller.call",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs

-- |Invoke an entrypoint that tries to invoke a non-existing entrypoint.
-- The expected return code is
-- 0x0004_0000_0000
-- because
-- - there is no return value (hence 00)
-- - the call fails with "invalid entrypoint" (hence 04)
-- - the remaining is set to 0 since there is no logic error
invokeContract4 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract4 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord32le 1 -- instruction
          putWord64le 0 -- contract index
          putWord64le 0 -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "nonexisting"))
          putByteString "nonexisting" -- entrypoint name
          putWord64le 0 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "caller.call",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs


-- |Invoke an entrypoint that traps
-- The expected return code is
-- 0x0006_0000_0000
-- because
-- - there is no return value (hence 00)
-- - the call fails with "trap" (hence 06)
-- - the remaining is set to 0 since there is no logic error
invokeContract6 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract6 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord32le 1 -- instruction
          putWord64le 0 -- contract index
          putWord64le 0 -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "trap"))
          putByteString "trap" -- entrypoint name
          putWord64le 0 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "caller.call",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs


-- |Invoke an entrypoint that traps
-- The expected return code is
-- 0x0002_0000_0000
-- because
-- - there is no return value (hence 00)
-- - the call fails with "missing account" (hence 02)
-- - the remaining is set to 0 since there is no logic error
invokeContract2 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract2 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord32le 0 -- instruction
          put thomasAccount
          putWord64le 0 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "caller.call",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs


-- |Invoke a V0 contract such that the invocation fails.
-- The expected return code is
-- 0x0005_0000_0000
-- because
-- - there is no return value (hence 00)
-- - the call fails with "message failed" (hence 05)
-- - the remaining is set to 0 since there is no logic error
invokeContract5 :: Types.ContractAddress -> Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract5 ccContract targetContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ runPut $ do
          putWord32le 1 -- instruction
          putWord64le (fromIntegral (Types.contractIndex targetContract)) -- contract index
          putWord64le (fromIntegral (Types.contractSubindex targetContract)) -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "nonexistent"))
          putByteString "nonexistent" -- entrypoint name
          putWord64le 0 -- amount
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "caller.call",
        ccEnergy = 1_000_000_000,
        ..
        }
  InvokeContract.invokeContract Types.SP4 ctx cm bs


checkSuccess :: MonadIO m => String -> Word64 -> InvokeContract.InvokeContractResult -> m ()
checkSuccess msg targetValue icr = liftIO $
  case icr of
    InvokeContract.Failure{..} -> assertFailure $ "Invocation failed ( " ++ show msg ++ "): " ++ show rcrReason
    InvokeContract.Success{..} ->
      case rcrReturnValue of
        Nothing -> assertFailure "Invoking a V1 contract must produce a return value."
        Just rv -> assertEqual msg
                  (BS.unpack (runPut (putWord64le targetValue)))
                  (BS.unpack rv)

runCallerTests :: Assertion
runCallerTests = do
  runBlobStoreTemp "." . runPersistentBlockStateMonad $ do
    initState <- thawBlockState =<< initialBlockState
    (mod1, bsWithMod) <- deployModule1 initState
    (mod0, bsWithMods) <- deployModule0 bsWithMod
    (addr1, stateWithContract1) <- initContract1 bsWithMods mod1
    (addr0, stateWithContracts') <- initContract0 stateWithContract1 mod0
    stateWithContracts <- freezeBlockState stateWithContracts'
    let targetValue0 = 0x0100_ffff_ffef
    invokeContract0 addr1 stateWithContracts >>= checkSuccess "Invoking a caller with logic error" targetValue0
    let targetValue1 = 0x0001_0000_0000
    invokeContract1 addr1 stateWithContracts >>= checkSuccess "Invoking a caller with insufficient funds" targetValue1
    let targetValue3 = 0x0003_0000_0000
    invokeContract3 addr1 stateWithContracts >>= checkSuccess "Invoking a non-existing contract" targetValue3
    let targetValue4 = 0x0004_0000_0000
    invokeContract4 addr1 stateWithContracts >>= checkSuccess "Invoking non-existing entrypoint" targetValue4
    let targetValue6 = 0x0006_0000_0000
    invokeContract6 addr1 stateWithContracts >>= checkSuccess "Invoking non-existing entrypoint" targetValue6
    let targetValue2 = 0x0002_0000_0000
    invokeContract2 addr1 stateWithContracts >>= checkSuccess "Transferring to missing account" targetValue2

    let targetValue5 = 0x0005_0000_0000
    invokeContract5 addr1 addr0 stateWithContracts >>= checkSuccess "Invoking a V0 contract that fails." targetValue5


tests :: Spec
tests = describe "V1: Invoke contract" $ do
  specify "Caller contract" runCallerTests
