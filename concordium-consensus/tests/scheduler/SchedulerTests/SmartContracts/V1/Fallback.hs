{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-| Tests for the contract default method/fallback functionality.
-}
module SchedulerTests.SmartContracts.V1.Fallback (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual, Assertion)

import Control.Monad.Reader
import Data.Serialize
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as BS8

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
import qualified SchedulerTests.SmartContracts.V1.InvokeHelpers as InvokeHelpers

type ContextM = PersistentBlockStateMonad PV4 (PersistentBlockStateContext PV4) (BlobStoreM' (PersistentBlockStateContext PV4))

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

fallbackSourceFile :: FilePath
fallbackSourceFile = "./testdata/contracts/v1/fallback.wasm"

deployModule :: ContextM (PersistentBlockState PV4, InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1)
deployModule = do
  ((x, y), z) <- InvokeHelpers.deployModuleV1 fallbackSourceFile . hpbsPointers =<< initialBlockState
  return (z, x, y)

initContracts :: (PersistentBlockState PV4, InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1)
              -> ContextM ((Types.ContractAddress, Types.ContractAddress), HashedPersistentBlockState PV4)
initContracts (bs, miv, wm) = do
  (ca2, pbs2) <- InvokeHelpers.initContractV1 alesAccount (InitName "init_two") emptyParameter (0 :: Types.Amount) bs (miv, wm)
  -- initialize the "one" contract with the address of the "two" contract
  (ca1, pbs1) <- InvokeHelpers.initContractV1 alesAccount (InitName "init_one") (Parameter $ BSS.toShort (encode ca2)) (0 :: Types.Amount) pbs2 (miv, wm)
  ((ca1, ca2),) <$> freezeBlockState pbs1

-- |Invoke the fallback directly. This should fail with execution failure/trap
-- because it will redirect to "two." which does not exist. Hence this will fail
-- and the fallback will try to look up a non-existing return value.
invokeContract1 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract1 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "one.",
        ccParameter = emptyParameter,
        ccEnergy = 1_000_000,
        ..
        }
  InvokeContract.invokeContract ctx cm bs

-- |Invoke "two.do" via "one.do" and the fallback.
invokeContract2 :: Types.ContractAddress -> HashedPersistentBlockState PV4 -> ContextM InvokeContract.InvokeContractResult
invokeContract2 ccContract bs = do
  let cm = Types.ChainMetadata 0
  let ccParameter = Parameter $ BSS.toShort $ "ASDF"
  let ctx = InvokeContract.ContractContext{
        ccInvoker = Nothing,
        ccAmount = 0,
        ccMethod = ReceiveName "one.do",
        ccEnergy = 1_000_000,
        ..
        }
  InvokeContract.invokeContract ctx cm bs

runFallbackTests :: Assertion
runFallbackTests = do
  runBlobStoreTemp "." . withNewAccountCache 100 . runPersistentBlockStateMonad $ do
    bsWithMod <- deployModule
    ((addr1, _), stateWithContract) <- initContracts bsWithMod
    invokeContract1 addr1 stateWithContract >>= \case
      InvokeContract.Failure{..} -> liftIO $ assertEqual "Invocation should trap: " Types.RuntimeFailure rcrReason
      InvokeContract.Success{} -> liftIO $ assertFailure "Invocation of one. directly should fail."

    invokeContract2 addr1 stateWithContract >>= \case
      InvokeContract.Failure{..} -> liftIO $ assertFailure $ "Invocation failed: " ++ show rcrReason
      InvokeContract.Success{..} ->
        case rcrReturnValue of
          Nothing -> liftIO $ assertFailure "Invoking a V1 contract must produce a return value."
          Just rv -> liftIO $ assertEqual "Invoking a fallback should return the parameter." "ASDF" (BS8.unpack rv)

tests :: Spec
tests = describe "Invoke contract" $ do
  specify "V1: Fallback contract" runFallbackTests
