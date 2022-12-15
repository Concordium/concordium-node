{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the contract default method/fallback functionality.
module SchedulerTests.SmartContracts.V1.Fallback (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as BSS
import Data.Serialize

import qualified Concordium.Scheduler.Types as Types

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.InvokeContract as InvokeContract
import qualified Concordium.Types.InvokeContract as InvokeContract
import Concordium.Wasm

import Concordium.Crypto.DummyData
import Concordium.Types.DummyData

import qualified SchedulerTests.Helpers as Helpers
import qualified SchedulerTests.SmartContracts.V1.InvokeHelpers as InvokeHelpers
import SchedulerTests.TestUtils

-- empty state, no accounts, no modules, no instances
initialBlockState :: Helpers.PersistentBSM PV4 (HashedPersistentBlockState PV4)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccount alesVK alesAccount 1000]

fallbackSourceFile :: FilePath
fallbackSourceFile = "./testdata/contracts/v1/fallback.wasm"

deployModule ::
    Helpers.PersistentBSM
        PV4
        ( PersistentBlockState PV4,
          InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1,
          WasmModuleV GSWasm.V1
        )
deployModule = do
    ((x, y), z) <- InvokeHelpers.deployModuleV1 fallbackSourceFile . hpbsPointers =<< initialBlockState
    return (z, x, y)

initContracts ::
    (PersistentBlockState PV4, InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1) ->
    Helpers.PersistentBSM PV4 ((Types.ContractAddress, Types.ContractAddress), HashedPersistentBlockState PV4)
initContracts (bs, miv, wm) = do
    (ca2, pbs2) <- InvokeHelpers.initContractV1 alesAccount (InitName "init_two") emptyParameter (0 :: Types.Amount) bs (miv, wm)
    -- initialize the "one" contract with the address of the "two" contract
    (ca1, pbs1) <- InvokeHelpers.initContractV1 alesAccount (InitName "init_one") (Parameter $ BSS.toShort (encode ca2)) (0 :: Types.Amount) pbs2 (miv, wm)
    ((ca1, ca2),) <$> freezeBlockState pbs1

-- |Invoke the fallback directly. This should fail with execution failure/trap
-- because it will redirect to "two." which does not exist. Hence this will fail
-- and the fallback will try to look up a non-existing return value.
invokeContract1 ::
    Types.ContractAddress ->
    HashedPersistentBlockState PV4 ->
    Helpers.PersistentBSM PV4 InvokeContract.InvokeContractResult
invokeContract1 ccContract bs = do
    let cm = Types.ChainMetadata 0
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 0,
                  ccMethod = ReceiveName "one.",
                  ccParameter = emptyParameter,
                  ccEnergy = 1_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx cm bs

-- |Invoke "two.do" via "one.do" and the fallback.
invokeContract2 ::
    Types.ContractAddress ->
    HashedPersistentBlockState PV4 ->
    Helpers.PersistentBSM PV4 InvokeContract.InvokeContractResult
invokeContract2 ccContract bs = do
    let cm = Types.ChainMetadata 0
    let ccParameter = Parameter $ BSS.toShort $ "ASDF"
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 0,
                  ccMethod = ReceiveName "one.do",
                  ccEnergy = 1_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx cm bs

runFallbackTests :: Assertion
runFallbackTests = do
    Helpers.runTestBlockState $ do
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
