{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module tests invoking a contract directly using invokeContract.
module SchedulerTests.SmartContracts.Invoke (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec

import Control.Monad.Reader
import qualified Data.ByteString as BS
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

initialBlockState :: Helpers.PersistentBSM PV4 (HashedPersistentBlockState PV4)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccount alesVK alesAccount 1_000]

counterSourceFile :: FilePath
counterSourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/call-counter.wasm"

deployModule ::
    Helpers.PersistentBSM
        PV4
        ( PersistentBlockState PV4,
          InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1,
          WasmModuleV GSWasm.V1
        )
deployModule = do
    ((x, y), z) <- InvokeHelpers.deployModuleV1 counterSourceFile . hpbsPointers =<< initialBlockState
    return (z, x, y)

initContract ::
    ( PersistentBlockState PV4,
      InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1,
      WasmModuleV GSWasm.V1
    ) ->
    Helpers.PersistentBSM PV4 (Types.ContractAddress, HashedPersistentBlockState PV4)
initContract (bs, miv, wm) = do
    (ca, pbs) <-
        InvokeHelpers.initContractV1
            alesAccount
            (InitName "init_counter")
            emptyParameter
            (0 :: Types.Amount)
            bs
            (miv, wm)
    (ca,) <$> freezeBlockState pbs

-- |Invoke the contract without an invoker expecting success.
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
                  ccMethod = ReceiveName "counter.inc",
                  ccParameter = emptyParameter,
                  ccEnergy = 1_000_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx cm bs

-- |Invoke an entrypoint that calls other entrypoints, and expects a parameter.
-- This entrypoint does not return anything, meaning the return value is an empty byte array.
invokeContract2 ::
    Types.ContractAddress ->
    HashedPersistentBlockState PV4 ->
    Helpers.PersistentBSM PV4 InvokeContract.InvokeContractResult
invokeContract2 ccContract bs = do
    let cm = Types.ChainMetadata 0
    let ccParameter = Parameter $ BSS.toShort $ runPut $ do
            putWord64le 0 -- contract index
            putWord64le 0 -- contract subindex
            putWord16le 0 -- length of parameter
            putWord16le (fromIntegral (BSS.length "inc"))
            putByteString "inc" -- entrypoint name
            putWord64le 0 -- amount
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 0,
                  ccMethod = ReceiveName "counter.inc10",
                  ccEnergy = 1_000_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx cm bs

-- |Same as 2, but a wrong parameter is passed.
-- Expects runtime failure
invokeContract3 ::
    Types.ContractAddress ->
    HashedPersistentBlockState PV4 ->
    Helpers.PersistentBSM PV4 InvokeContract.InvokeContractResult
invokeContract3 ccContract bs = do
    let cm = Types.ChainMetadata 0
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 0,
                  ccMethod = ReceiveName "counter.inc10",
                  ccEnergy = 1_000_000_000,
                  ccParameter = emptyParameter,
                  ..
                }
    InvokeContract.invokeContract ctx cm bs

-- |Same as 2, but with an invoker.
invokeContract4 ::
    Types.ContractAddress ->
    HashedPersistentBlockState PV4 ->
    Helpers.PersistentBSM PV4 InvokeContract.InvokeContractResult
invokeContract4 ccContract bs = do
    let cm = Types.ChainMetadata 0
    let ccParameter = Parameter $ BSS.toShort $ runPut $ do
            putWord64le 0 -- contract index
            putWord64le 0 -- contract subindex
            putWord16le 0 -- length of parameter
            putWord16le (fromIntegral (BSS.length "inc"))
            putByteString "inc" -- entrypoint name
            putWord64le 0 -- amount
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Just (Types.AddressContract (Types.ContractAddress 0 0)),
                  ccAmount = 0,
                  ccMethod = ReceiveName "counter.inc10",
                  ccEnergy = 1_000_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx cm bs

runCounterTests :: Assertion
runCounterTests = do
    Helpers.runTestBlockState $ do
        bsWithMod <- deployModule
        (addr, stateWithContract) <- initContract bsWithMod
        invokeContract1 addr stateWithContract >>= \case
            InvokeContract.Failure{..} -> liftIO $ assertFailure $ "Invocation failed: " ++ show rcrReason
            InvokeContract.Success{..} ->
                case rcrReturnValue of
                    Nothing -> liftIO $ assertFailure "Invoking a V1 contract must produce a return value."
                    Just rv -> liftIO $ assertEqual "Invoking a counter in initial state should return 1" [1, 0, 0, 0, 0, 0, 0, 0] (BS.unpack rv)

        invokeContract2 addr stateWithContract >>= \case
            InvokeContract.Failure{..} -> liftIO $ assertFailure $ "Invocation failed: " ++ show rcrReason
            InvokeContract.Success{..} ->
                case rcrReturnValue of
                    Nothing -> liftIO $ assertFailure "Invoking a V1 contract must produce a return value."
                    Just rv -> liftIO $ assertEqual "Invoking a counter in initial state should return an empty array." [] (BS.unpack rv)

        invokeContract3 addr stateWithContract >>= \case
            InvokeContract.Failure{..} -> liftIO $ assertEqual "Invocation should fail: " Types.RuntimeFailure rcrReason
            InvokeContract.Success{} -> liftIO $ assertFailure "Invocation succeeded, but should fail."

        invokeContract4 addr stateWithContract >>= \case
            InvokeContract.Failure{..} -> liftIO $ assertFailure $ "Invocation failed: " ++ show rcrReason
            InvokeContract.Success{..} ->
                case rcrReturnValue of
                    Nothing -> liftIO $ assertFailure "Invoking a V1 contract must produce a return value."
                    Just rv -> liftIO $ assertEqual "Invoking a counter in initial state should return an empty array." [] (BS.unpack rv)

tests :: Spec
tests =
    describe "Invoke contract" $
        specify "V1: Counter contract" runCounterTests
