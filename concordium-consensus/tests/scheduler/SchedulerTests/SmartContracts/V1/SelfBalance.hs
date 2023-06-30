{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module tests that the correct self-balance is exposed to V1 contracts
--    In essence that the self-balance is updated by the invoke.
module SchedulerTests.SmartContracts.V1.SelfBalance (tests) where

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

import qualified SchedulerTests.Helpers as Helpers
import qualified SchedulerTests.SmartContracts.V1.InvokeHelpers as InvokeHelpers
import SchedulerTests.TestUtils

-- empty state, no accounts, no modules, no instances
initialBlockState :: Helpers.PersistentBSM PV4 (HashedPersistentBlockState PV4)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 1_000 0]

selfBalanceSourceFile :: FilePath
selfBalanceSourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/self-balance.wasm"

nestedSelfBalanceSourceFile :: FilePath
nestedSelfBalanceSourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/self-balance-nested.wasm"

deployModule1 ::
    PersistentBlockState PV4 ->
    Helpers.PersistentBSM
        PV4
        ( (InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1),
          PersistentBlockState PV4
        )
deployModule1 = InvokeHelpers.deployModuleV1 Types.SP4 selfBalanceSourceFile

-- Initialize a contract with 0 CCD in its balance.
initContract1 ::
    PersistentBlockState PV4 ->
    (InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1) ->
    Helpers.PersistentBSM PV4 (Types.ContractAddress, PersistentBlockState PV4)
initContract1 =
    InvokeHelpers.initContractV1
        (Helpers.accountAddressFromSeed 0)
        (InitName "init_transfer")
        emptyParameter
        0

-- |Invoke an entrypoint and transfer to ourselves.
-- The before and after self-balances are the same.
invokeContract1 ::
    Types.ContractAddress ->
    HashedPersistentBlockState PV4 ->
    Helpers.PersistentBSM PV4 InvokeContract.InvokeContractResult
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
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 123,
                  ccMethod = ReceiveName "transfer.forward",
                  ccEnergy = 1_000_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx cm bs

-- |Invoke an entrypoint and transfer to another instance. The before and after
-- self-balances are different. The key difference from invokeContract1 test is
-- that the address (the contract index) in the parameter is different.
invokeContract2 ::
    Types.ContractAddress ->
    HashedPersistentBlockState PV4 ->
    Helpers.PersistentBSM PV4 InvokeContract.InvokeContractResult
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
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 123,
                  ccMethod = ReceiveName "transfer.forward",
                  ccEnergy = 1_000_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx cm bs

-- |Invoke an entrypoint and transfer to an account.
-- The before and after balances are different.
invokeContract3 ::
    Types.ContractAddress ->
    HashedPersistentBlockState PV4 ->
    Helpers.PersistentBSM PV4 InvokeContract.InvokeContractResult
invokeContract3 ccContract bs = do
    let cm = Types.ChainMetadata 0
    let ccParameter = Parameter $ BSS.toShort $ runPut $ do
            putWord32le 0 -- instruction
            put (Helpers.accountAddressFromSeed 0)
            putWord64le 100 -- amount to transfer
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 123,
                  ccMethod = ReceiveName "transfer.forward",
                  ccEnergy = 1_000_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx cm bs

checkSuccess ::
    MonadIO m =>
    -- | Custom error message.
    String ->
    -- | Expected balance before the transfer.
    Types.Amount ->
    -- | Expected balance after the transfer.
    Types.Amount ->
    InvokeContract.InvokeContractResult ->
    m ()
checkSuccess msg expectBefore expectAfter icr = liftIO $
    case icr of
        InvokeContract.Failure{..} ->
            assertFailure $
                "Invocation failed ( " ++ show msg ++ "): " ++ show rcrReason
        InvokeContract.Success{..} ->
            case rcrReturnValue of
                Nothing -> assertFailure "Invoking a V1 contract must produce a return value."
                Just rv ->
                    assertEqual
                        msg
                        ( BS.unpack $
                            runPut $
                                (putWord64le . Types._amount $ expectBefore)
                                    <> (putWord64le . Types._amount $ expectAfter)
                        )
                        (BS.unpack rv)

-- |Deploy the module that contains the @test@ contract to test nested self-transfers.
deployModule2 ::
    PersistentBlockState PV4 ->
    Helpers.PersistentBSM
        PV4
        ( (InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1),
          PersistentBlockState PV4
        )
deployModule2 = InvokeHelpers.deployModuleV1 Types.SP4 nestedSelfBalanceSourceFile

-- |Initialize the @test@ contract for testing nested self transfers.
-- The initial balance of the contract is 456
initContract2 ::
    -- |Initial balance of the contract
    Types.Amount ->
    -- |State to create the contract in.
    PersistentBlockState PV4 ->
    -- |And the module from which to initialize the contract.
    (InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1) ->
    -- |The address of the created contract, and the new state.
    Helpers.PersistentBSM PV4 (Types.ContractAddress, PersistentBlockState PV4)
initContract2 =
    InvokeHelpers.initContractV1
        (Helpers.accountAddressFromSeed 0)
        (InitName "init_test")
        emptyParameter

runSelfBalanceTests :: Assertion
runSelfBalanceTests = do
    Helpers.runTestBlockState $ do
        initState <- thawBlockState =<< initialBlockState
        (mod1, bsWithMod) <- deployModule1 initState
        (addr1, stateWithContract1) <- initContract1 bsWithMod mod1
        (_another, stateWithBothContracts') <- initContract1 stateWithContract1 mod1
        stateWithBothContracts <- freezeBlockState stateWithBothContracts'
        invokeContract1 addr1 stateWithBothContracts >>= checkSuccess "Self selfBalance" 123 123
        invokeContract2 addr1 stateWithBothContracts >>= checkSuccess "SelfBalance to another instance" 123 23
        invokeContract3 addr1 stateWithBothContracts >>= checkSuccess "SelfBalance to account" 123 23

-- |Invoke the @invoke_nested@ entrypoint at the given address.
invokeNestedSelfBalanceTest ::
    -- |Address of the contract to invoke.
    Types.ContractAddress ->
    HashedPersistentBlockState PV4 ->
    Helpers.PersistentBSM PV4 InvokeContract.InvokeContractResult
invokeNestedSelfBalanceTest ccContract bs = do
    let cm = Types.ChainMetadata 0
    let innerEntrypointName = "accept"
    let outerEntrypointName = "invoke_nested"
    let invokeItselfParameter = runPut $ do
            putWord32le 1 -- instruction, invoke contract
            -- contract address, serialized in little endian since that is what
            -- the contract expects
            putWord64le (fromIntegral (Types.contractIndex ccContract))
            putWord64le (fromIntegral (Types.contractSubindex ccContract))
            -- (invoke itself with the empty parameter, this is just so we get to the resume)
            putWord16le 0
            -- the entrypoint to invoke.
            putWord16le (fromIntegral (BS.length innerEntrypointName))
            putByteString innerEntrypointName
            putWord64le 0 -- amount to transfer
    let ccParameter = Parameter $ BSS.toShort $ runPut $ do
            putWord32le 1 -- instruction, invoke contract
            -- contract address, serialized in little endian since that is what
            -- the contract expects
            putWord64le (fromIntegral (Types.contractIndex ccContract))
            putWord64le (fromIntegral (Types.contractSubindex ccContract))
            -- (invoke itself with the empty parameter, this is just so we get to the resume)
            putWord16le (fromIntegral (BS.length invokeItselfParameter))
            putByteString invokeItselfParameter
            -- the entrypoint to invoke.
            putWord16le (fromIntegral (BS.length outerEntrypointName))
            putByteString outerEntrypointName
            putWord64le 100 -- amount to transfer
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 10,
                  ccMethod = ReceiveName "test.invoke_nested",
                  ccEnergy = 1_000_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx cm bs

-- A starts with 123
-- account transfers 10 to A
-- A transfers 100 to itself
-- A transfers 0 to itself.

-- The balances observed are after each call, in reverse order, so they should
-- be
-- A: 123 + 10
-- B: 123 + 10
--
-- Previously there was a bug, so the second-innermost call, **after the resume**, the self balance was reported
-- incorrectly since the +10 delta was applied twice.
checkNestedSelfBalanceTest ::
    MonadIO m =>
    InvokeContract.InvokeContractResult ->
    m ()
checkNestedSelfBalanceTest icr = liftIO $
    case icr of
        InvokeContract.Failure{..} ->
            assertFailure $
                "Invocation failed for nested self balance test: " ++ show rcrReason
        InvokeContract.Success{..} ->
            case rcrReturnValue of
                Nothing -> assertFailure "Invoking a V1 contract must produce a return value."
                Just rv ->
                    assertEqual
                        "Nested self balance test"
                        (BS.unpack (runPut . mapM_ putWord64le $ [133, 133]))
                        (BS.unpack rv)

-- Self balance test in case of nested calls.
-- This tests that the balance is correctly reported in case of re-entrancy.
runNestedSelfBalanceTests :: Assertion
runNestedSelfBalanceTests = do
    Helpers.runTestBlockState $ do
        initState <- thawBlockState =<< initialBlockState
        (mod1, bsWithMod) <- deployModule2 initState
        -- we will invoke the second contract with the address of the first.
        (addr2, mutStateWithBothContracts) <- initContract2 123 bsWithMod mod1
        stateWithBothContracts <- freezeBlockState mutStateWithBothContracts
        invokeNestedSelfBalanceTest addr2 stateWithBothContracts >>= checkNestedSelfBalanceTest

tests :: Spec
tests = describe "V1: Self balance" $ do
    specify "Self balance contract" runSelfBalanceTests
    specify "Nested self balance contract" runNestedSelfBalanceTests
