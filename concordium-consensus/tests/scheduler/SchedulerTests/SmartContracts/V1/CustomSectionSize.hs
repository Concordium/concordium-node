{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module tests that in P6 the custom section size does not count towards
-- execution cost.
module SchedulerTests.SmartContracts.V1.CustomSectionSize (tests) where

import Control.Monad (when)
import Test.HUnit (Assertion, assertBool, assertEqual)
import Test.Hspec

import qualified Concordium.Types as Types

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.InvokeContract as InvokeContract
import qualified Concordium.Types.InvokeContract as InvokeContract
import Concordium.Wasm

import qualified SchedulerTests.Helpers as Helpers
import qualified SchedulerTests.SmartContracts.V1.InvokeHelpers as InvokeHelpers

-- empty state, no accounts, no modules, no instances
initialBlockState :: Types.IsProtocolVersion pv => Helpers.PersistentBSM pv (HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 1_000 0]

sourceFile :: FilePath
sourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading_1.wasm"

sourceFileWithCustomSections :: FilePath
sourceFileWithCustomSections = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading_1_with_custom_section.wasm"

deployModule1 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    PersistentBlockState pv ->
    Helpers.PersistentBSM
        pv
        ( (InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1),
          PersistentBlockState pv
        )
deployModule1 = InvokeHelpers.deployModuleV1 (Types.protocolVersion @pv) sourceFile

-- Same as module 1, but with two custom sections.
deployModule2 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    PersistentBlockState pv ->
    Helpers.PersistentBSM
        pv
        ( (InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1),
          PersistentBlockState pv
        )
deployModule2 = InvokeHelpers.deployModuleV1 (Types.protocolVersion @pv) sourceFileWithCustomSections

-- Initialize contract a.
initContract ::
    Types.IsProtocolVersion pv =>
    PersistentBlockState pv ->
    (InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1) ->
    Helpers.PersistentBSM pv (Types.ContractAddress, PersistentBlockState pv)
initContract =
    InvokeHelpers.initContractV1
        (Helpers.accountAddressFromSeed 0)
        (InitName "init_a")
        emptyParameter
        0

-- |Invoke the entrypoint @newfun@ on contract @a@.
invokeContract ::
    Types.IsProtocolVersion pv =>
    Types.ContractAddress ->
    HashedPersistentBlockState pv ->
    Helpers.PersistentBSM pv InvokeContract.InvokeContractResult
invokeContract ccContract bs = do
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 0,
                  ccMethod = ReceiveName "a.newfun",
                  ccEnergy = 1_000_000_000,
                  ccParameter = emptyParameter,
                  ..
                }
    InvokeContract.invokeContract ctx (Types.ChainMetadata 123) bs

-- Self balance test in case of nested calls.
-- This tests that the balance is correctly reported in case of re-entrancy.
runTests :: forall pv. Types.IsProtocolVersion pv => Types.SProtocolVersion pv -> String -> Assertion
runTests spv pvString = when (Types.demoteProtocolVersion spv >= Types.P4) $ do
    cost1 <- Helpers.runTestBlockState @pv $ do
        initState <- thawBlockState =<< initialBlockState
        (mod1, bsWithMod) <- deployModule1 initState
        (addr2, mutStateWithBothContracts) <- initContract bsWithMod mod1
        stateWithBothContracts <- freezeBlockState mutStateWithBothContracts
        InvokeContract.rcrUsedEnergy <$> invokeContract addr2 stateWithBothContracts
    cost2 <- Helpers.runTestBlockState @pv $ do
        initState <- thawBlockState =<< initialBlockState
        (mod1, bsWithMod) <- deployModule2 initState
        (addr2, mutStateWithBothContracts) <- initContract bsWithMod mod1
        stateWithBothContracts <- freezeBlockState mutStateWithBothContracts
        InvokeContract.rcrUsedEnergy <$> invokeContract addr2 stateWithBothContracts
    if Types.omitCustomSectionFromSize (Types.protocolVersion @pv)
        then assertEqual (pvString ++ ": The costs of execution should be the same.") cost1 cost2
        else assertBool (pvString ++ ": The cost with custom section should be higher: " ++ show cost2 ++ " > " ++ show cost1) (cost2 > cost1)

tests :: Spec
tests = describe "V1: Custom section counts or not" $ do
    specify "Deploy and invoke instance" $
        sequence_ $
            Helpers.forEveryProtocolVersion runTests
