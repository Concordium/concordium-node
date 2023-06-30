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

-- |This module is the same as upgrading_1.wasm, but has a number of custom
-- sections added at the end to make the Wasm file 100B bigger. This leads to
-- different costs when executing contracts based on it (specificallly 2NRG more
-- when invoking an instance).
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

-- |Deploy and initialize two contracts, then invoke entrypoint newfun in them,
-- and return how much it cost. In the first instance the module is without
-- custom sections, in the second it is with custom sections.
runTests :: forall pv. Types.IsProtocolVersion pv => Types.SProtocolVersion pv -> String -> Assertion
runTests spv pvString = when (Types.demoteProtocolVersion spv >= Types.P4) $ do
    (size1, cost1) <- Helpers.runTestBlockState @pv $ do
        initState <- thawBlockState =<< initialBlockState
        (mod1, bsWithMod) <- deployModule1 initState
        let size1 = GSWasm.miModuleSize (fst mod1)
        (addr1, mutStateWithBothContracts) <- initContract bsWithMod mod1
        stateWithBothContracts <- freezeBlockState mutStateWithBothContracts
        energy1 <- InvokeContract.rcrUsedEnergy <$> invokeContract addr1 stateWithBothContracts
        return (size1, energy1)
    (size2, cost2) <- Helpers.runTestBlockState @pv $ do
        initState <- thawBlockState =<< initialBlockState
        (mod2, bsWithMod) <- deployModule2 initState
        let size2 = GSWasm.miModuleSize (fst mod2)
        (addr2, mutStateWithBothContracts) <- initContract bsWithMod mod2
        stateWithBothContracts <- freezeBlockState mutStateWithBothContracts
        energy2 <- InvokeContract.rcrUsedEnergy <$> invokeContract addr2 stateWithBothContracts
        return (size2, energy2)
    -- In P4 and P5 the custom section is counted towards the size, so the cost
    -- of execution in the second case is higher. In P6 and later the costs must
    -- be the same since the custom section is ignored.
    if Types.omitCustomSectionFromSize (Types.protocolVersion @pv)
        then do
            assertEqual (pvString ++ ": The costs of execution should be the same.") cost1 cost2
            assertEqual (pvString ++ ": The sizes of modules must be the same.") size1 size2
        else do
            assertBool (pvString ++ ": The cost with custom section should be higher: " ++ show cost2 ++ " > " ++ show cost1) (cost2 > cost1)
            assertBool (pvString ++ ": The size with custom section should be higher: " ++ show size2 ++ " > " ++ show size1) (size2 > size1)

tests :: Spec
tests = describe "V1: Custom section counts or not" $ do
    specify "Deploy and invoke instance" $
        sequence_ $
            Helpers.forEveryProtocolVersion runTests
