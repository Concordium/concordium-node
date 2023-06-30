{-# LANGUAGE OverloadedStrings #-}

-- | This module tests that a module containing all the new V1 host functions is
--    accepted. It serves as a basic integration test. Individual functions either
--    have tests in wasm-chain-integration, or as part of other scheduler tests if
--    they require more complex interactions with the chain.
module SchedulerTests.SmartContracts.V1.AllNewHostFunctions (tests) where

import Test.HUnit (Assertion, assertFailure)
import Test.Hspec

import qualified Data.ByteString as BS

import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import Concordium.Wasm

-- |A V1 module with extra exports.
testModule1 :: Assertion
testModule1 = do
    ws <- BS.readFile "../concordium-base/smart-contracts/testdata/contracts/v1/all-new-host-functions.wasm"
    let wm1 = WasmModuleV (ModuleSource ws)
    case WasmV1.processModule Types.SP5 wm1 of
        Nothing -> assertFailure "Invalid caller module."
        Just GSWasm.ModuleInterface{} -> return ()

-- |A V1 module with extra exports.
-- This should not pass the processing as the module
-- contains 'upgrade' and this is only allowed for P5 and
-- onwards.
testModule2 :: Assertion
testModule2 = do
    ws <- BS.readFile "../concordium-base/smart-contracts/testdata/contracts/v1/all-new-host-functions.wasm"
    let wm1 = WasmModuleV (ModuleSource ws)
    case WasmV1.processModule Types.SP4 wm1 of
        Nothing -> return ()
        Just GSWasm.ModuleInterface{} -> assertFailure "Caller module contains 'upgrade' in unsupported PV."

tests :: Spec
tests = describe "V1: Process modules" $ do
    specify "All new host functions" testModule1
    specify "Host fn 'upgrade' in < P5 fails" testModule2
