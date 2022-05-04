{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-| This module tests that a module containing all the new V1 host functions is
    accepted. It serves as a basic integration test. Individual functions either
    have tests in wasm-chain-integration, or as part of other scheduler tests if
    they require more complex interactions with the chain.
-}
module SchedulerTests.SmartContracts.V1.AllNewHostFunctions (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual, Assertion)

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Concordium.Wasm
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import qualified Concordium.Scheduler.WasmIntegration as WasmV0

-- |A V1 module with extra exports.
testModule1 :: Assertion
testModule1 = do
  ws <- BS.readFile "./testdata/contracts/v1/all-new-host-functions.wasm"
  let wm1 = WasmModuleV (ModuleSource ws)
  case WasmV1.processModule wm1 of
    Nothing -> assertFailure "Invalid caller module."
    Just GSWasm.ModuleInterface{} -> return ()

tests :: Spec
tests = describe "V1: Process modules" $ do
  specify "All new host functiosn" testModule1
