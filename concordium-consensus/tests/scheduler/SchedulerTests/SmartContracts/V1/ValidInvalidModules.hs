{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-| This module tests validating modules of various kinds to test that
    they may or may not be deployed.
-}
module SchedulerTests.SmartContracts.V1.ValidInvalidModules (tests) where

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
  ws <- BS.readFile "./testdata/contracts/v1/extra-exports.wasm"
  let wm1 = WasmModuleV (ModuleSource ws)
  case WasmV1.processModule wm1 of
    Nothing -> assertFailure "Invalid caller module."
    Just GSWasm.ModuleInterface{..} -> do
      assertEqual "Only valid init functions should be exposed" (Set.singleton (InitName "init_contract")) miExposedInit
      let expectedReceive = Map.singleton (InitName "init_contract") (Set.singleton (ReceiveName "contract.call"))
      assertEqual "Only valid receive functions should be exposed" expectedReceive miExposedReceive
  let wm0 = WasmModuleV (ModuleSource ws)
  case WasmV0.processModule wm0 of
    Nothing -> return ()
    Just _ -> assertFailure "Extra exports are not allowed for V0 modules."

tests :: Spec
tests = describe "V1: Process modules" $ do
  specify "Extra exports" testModule1
