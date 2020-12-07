{-# LANGUAGE OverloadedStrings #-}
{-| This tests whether policies are passed correctly to the init context of
    a smart contract.
    See ../smart-contracts/rust-contracts/example-contracts/context-test for the source code.
-}
module SchedulerTests.InitPoliciesTest where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual, assertBool, Assertion)

import qualified Data.ByteString as BS
import Data.Maybe

import qualified Concordium.Scheduler.Types as Types

import Concordium.ID.Types
import Concordium.Wasm
import Concordium.Scheduler.WasmIntegration

import Concordium.Types.DummyData

testMany :: Assertion
testMany = do
  source <- BS.readFile "./testdata/contracts/context_test.wasm"
  let wasmMod = WasmModule 0 source
  let miface = processModule wasmMod
  assertBool "Module not valid" (isJust miface)
  let iface = fromJust miface
  let initCtx1 = InitContext{
        initOrigin = alesAccount,
        icSenderPolicies = []
        }
  let res1 = applyInitFun iface Types.dummyChainMeta initCtx1 (InitName "init_context_test") (Parameter mempty) 0 100000
  case res1 of
    Nothing -> assertFailure "Initialization failed due to out of energy."
    Just (Left execFailure, _) -> assertFailure $ "Initalizatio failed due to " ++ show execFailure
    Just (Right SuccessfulResultData{..}, _) -> do
      assertEqual "No logs should be produced." [] logs
      assertEqual "State should be the singleton 1" (ContractState (BS.singleton 1)) newState

  let initCtx2 = InitContext{
        initOrigin = alesAccount,
        icSenderPolicies = [
            SenderPolicy{
                spIdentityProvider = IP_ID 17,
                spCreatedAt = 0,
                spValidTo = 0,
                spItems = []
                }
            ]
        }
  let res2 = applyInitFun iface Types.dummyChainMeta initCtx2 (InitName "init_context_test") (Parameter mempty) 0 100000
  case res2 of
    Nothing -> assertFailure "Initialization failed due to out of energy."
    Just (Left execFailure, _) -> assertFailure $ "Initalizatio failed due to " ++ show execFailure
    Just (Right SuccessfulResultData{..}, _) -> do
      assertEqual "No logs should be produced." [] logs
      assertEqual "State should be the singleton 0" (ContractState (BS.singleton 0)) newState

tests :: Spec
tests = describe "Init policies test." $
  specify "Test simple" testMany
