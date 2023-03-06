{-# LANGUAGE OverloadedStrings #-}

-- | This tests whether policies are passed correctly to the init context of
--    a smart contract.
--    See ../smart-contracts/rust-contracts/example-contracts/context-test for the source code.
module SchedulerTests.InitPoliciesTest (tests) where

import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure)
import Test.Hspec

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as Map
import Data.Maybe

import Concordium.GlobalState.Wasm
import Concordium.ID.Types
import Concordium.Scheduler.WasmIntegration
import Concordium.Wasm

import Concordium.Scheduler.DummyData

import qualified SchedulerTests.Helpers as Helpers

setup :: String -> IO (ModuleInterfaceV V0)
setup errString = do
    source <- BS.readFile "../concordium-base/smart-contracts/testdata/contracts/context_test.wasm"
    let wasmMod = WasmModuleV (ModuleSource source)
    let miface = processModule wasmMod
    assertBool ("Module not valid " ++ errString) (isJust miface)
    return (fromJust miface)

accountAddress0 :: AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

testNoAttributes :: Assertion
testNoAttributes = do
    iface <- setup "testNoAttributes"
    let artifact = miModule iface
    let initCtx1 =
            InitContext
                { initOrigin = accountAddress0,
                  icSenderPolicies = []
                }
    let res1 = applyInitFun artifact dummyChainMeta initCtx1 (InitName "init_context_test") (Parameter mempty) False 0 100000
    case res1 of
        Nothing -> assertFailure "Initialization failed due to out of energy."
        Just (Left execFailure, _) -> assertFailure $ "Initalizatio failed due to " ++ show execFailure
        Just (Right SuccessfulResultData{..}, _) -> do
            assertEqual "No logs should be produced." [] logs
            assertEqual "State should be the singleton 1" (ContractState (BS.singleton 1)) newState

    let initCtx2 =
            InitContext
                { initOrigin = accountAddress0,
                  icSenderPolicies =
                    [ SenderPolicy
                        { spIdentityProvider = IP_ID 17,
                          spCreatedAt = 0,
                          spValidTo = 0,
                          spItems = []
                        }
                    ]
                }
    let res2 = applyInitFun artifact dummyChainMeta initCtx2 (InitName "init_context_test") (Parameter mempty) False 0 100000
    case res2 of
        Nothing -> assertFailure "Initialization failed due to out of energy."
        Just (Left execFailure, _) -> assertFailure $ "Initalizatio failed due to " ++ show execFailure
        Just (Right SuccessfulResultData{..}, _) -> do
            assertEqual "No logs should be produced." [] logs
            assertEqual "State should be the singleton 0" (ContractState (BS.singleton 0)) newState

testSingleAttribute :: Assertion
testSingleAttribute = do
    iface <- setup "testSingleAttribute"
    let artifact = miModule iface
    let initCtx3 =
            InitContext
                { initOrigin = accountAddress0,
                  icSenderPolicies =
                    [ SenderPolicy
                        { spIdentityProvider = IP_ID 17,
                          spCreatedAt = 0,
                          spValidTo = 0,
                          spItems =
                            [   ( mapping Map.! "countryOfResidence",
                                  AttributeValue (BSS.toShort (BS.pack [1 .. 31]))
                                )
                            ]
                        }
                    ]
                }
    let res3 = applyInitFun artifact dummyChainMeta initCtx3 (InitName "init_context_test_2") (Parameter mempty) False 0 100000
    case res3 of
        Nothing -> assertFailure "Initialization failed due to out of energy."
        Just (Left execFailure, _) -> assertFailure $ "Initalizatio failed due to " ++ show execFailure
        Just (Right SuccessfulResultData{..}, _) -> do
            assertEqual "No logs should be produced." [] logs
            assertEqual "State should be the singleton 0" (ContractState (BS.singleton 0)) newState

testTwoPoliciesTwoAttributes :: Assertion
testTwoPoliciesTwoAttributes = do
    iface <- setup "testTwoPoliciesTwoAttributes"
    let artifact = miModule iface
    let initCtx4 =
            InitContext
                { initOrigin = accountAddress0,
                  icSenderPolicies =
                    [ SenderPolicy
                        { spIdentityProvider = IP_ID 17,
                          spCreatedAt = 123,
                          spValidTo = 123,
                          spItems =
                            [   ( mapping Map.! "countryOfResidence",
                                  AttributeValue (BSS.toShort (BS.pack [1 .. 31]))
                                )
                            ]
                        },
                      SenderPolicy
                        { spIdentityProvider = IP_ID 25,
                          spCreatedAt = 456,
                          spValidTo = 456 + 10,
                          spItems =
                            [   ( mapping Map.! "countryOfResidence",
                                  AttributeValue (BSS.toShort (BS.pack [1 .. 31]))
                                ),
                                ( mapping Map.! "dob",
                                  AttributeValue (BSS.toShort (BS.pack (replicate 13 17)))
                                )
                            ]
                        }
                    ]
                }
    let res4 = applyInitFun artifact dummyChainMeta initCtx4 (InitName "init_context_test_3") (Parameter mempty) False 0 1000000
    case res4 of
        Nothing -> assertFailure "Initialization failed due to out of energy."
        Just (Left execFailure, _) -> assertFailure $ "Initalizatio failed due to " ++ show execFailure
        Just (Right SuccessfulResultData{..}, _) -> do
            assertEqual "No logs should be produced." [] logs
            assertEqual "State should be the singleton 0" (ContractState (BS.singleton 0)) newState

tests :: Spec
tests = describe "Init policies test." $ do
    specify "Test with no attributes" testNoAttributes
    specify "Test with single attribute" testSingleAttribute
    specify "Test with two policies and attributes" testTwoPoliciesTwoAttributes
