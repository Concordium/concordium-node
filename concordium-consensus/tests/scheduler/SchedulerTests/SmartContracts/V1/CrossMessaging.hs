{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests calling a V0 contract from a V1 contract and sending a message from a V0 to V1 contract.
module SchedulerTests.SmartContracts.V1.CrossMessaging (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.Serialize (putByteString, putWord16le, putWord64le, runPut)
import Data.Word (Word64)

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.ID.Types as ID
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm
import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests =
    describe "V1: Counter with cross-messaging." $
        sequence_ $
            Helpers.forEveryProtocolVersion test1

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 100_000_000 0]

accountAddress0 :: ID.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

counterSourceFile :: FilePath
counterSourceFile = "./testdata/contracts/v1/call-counter.wasm"

version1 :: WasmVersion
version1 = V1

proxySourceFile :: FilePath
proxySourceFile = "./testdata/contracts/v1/send-message-v1.wasm"

version0 :: WasmVersion
version0 = V0

-- This test sets up two contracts. The counter contract on address 0,0 and the
-- proxy contract on address 1,0. Then it invokes a single method on the counter
-- contract. That method calls the forward method on the proxy contract which
-- forwards the call to the inc method of the counter contract, which finally
-- increments the counter.
test1 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
test1 spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": CrossMessaging via a proxy") $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions :: [Helpers.TransactionAndAssertion pv]
    transactionsAndAssertions =
        [ Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule version1 counterSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 counterSourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = DeployModule version0 proxySourceFile,
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyDeploymentV1 proxySourceFile result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 version1 counterSourceFile "init_counter" "",
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doAssertState <- assertCounterState 0 state
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        counterSourceFile
                        (InitName "init_counter")
                        (Parameter "")
                        Nothing
                        result
                    doAssertState
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 version0 proxySourceFile "init_proxy" "",
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ do
                    Helpers.assertSuccess result
                    Helpers.assertUsedEnergyInitialization
                        proxySourceFile
                        (InitName "init_proxy")
                        (Parameter "")
                        Nothing
                        result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "counter.inc10nocheck" callArgs,
                      metadata = makeDummyHeader accountAddress0 5 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doAssertState <- assertCounterState 10 state
                return $ do
                    Helpers.assertSuccess result
                    doAssertState
            }
        ]
    forwardParameter = runPut $ do
        putWord64le 0 -- index of the counter
        putWord64le 0 -- subindex of the counter contract
        putWord16le (fromIntegral (BSS.length "counter.inc"))
        putByteString "counter.inc" -- receive name, actions for V0 contracts must still use the full name
        putWord16le 0 -- length of parameter
    callArgs = BSS.toShort $ runPut $ do
        putWord64le 1 -- contract index (the proxy contract)
        putWord64le 0 -- contract subindex
        putWord16le (fromIntegral (BS.length forwardParameter)) -- length of parameter
        putByteString forwardParameter
        putWord16le (fromIntegral (BSS.length "forward"))
        putByteString "forward" -- entrypoint name, calls for V1 contracts use just the entrypoint name
        putWord64le 0 -- amount

    -- Check that the contract state contains n.
    assertCounterState ::
        Word64 ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    assertCounterState expectedCount blockState = do
        BS.bsoGetInstance blockState (Types.ContractAddress 0 0) >>= \case
            Nothing -> return $ assertFailure "Missing instance."
            Just (BS.InstanceInfoV0 _) -> return $ assertFailure "Expected V1 instance, but got V0."
            Just (BS.InstanceInfoV1 ii) -> do
                contractState <- BS.externalContractState $ BS.iiState ii
                -- the contract stores the state at key = [0u8; 8]
                maybeValue <- StateV1.lookupKey contractState (runPut (putWord64le 0))
                return $ case maybeValue of
                    Nothing -> assertFailure "Failed to find key [0,0,0,0,0,0,0,0]"
                    Just stateContents ->
                        assertEqual
                            ("State contains " ++ show expectedCount ++ ".")
                            (runPut (putWord64le expectedCount))
                            stateContents
