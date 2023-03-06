{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests calling a contract from a contract and inspecting the return
--    message. Concretely it invokes a counter contract that maintains a 64-bit
--    counter in its state.
module SchedulerTests.SmartContracts.V1.Counter (tests) where

import Control.Monad
import qualified Data.ByteString.Short as BSS
import Data.Serialize (putByteString, putWord16le, putWord64le, runPut)
import Data.Word (Word64)
import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec

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
counterSourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/call-counter.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

test1 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
test1 spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": Counter updates and returns.") $
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
                    { payload = DeployModule wasmModVersion counterSourceFile,
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
                    { payload = InitContract 0 wasmModVersion counterSourceFile "init_counter" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
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
                    { payload = Update 0 (Types.ContractAddress 0 0) "counter.inc" BSS.empty,
                      metadata = makeDummyHeader accountAddress0 3 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doAssertState <- assertCounterState 1 state
                return $ do
                    Helpers.assertSuccess result
                    doAssertState
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "counter.inc" BSS.empty,
                      metadata = makeDummyHeader accountAddress0 4 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doAssertState <- assertCounterState 2 state
                return $ do
                    Helpers.assertSuccess result
                    doAssertState
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "counter.inc10" callArgs,
                      metadata = makeDummyHeader accountAddress0 5 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doAssertState <- assertCounterState 12 state
                return $ do
                    Helpers.assertSuccess result
                    doAssertState
            }
        ]
    callArgs = BSS.toShort $ runPut $ do
        putWord64le 0 -- contract index
        putWord64le 0 -- contract subindex
        putWord16le 0 -- length of parameter
        putWord16le (fromIntegral (BSS.length "inc"))
        putByteString "inc" -- entrypoint name
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
                value <- StateV1.lookupKey contractState (runPut (putWord64le 0))
                return $
                    do
                        case value of
                            Nothing -> assertFailure "Failed to find key [0,0,0,0,0,0,0,0]"
                            Just stateContents ->
                                assertEqual
                                    ("State contains " ++ show expectedCount ++ ".")
                                    (runPut (putWord64le expectedCount))
                                    stateContents

tests :: Spec
tests =
    describe "V1: Counter counts." $
        sequence_ $
            Helpers.forEveryProtocolVersion test1
