{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests basic V1 state operations with the recorder contract.
module SchedulerTests.SmartContracts.V1.Recorder (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec

import Control.Monad
import qualified Data.ByteString.Short as BSS
import Data.Serialize (putWord64le, runPut)
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
    describe "V1: Record 20 + 40 strings." $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                testCase spv pvString

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

recorderSourceFile :: FilePath
recorderSourceFile = "./testdata/contracts/v1/record-parameters.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

testCase ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
testCase spv pvString =
    when (Types.supportsV1Contracts spv) $
        specify (pvString ++ ": Record data in a contract.") $
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
                    { payload = DeployModule wasmModVersion recorderSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 wasmModVersion recorderSourceFile "init_recorder" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doStateAssertion <- recorderSpec 0 state
                return $ do
                    Helpers.assertSuccess result
                    doStateAssertion
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload =
                        Update
                            0
                            (Types.ContractAddress 0 0)
                            "recorder.record_u64"
                            (BSS.toShort (runPut (putWord64le 20))),
                      metadata = makeDummyHeader accountAddress0 3 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doStateAssertion <- recorderSpec 20 state
                return $ do
                    Helpers.assertSuccess result
                    doStateAssertion
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload =
                        Update
                            0
                            (Types.ContractAddress 0 0)
                            "recorder.record_u64"
                            (BSS.toShort (runPut (putWord64le 40))),
                      metadata = makeDummyHeader accountAddress0 4 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doStateAssertion <- recorderSpec 60 state
                return $ do
                    Helpers.assertSuccess result
                    doStateAssertion
            }
        ]
    recorderSpec :: Word64 -> BS.PersistentBlockState pv -> Helpers.PersistentBSM pv Assertion
    recorderSpec n blockState = do
        maybeInstance <- BS.bsoGetInstance blockState (Types.ContractAddress 0 0)
        case maybeInstance of
            Nothing -> return $ assertFailure "Instance at <0,0> does not exist."
            Just (BS.InstanceInfoV0 _) -> return $ assertFailure "Expected V1 instance, but got V0."
            Just (BS.InstanceInfoV1 ii) -> do
                contractState <- BS.externalContractState $ BS.iiState ii
                -- since we inserted 60 values we expect to find keys on all those indices
                doAssertInserted <- forM [1 .. n] $ \idx -> do
                    maybeValue <- StateV1.lookupKey contractState (runPut (putWord64le (idx - 1)))
                    return $ case maybeValue of
                        Nothing -> assertFailure $ "Failed to find key " ++ show (idx - 1)
                        Just _ -> return ()
                doAssertRemoved <- do
                    maybeValue <- StateV1.lookupKey contractState (runPut (putWord64le n))
                    return $ case maybeValue of
                        Nothing -> return ()
                        Just _ -> assertFailure $ "Found key " ++ show n ++ ", but did not expect to."
                return $ do
                    assertEqual "Contract has 0 CCD." (Types.Amount 0) (BS.iiBalance ii)
                    sequence_ doAssertInserted
                    doAssertRemoved
