{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests making a transfer from a contract to an account.
module SchedulerTests.SmartContracts.V1.Transfer (tests) where

import Control.Monad
import qualified Data.ByteString.Short as BSS
import Data.Serialize (encode, putWord64le, runPut)
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.ID.Types as ID
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm
import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests =
    describe "V1: Transfer from contract to account." $
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

transferSourceFile :: FilePath
transferSourceFile = "./testdata/contracts/v1/transfer.wasm"

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
        specify (pvString ++ ": Transfer from V1 contract to account.") $
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
                    { payload = DeployModule wasmModVersion transferSourceFile,
                      metadata = makeDummyHeader accountAddress0 1 100000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = InitContract 0 wasmModVersion transferSourceFile "init_transfer" "",
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doStateAssertion <- transferSpec state
                return $ do
                    Helpers.assertSuccess result
                    doStateAssertion
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 123 (Types.ContractAddress 0 0) "transfer.forward" (BSS.toShort (encode accountAddress0)),
                      metadata = makeDummyHeader accountAddress0 3 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doStateAssertion <- transferSpec state
                return $ do
                    Helpers.assertSuccess result
                    doStateAssertion
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 1_000 (Types.ContractAddress 0 0) "transfer.deposit" "",
                      metadata = makeDummyHeader accountAddress0 4 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ -> do
                return $ Helpers.assertSuccess result
            },
          Helpers.TransactionAndAssertion
            { taaTransaction =
                TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "transfer.send" sendParameter,
                      metadata = makeDummyHeader accountAddress0 5 700_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result state -> do
                doStateAssertion <- sendSpec state
                return $ do
                    Helpers.assertSuccessWithEvents
                        [ Types.Interrupted (Types.ContractAddress 0 0) [],
                          Types.Transferred
                            (Types.AddressContract (Types.ContractAddress 0 0))
                            17
                            (Types.AddressAccount accountAddress0),
                          Types.Resumed (Types.ContractAddress 0 0) True,
                          Types.Updated
                            (Types.ContractAddress 0 0)
                            (Types.AddressAccount accountAddress0)
                            0
                            (Parameter sendParameter)
                            (ReceiveName "transfer.send")
                            V1
                            []
                        ]
                        result
                    doStateAssertion
            }
        ]
    sendParameter = BSS.toShort (encode accountAddress0 <> runPut (putWord64le 17))
    -- Check that the contract has the initial amount 0 microCCD on its account.
    transferSpec :: BS.PersistentBlockState pv -> Helpers.PersistentBSM pv Assertion
    transferSpec blockState = do
        maybeInstance <- BS.bsoGetInstance blockState (Types.ContractAddress 0 0)
        return $ case maybeInstance of
            Nothing -> assertFailure "Instance at <0,0> does not exist."
            Just (BS.InstanceInfoV0 _) -> assertFailure "Expected V1 instance, but got V0."
            Just (BS.InstanceInfoV1 ii) -> do
                assertEqual "Contract has 0 CCD." (Types.Amount 0) (BS.iiBalance ii)

    -- Check that the contract has the deposited amount (1000) minus 17 microCCD on its account.
    sendSpec :: BS.PersistentBlockState pv -> Helpers.PersistentBSM pv Assertion
    sendSpec blockState = do
        maybeInstance <- BS.bsoGetInstance blockState (Types.ContractAddress 0 0)
        return $ case maybeInstance of
            Nothing -> assertFailure "Instance at <0,0> does not exist."
            Just (BS.InstanceInfoV0 _) -> assertFailure "Expected V1 instance, but got V0."
            Just (BS.InstanceInfoV1 ii) -> do
                assertEqual "Contract has 983 CCD." (Types.Amount (1000 - 17)) (BS.iiBalance ii)
