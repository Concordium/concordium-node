{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SchedulerTests.ChainMetatest (tests) where

import Test.HUnit
import Test.Hspec

import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.EnvironmentImplementation as EI
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm (WasmVersion (..))

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS

import Concordium.Scheduler.DummyData

import qualified SchedulerTests.Helpers as Helpers

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 1000000000 0]

testConfig :: Helpers.TestConfig
testConfig =
    Helpers.defaultTestConfig
        { Helpers.tcContextState = contextState
        }
  where
    contextState =
        Helpers.defaultContextState
            { -- Set slot time to match the expected slot time hardcoded in the test smart contract.
              EI._chainMetadata = dummyChainMeta{Types.slotTime = 444}
            }

transactionInputs :: [TransactionJSON]
transactionInputs =
    [ TJSON
        { metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 1 100000,
          payload = DeployModule V0 "./testdata/contracts/chain-meta-test.wasm",
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        },
      TJSON
        { metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 2 100000,
          payload = InitContract 9 V0 "./testdata/contracts/chain-meta-test.wasm" "init_check_slot_time" "",
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        }
    ]

testChainMeta ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
testChainMeta _ pvString =
    specify (pvString ++ ": Reading chain metadata. ") $ do
        (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                testConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                transactionInputs
        let Sch.FilteredTransactions{..} = srTransactions
        assertEqual "There should be no failed transactions." [] ftFailed
        assertEqual "There should be no rejected transactions." []
            $ filter
                ( \case
                    (_, Types.TxSuccess{}) -> False
                    (_, Types.TxReject{}) -> True
                )
            $ Helpers.getResults ftAdded
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result state = do
        hashedState <- BS.hashBlockState state
        doInvariantAssertions <- Helpers.assertBlockStateInvariants hashedState (Helpers.srExecutionCosts result)
        instances <- BS.getContractInstanceList hashedState
        return $ do
            doInvariantAssertions
            assertEqual "There should be 1 instance." 1 (length instances)

tests :: Spec
tests =
    describe "Chain metadata in transactions:" $
        sequence_ $
            Helpers.forEveryProtocolVersion testChainMeta
