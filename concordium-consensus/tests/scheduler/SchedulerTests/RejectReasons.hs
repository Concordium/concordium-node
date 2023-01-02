{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.RejectReasons (tests) where

import Data.Maybe (catMaybes)
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm (WasmVersion (..))
import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests =
    describe "Testing error codes in rejected smart contracts." $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString ->
                testRejectReasons spv pvString

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 1_000_000_000 0]

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

wasmPath :: String
wasmPath = "./testdata/contracts/reject-reasons.wasm"

transactionInputs :: [TransactionJSON]
transactionInputs =
    [ TJSON
        { metadata = makeDummyHeader accountAddress0 1 100000,
          payload = DeployModule V0 wasmPath,
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress0 2 100000,
          payload = InitContract 0 V0 wasmPath "init_success" "",
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress0 3 100000,
          payload = InitContract 0 V0 wasmPath "init_error_pos" "",
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress0 4 100000,
          payload = InitContract 0 V0 wasmPath "init_fail_minus2" "",
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress0 5 100000,
          payload = InitContract 0 V0 wasmPath "init_fail_big" "",
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress0 6 100000,
          payload = Update 0 (Types.ContractAddress 0 0) "success.receive_error_no_action" "",
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress0 7 100000,
          payload = Update 0 (Types.ContractAddress 0 0) "success.receive_success" "",
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress0 8 100000,
          payload = Update 0 (Types.ContractAddress 0 0) "success.receive_error_pos" "",
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress0 9 100000,
          payload = Update 0 (Types.ContractAddress 0 0) "success.receive_fail_minus5" "",
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress0 10 100000,
          payload = Update 0 (Types.ContractAddress 0 0) "success.receive_fail_big" "",
          keys = [(0, [(0, keyPair0)])]
        }
    ]

testRejectReasons ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
testRejectReasons _ pvString =
    specify (pvString ++ ": Processing reject-reasons.wasm smart contract") $ do
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                transactionInputs
        let Sch.FilteredTransactions{..} = Helpers.srTransactions result
        let results = Helpers.getResults ftAdded
        assertEqual "There should be 10 successful transactions." 10 (length results)
        assertEqual "There should be no failed transactions." [] ftFailed
        let runtimeFailures =
                filter
                    ( \case
                        (_, Types.TxReject{vrRejectReason = Types.RuntimeFailure}) -> True
                        _ -> False
                    )
                    results

        assertEqual
            "There should be 3 runtime failures (from using positive return codes)."
            3
            (length runtimeFailures)
        let rejects =
                map
                    ( \case
                        ( _,
                          Types.TxReject
                            { vrRejectReason =
                                Types.RejectedInit{Types.rejectReason = reason}
                            }
                            ) -> Just reason
                        ( _,
                          Types.TxReject
                            { vrRejectReason =
                                Types.RejectedReceive{Types.rejectReason = reason}
                            }
                            ) -> Just reason
                        _ -> Nothing
                    )
                    results
        assertEqual
            "There should be 2 rejected init and 2 rejected update transactions."
            [-2, -2147483648, -5, -2147483648]
            (catMaybes rejects)
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result state = do
        hashedState <- BS.hashBlockState state
        doInvariantAssertions <-
            Helpers.assertBlockStateInvariants
                hashedState
                (Helpers.srExecutionCosts result)
        instances <- BS.getContractInstanceList hashedState
        return $ do
            doInvariantAssertions
            assertEqual "There should be 1 instance." 1 (length instances)
