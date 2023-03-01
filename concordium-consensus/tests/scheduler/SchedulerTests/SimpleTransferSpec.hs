{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- Testing the SimpleTransfer transaction.
NOTE: See also 'SchedulerTests.SimpleTransfersTest'.
-}
module SchedulerTests.SimpleTransferSpec (tests) where

import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types.ProtocolVersion
import Concordium.Wasm
import qualified SchedulerTests.Helpers as Helpers

initialBlockState ::
    (IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed 1_000_000_000 0,
          Helpers.makeTestAccountFromSeed 1_000_000_000 1
        ]

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

transactionInputs :: [TransactionJSON]
transactionInputs =
    [ TJSON
        { payload = DeployModule V0 "../concordium-base/smart-contracts/testdata/contracts/send-tokens-test.wasm",
          metadata = makeDummyHeader accountAddress0 1 100_000,
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { payload = InitContract 0 V0 "../concordium-base/smart-contracts/testdata/contracts/send-tokens-test.wasm" "init_send" "",
          metadata = makeDummyHeader accountAddress0 2 100_000,
          keys = [(0, [(0, keyPair0)])]
        },
      TJSON
        { payload = Update 11 (Types.ContractAddress 0 0) "send.receive" "",
          metadata = makeDummyHeader accountAddress0 3 70_000,
          keys = [(0, [(0, keyPair0)])]
        }
    ]

testCase0 ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
testCase0 _ pvString = specify
    (pvString ++ ": Transfers from a contract to accounts.")
    $ do
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                transactionInputs
        let Sch.FilteredTransactions{..} = Helpers.srTransactions result
        let results = Helpers.getResults ftAdded
        assertEqual "There should be no failed transactions." [] ftFailed
        assertEqual "There should be no rejected transactions." [] $
            filter
                ( \case
                    (_, Types.TxSuccess{}) -> False
                    (_, Types.TxReject{}) -> True
                )
                results
        assertEqual "There should be 3 successful transactions." 3 (length results)
        case results !! 2 of
            (_, Types.TxSuccess events) ->
                assertEqual
                    "Produces events as expected"
                    [ Types.Updated
                        { euAddress = Types.ContractAddress 0 0,
                          euInstigator = Types.AddressAccount accountAddress0,
                          euAmount = 11,
                          euMessage = Parameter "",
                          euReceiveName = ReceiveName "send.receive",
                          euContractVersion = V0,
                          euEvents = []
                        },
                      Types.Transferred
                        { etFrom = Types.AddressContract (Types.ContractAddress 0 0),
                          etAmount = 11,
                          etTo = Types.AddressAccount accountAddress0
                        }
                    ]
                    events
            _ -> assertFailure "Unexpected event produced"
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result state =
        -- NOTE: Could also check resulting balances on each affected account or contract, but
        -- the block state invariant at least tests that the total amount is preserved.
        Helpers.assertBlockStateInvariantsH state (Helpers.srExecutionCosts result)

tests :: Spec
tests =
    describe "SimpleTransfer from contract to account." $
        sequence_ $
            Helpers.forEveryProtocolVersion testCase0
