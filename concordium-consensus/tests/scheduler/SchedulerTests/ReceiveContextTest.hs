{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Test that the receive context of a contract is passed correctly by the scheduler.
module SchedulerTests.ReceiveContextTest (tests) where

import Data.FixedByteString (pack)
import Test.HUnit
import Test.Hspec

import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types.ProtocolVersion
import Concordium.Wasm (WasmVersion (..))

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.ID.Types (AccountAddress (..), accountAddressSize)

import Concordium.Scheduler.DummyData

import qualified SchedulerTests.Helpers as Helpers
import SchedulerTests.TestUtils

tests :: Spec
tests =
    describe "Receive context in transactions." $
        sequence_ $
            Helpers.forEveryProtocolVersion testReceive

-- See the contract in /testdata/contracts/send/src/lib.rs from which the wasm
-- module is derived. The contract calls check that the invoker or sender is the
-- account address consisting of only 2's, and that the owner is an account
-- consisting of only 1's. We set up the state with addresses different from
-- those and use the above-mentioned addresses as alias for the same account.
-- This only applies to protocol P3 and up.
initialBlockState ::
    forall pv.
    (IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccount
            (SigScheme.correspondingVerifyKey keyPair1)
            (sender1 $ protocolVersion @pv)
            1_000_000_000,
          Helpers.makeTestAccount
            (SigScheme.correspondingVerifyKey keyPair2)
            (sender2 $ protocolVersion @pv)
            1_000_000_000
        ]

accountAddress1 :: Types.AccountAddress
accountAddress1 = AccountAddress $ pack $ replicate accountAddressSize 1

accountAddress2 :: Types.AccountAddress
accountAddress2 = AccountAddress $ pack $ replicate accountAddressSize 2

keyPair1 :: SigScheme.KeyPair
keyPair1 = Helpers.keyPairFromSeed 1

keyPair2 :: SigScheme.KeyPair
keyPair2 = Helpers.keyPairFromSeed 2

sender1 :: forall pv. IsProtocolVersion pv => SProtocolVersion pv -> Types.AccountAddress
sender1 spv
    | supportsAccountAliases spv = createAlias accountAddress1 17
    | otherwise = accountAddress1

sender2 :: forall pv. IsProtocolVersion pv => SProtocolVersion pv -> Types.AccountAddress
sender2 spv
    | supportsAccountAliases spv = createAlias accountAddress2 77
    | otherwise = accountAddress2

wasmPath :: String
wasmPath = "./testdata/contracts/send/target/concordium/wasm32-unknown-unknown/release/send.wasm"

transactionInputs :: [TransactionJSON]
transactionInputs =
    [ TJSON
        { metadata = makeDummyHeader accountAddress1 1 100_000,
          payload = DeployModule V0 wasmPath,
          keys = [(0, [(0, keyPair1)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress1 2 100_000,
          payload = InitContract 0 V0 wasmPath "init_c10" "",
          keys = [(0, [(0, keyPair1)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress1 3 100_000,
          payload = InitContract 42 V0 wasmPath "init_c10" "",
          keys = [(0, [(0, keyPair1)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress1 4 100_000,
          payload = InitContract 0 V0 wasmPath "init_c20" "",
          keys = [(0, [(0, keyPair1)])]
        },
      TJSON
        { metadata = makeDummyHeader accountAddress2 1 100_000,
          payload = Update 5 (Types.ContractAddress 2 0) "c20.call_c10" "",
          keys = [(0, [(0, keyPair2)])]
        }
    ]

testReceive :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> String -> SpecWith (Arg Assertion)
testReceive _ pvString =
    specify (pvString ++ ": Passing receive context to contract") $ do
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                transactionInputs
        let Sch.FilteredTransactions{..} = Helpers.srTransactions result
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
            assertEqual "There should be 3 instances." 3 (length instances)
