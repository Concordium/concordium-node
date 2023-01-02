{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.RejectReasonsRustContract (tests) where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm (ReceiveName (..), WasmVersion (..))
import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests =
    describe "Testing error codes in Rust smart contracts." $
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
wasmPath = "./testdata/contracts/error_code/target/concordium/wasm32-unknown-unknown/release/error_code.wasm"

transaction :: PayloadJSON -> Types.Nonce -> TransactionJSON
transaction payload n =
    TJSON
        { metadata = makeDummyHeader accountAddress0 n 100_000,
          payload = payload,
          keys = [(0, [(0, keyPair0)])]
        }

initWithAmount :: Types.Amount -> Types.Nonce -> TransactionJSON
initWithAmount amount = transaction (InitContract amount V0 wasmPath "init_error_codes" "")

updateWithAmount :: Types.Amount -> Text -> Types.Nonce -> TransactionJSON
updateWithAmount amount fun = transaction (Update amount firstAddress fun "")

firstAddress :: Types.ContractAddress
firstAddress = Types.ContractAddress 0 0

transactionInputs :: [TransactionJSON]
transactionInputs = zipWith ($) transactionFunctionList [1 ..]
  where
    transactionFunctionList =
        [ transaction (DeployModule V0 wasmPath),
          -- returns InitError::VeryBadError
          -- error code: -1
          initWithAmount 1,
          -- returns ParseError::default() which gets converted into InitError::ParseErrorWrapper
          -- error code: -1
          initWithAmount 2,
          -- returns InitError::ParseErrorWrapper
          -- error code: -1
          initWithAmount 3,
          -- returns SpecialInitError::VerySpecialError which gets also converted into InitError::ParseErrorWrapper
          -- error code: -1
          initWithAmount 4,
          -- returns MostSpecialInitError::SuperSpecialError which gets converted into InitError::SomeOtherError
          -- error code: -1
          initWithAmount 5,
          -- returns MostSpecialInitError::TheAmazingError which gets also converted into InitError::SomeOtherError
          -- error code: -1
          initWithAmount 6,
          -- successfully initializes contract
          initWithAmount 7,
          -- Now a similar sequence as above for receive functions:

          -- returns ReceiveError::VeryBadError
          -- error code: -1
          updateWithAmount 1 "error_codes.receive",
          -- returns a ParseError which gets wrapped into a ReceiveError::ParseErrorWrapper
          -- error code: -1
          updateWithAmount 2 "error_codes.receive",
          -- directly returns a ReceiveError::ParseErrorWrapper
          -- error code: -1
          updateWithAmount 3 "error_codes.receive",
          -- returns a SpecialReceiveError::VerySpecialError which gets wrapped into a ReceiveError::ParseError
          -- error code: -1
          updateWithAmount 4 "error_codes.receive",
          -- returns a ParseError
          -- error code: i32::MIN + 2
          updateWithAmount 0 "error_codes.receive2",
          -- returns Err(()) (Unit)
          -- error code: i32::MIN + 1
          updateWithAmount 0 "error_codes.receive3",
          -- successfully initializes a second contract
          initWithAmount 7,
          -- returns a ParseError from <0, 0>
          -- error code: i32::MIN + 2
          transaction (Update 0 (Types.ContractAddress 1 0) "error_codes.receive_send" "")
        ]

testRejectReasons ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
testRejectReasons _ pvString =
    specify (pvString ++ ": Processing error_codes.wasm smart contract") $ do
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                transactionInputs
        let Sch.FilteredTransactions{..} = Helpers.srTransactions result
        let results = Helpers.getResults ftAdded
        assertEqual "There should be 16 successful transactions." 16 (length results)
        assertEqual "There should be no failed transactions." [] ftFailed
        let runtimeFailures =
                filter
                    ( \case
                        (_, Types.TxReject{vrRejectReason = Types.RuntimeFailure}) -> True
                        _ -> False
                    )
                    results

        assertEqual
            "There should be no runtime failures."
            0
            (length runtimeFailures)
        let rejects =
                map
                    ( \case
                        ( _,
                          Types.TxReject
                            { vrRejectReason =
                                Types.RejectedInit{Types.rejectReason = reason}
                            }
                            ) -> Just (reason, Nothing)
                        ( _,
                          Types.TxReject
                            { vrRejectReason =
                                Types.RejectedReceive
                                    { Types.rejectReason = reason,
                                      Types.receiveName = name,
                                      Types.contractAddress = ca
                                    }
                            }
                            ) -> Just (reason, Just (name, ca))
                        _ -> Nothing
                    )
                    results

        assertEqual
            "There should be 6 rejected init and 7 rejected update transactions."
            (rejectedInitsExpected ++ rejectedReceivesExpected)
            (catMaybes rejects)

        doBlockStateAssertions
  where
    rejectedInitsExpected = (,Nothing) <$> [-1, -2, -2, -2, -3, -3]

    rejectedReceivesExpected =
        [ (-1, Just (ReceiveName{receiveName = "error_codes.receive"}, firstAddress)),
          (-2, Just (ReceiveName{receiveName = "error_codes.receive"}, firstAddress)),
          (-2, Just (ReceiveName{receiveName = "error_codes.receive"}, firstAddress)),
          (-2, Just (ReceiveName{receiveName = "error_codes.receive"}, firstAddress)),
          (minBound + 2, Just (ReceiveName{receiveName = "error_codes.receive2"}, firstAddress)),
          (minBound + 1, Just (ReceiveName{receiveName = "error_codes.receive3"}, firstAddress)),
          (minBound + 2, Just (ReceiveName{receiveName = "error_codes.receive2"}, firstAddress))
        ]

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
            assertEqual "There should be 2 instance." 2 (length instances)
