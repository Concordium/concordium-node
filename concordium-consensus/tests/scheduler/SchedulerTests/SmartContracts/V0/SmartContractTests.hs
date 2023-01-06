{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
--  Functional tests for smart contract host functions as they are invoked from the scheduler.
--  See individual test descriptions for details of what is being tested. The tests in this file rely on hand-written Wasm modules.
--  Their sources can be found in the testdata folder. The following files are relevant for these tests
--
--  /contracts/log-event-tests.wat
--  /contracts/get-parameter-size-tests.wat
--  /contracts/get-parameter-section-tests.wat
--  /contracts/state-size-tests.wat
--  /contracts/load-state-tests.wat
--  /contracts/write-state-tests.wat
--  /contracts/resize-state-tests.wat
--  /contracts/only-in-init-tests.wat
--  /contracts/only-in-receive-tests.wat
--  /contracts/simple-transfer-tests.wat
--  /contracts/send-tests.wat
--  /contracts/action-tree-tests.wat
--  /contracts/memory-tests.wat
--
--  If any of these files are modified they should be compiled to a `.wasm` file using `wat2wasm` before the tests are run.
module SchedulerTests.SmartContracts.V0.SmartContractTests (tests) where

import Control.Monad
import qualified Data.ByteString.Short as BSS
import Data.Either (fromRight)
import Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Test.HUnit hiding (TestCase)
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.ID.Types as ID
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner (PayloadJSON (..), TransactionJSON (..))
import Concordium.Scheduler.Types (Amount, ContractAddress (..), Event, RejectReason (..))
import qualified Concordium.Scheduler.Types as Types
import Concordium.Wasm (WasmVersion (..))
import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests = do
    describe "Smart contract V0 host functions" $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                logEventTestCases spv pvString
                getParameterSizeTestCases spv pvString
                getParameterSectionTestCases spv pvString
                stateSizeTestCases spv pvString
                loadStateTestCases spv pvString
                writeStateTestCases spv pvString
                resizeStateTestCases spv pvString
                onlyInInitTestCases spv pvString
                onlyInReceiveTestCases spv pvString
                simpleTransferTestCases spv pvString
                sendTestCases spv pvString
                actionTreeTestCases spv pvString
                memoryTestCases spv pvString

-- ** Test runners **

-- | Run a number of init tests from a specific file.
runInitTestsFromFile ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    FilePath ->
    [(Text, BSS.ShortByteString, Helpers.SchedulerResult -> Assertion)] ->
    Spec
runInitTestsFromFile _ testCaseDescription testFile testCases =
    describe testCaseDescription $
        forM_ testCases f
  where
    f (testName, initParam, resSpec) =
        specify (Text.unpack testName) $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                [ Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = DeployModule V0 testFile,
                              metadata = makeDummyHeader accountAddress0 1 100_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertSuccess result
                    },
                  Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = InitContract 0 V0 testFile testName initParam,
                              metadata = makeDummyHeader accountAddress0 2 100_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ resSpec result
                    }
                ]

-- | Run a number of receive tests from a specific file.
--   Each test is run in isolation, i.e., with a new state.
--   The file is expected to have an init function named 'init_test'
--   and a number of receive functions, each prefixed with 'test.'.
runReceiveTestsFromFile ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    FilePath ->
    [(Text, BSS.ShortByteString, Helpers.SchedulerResult -> Assertion)] ->
    Spec
runReceiveTestsFromFile _ testCaseDescription testFile testCases =
    describe testCaseDescription $
        forM_ testCases f
  where
    f (testName, rcvParam, resSpec) =
        specify (Text.unpack testName) $
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                [ Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = DeployModule V0 testFile,
                              metadata = makeDummyHeader accountAddress0 1 100_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertSuccess result
                    },
                  Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = InitContract 10_000 V0 testFile "init_test" "",
                              metadata = makeDummyHeader accountAddress0 2 100_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ Helpers.assertSuccess result
                    },
                  Helpers.TransactionAndAssertion
                    { taaTransaction =
                        TJSON
                            { payload = Update 0 (ContractAddress 0 0) ("test." <> testName) rcvParam,
                              metadata = makeDummyHeader accountAddress0 3 100_000,
                              keys = [(0, [(0, keyPair0)])]
                            },
                      taaAssertion = \result _ ->
                        return $ resSpec result
                    }
                ]

-- ** Helper Functions **

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed 100_000_000 0,
          Helpers.makeTestAccountFromSeed 100_000_000 1
        ]

accountAddress0 :: ID.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

accountAddress1 :: ID.AccountAddress
accountAddress1 = Helpers.accountAddressFromSeed 1

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

emptyParam :: BSS.ShortByteString
emptyParam = ""

mkParamOfSize :: Int -> BSS.ShortByteString
mkParamOfSize n = BSS.pack $ replicate n 1

encodedAccountAddress1 :: BSS.ShortByteString
encodedAccountAddress1 = BSS.toShort . S.encode $ accountAddress1

expectCCDTransferred :: Amount -> [Event] -> Expectation
expectCCDTransferred expectedAmnt events = foldr sumTransfers 0 events `shouldBe` expectedAmnt
  where
    sumTransfers e acc = case e of
        Types.Transferred{..} -> acc + etAmount
        _ -> acc

-- ** Tests **

logEventTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
logEventTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": log-event-tests")
        "./testdata/contracts/log-event-tests.wasm"
        [ -- Try to read memory before position 0.
          ("start_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("length_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("start_and_length_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- Try to read after end of memory
          ("start_greater_than_mem__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("length_greater_than_mem__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure)
        ]

getParameterSizeTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
getParameterSizeTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": get-parameter-size-tests")
        "./testdata/contracts/get-parameter-size-tests.wasm"
        [ ("size_is_0__return_0_and_succeed", emptyParam, Helpers.assertSuccess),
          ("size_is_max__return_max_and_succeed", maxSizedParam, Helpers.assertSuccess)
        ]
  where
    -- Value must stay in sync with MAX_PARAMETER_SIZE from 'wasm-chain-integration/src/constants.src'
    maxSizedParam = mkParamOfSize 1024

getParameterSectionTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
getParameterSectionTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": get-parameter-section-tests")
        "./testdata/contracts/get-parameter-section-tests.wasm"
        [ -- Try to read before parameter section begins
          ("offset_negative__fail", param100, Helpers.assertRejectWithReason RuntimeFailure),
          -- the reason this fails with out of energy is because we charge
          -- based on the length of data. "Negative" lengths are interpreted as very large numbers
          -- due to two's complement representation. The "negative" is just a point of view,
          -- we use unsigned integers for lengths all the time.
          ("length_negative__fail", param100, Helpers.assertRejectWithReason OutOfEnergy),
          -- Read whole parameter section
          ("length_equal_to_param_size_100__return_100_and_succeed", param100, Helpers.assertSuccess),
          -- Try to read after parameter section ends
          ("length_greater_than_param_size_100__return_100_and_succeed", param100, Helpers.assertSuccess),
          ("offset_greater_than_param_size_100__fail", param100, Helpers.assertRejectWithReason RuntimeFailure),
          -- Try to write outside bounds of linear memory
          ("write_location_negative__fail", param100, Helpers.assertRejectWithReason RuntimeFailure),
          ("write_location_greater_than_mem__fail", param100, Helpers.assertRejectWithReason RuntimeFailure)
        ]
  where
    param100 = mkParamOfSize 100

stateSizeTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
stateSizeTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": state-size-tests")
        "./testdata/contracts/state-size-tests.wasm"
        [ ("size_is_0__return_0_and_succeed", emptyParam, Helpers.assertSuccess),
          ("size_is_max__return_max_and_succeed", emptyParam, Helpers.assertSuccess)
        ]

loadStateTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
loadStateTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": load-state-tests")
        "./testdata/contracts/load-state-tests.wasm"
        [ -- Positive tests
          ("load_all_of_state__return_state_size_and_succeed", emptyParam, Helpers.assertSuccess),
          ("load_max_sized_state__return_state_size_and_succeed", emptyParam, Helpers.assertSuccess),
          ("length_greater_than_state_size__return_state_size_and_succeed", emptyParam, Helpers.assertSuccess),
          -- Try to read outside of state
          ("offset_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("offset_greater_than_state_size__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- the reason this fails with out of energy is because we charge
          -- based on the length of data. "Negative" lengths are interpreted as very large numbers
          -- due to two's complement representation. The "negative" is just a point of view,
          -- we use unsigned integers for lengths all the time.
          ("length_negative__fail", emptyParam, Helpers.assertRejectWithReason OutOfEnergy),
          -- Try to write outside bounds of linear memory
          ("write_location_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("write_location_greater_than_mem__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure)
        ]

writeStateTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
writeStateTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": write-state-tests")
        "./testdata/contracts/write-state-tests.wasm"
        [ -- Positive tests
          ("write_100_bytes__return_100_and_succeed", emptyParam, Helpers.assertSuccess),
          -- Read before start of memory
          ("read_location_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("length_negative__fail", emptyParam, Helpers.assertRejectWithReason OutOfEnergy),
          -- Read after end of memory
          ("length_greater_than_mem__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("read_location_greater_than_mem__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- Write outside of state bounds
          ("offset_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("offset_greater_than_current_state_size__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure)
        ]

resizeStateTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
resizeStateTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": resize-state-tests")
        "./testdata/contracts/resize-state-tests.wasm"
        -- resize_state returns 0 on failure and 1 on success.
        [ ("new_size_negative__return_zero_and_succeed", emptyParam, Helpers.assertSuccess),
          ("new_size_zero__return_one_and_succeed", emptyParam, Helpers.assertSuccess),
          ("new_size_max__return_one_and_succeed", emptyParam, Helpers.assertSuccess),
          ("new_size_greater_than_max__return_zero_and_succeed", emptyParam, Helpers.assertSuccess)
        ]

onlyInInitTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
onlyInInitTestCases spv pvString = do
    runInitTestsFromFile
        spv
        (pvString ++ ": only-in-init-tests")
        file
        [ ("init_get_init_origin__valid_param__succeed", emptyParam, Helpers.assertSuccess),
          ("init_get_init_origin__write_location_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("init_get_init_origin__write_location_greater_than_mem__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure)
        ]
    runReceiveTestsFromFile
        spv
        (pvString ++ ": only-in-init-tests")
        file
        [("get_init_origin__valid_param_from_receive__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure)]
  where
    file = "./testdata/contracts/only-in-init-tests.wasm"

onlyInReceiveTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
onlyInReceiveTestCases spv pvString = do
    runInitTestsFromFile
        spv
        (pvString ++ ": only-in-receive-tests")
        file
        [ -- Try calling functions from init with valid parameters.
          ("init_get_receive_invoker__valid_param_from_init__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("init_get_receive_sender__valid_param_from_init__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("init_get_receive_self_address__valid_param_from_init__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("init_get_receive_owner__valid_param_from_init__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("init_get_receive_self_balance__valid_param_from_init__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure)
        ]
    runReceiveTestsFromFile
        spv
        (pvString ++ ": only-in-receive-tests")
        file
        [ -- get_receive_invoker
          ("get_receive_invoker__valid_param__succeed", emptyParam, Helpers.assertSuccess),
          ("get_receive_invoker__start_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("get_receive_invoker__start_greater_than_mem__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- get_receive_sender
          ("get_receive_sender__valid_param__succeed", emptyParam, Helpers.assertSuccess),
          ("get_receive_sender__start_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("get_receive_sender__start_greater_than_mem__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- get_receive_self_address
          ("get_receive_self_address__valid_param__succeed", emptyParam, Helpers.assertSuccess),
          ("get_receive_self_address__start_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("get_receive_self_address__start_greater_than_mem__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- get_receive_owner
          ("get_receive_owner__valid_param__succeed", emptyParam, Helpers.assertSuccess),
          ("get_receive_owner__start_negative__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("get_receive_owner__start_greater_than_mem__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- get_receive_self_balance
          ("get_receive_self_balance__send_10K_microGTU__balance_is_10K_and_succeed", emptyParam, Helpers.assertSuccess)
        ]
  where
    file = "./testdata/contracts/only-in-receive-tests.wasm"

simpleTransferTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
simpleTransferTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": simple-transfer-tests")
        "./testdata/contracts/simple-transfer-tests.wasm"
        [ -- Ensure that only the _last_ transfer is processed.
          ("multiple_transfers__transfer_4_and_succeed", encodedAccountAddress1, Helpers.assertSuccessWhere $ expectCCDTransferred 4),
          -- Try reading memory outside of memory bounds.
          ("addr_bytes_negative__fail", encodedAccountAddress1, Helpers.assertRejectWithReason RuntimeFailure),
          ("addr_bytes_greater_than_mem__fail", encodedAccountAddress1, Helpers.assertRejectWithReason RuntimeFailure),
          -- Ensure correct error message for invalid acc ref.
            ( "invalid_addr__fail_with_InvalidAccountReference",
              emptyParam,
              Helpers.assertRejectWithReason $
                InvalidAccountReference accRef
            ),
          -- Amount tests.
            ( "amount_negative_one__fail_with_AmountTooLarge",
              encodedAccountAddress1,
              Helpers.assertRejectWithReason $
                AmountTooLarge
                    contractAddr
                    (Types.Amount 18_446_744_073_709_551_615) -- Expected amount after negative overflow of -1 for u64.
            ),
            ( "amount_10001_greater_than_balance_of_10K__fail_with_AmountTooLarge",
              encodedAccountAddress1,
              Helpers.assertRejectWithReason $
                AmountTooLarge contractAddr (Types.Amount 10_001)
            )
        ]
  where
    contractAddr = Types.AddressContract $ ContractAddress 0 0
    -- When using pointing to 32 0s in memory, this is the address that is used.
    -- This is safe, since addressFromText was passed a valid address string.
    accRef =
        fromRight
            (error "addressFromText did not return an account reference")
            $ ID.addressFromText "2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3"

sendTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
sendTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": send-tests")
        "./testdata/contracts/send-tests.wasm"
        [ -- Positive test to ensure correctness of test setup
          ("self_message_with_accept__succeed", rcvNameParam, Helpers.assertSuccess),
          -- Run with invalid address
            ( "invalid_address__fail_with_InvalidContractAddress_42_42",
              rcvNameParam,
              Helpers.assertRejectWithReason $ InvalidContractAddress $ ContractAddress 42 42
            ),
          -- Try reading outside memory with receive_name
          ("receive_name_negative__fail", rcvNameParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("receive_name_greater_than_mem__fail", rcvNameParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- Try reading outside memory with receive_name_len
          ("receive_name_len_negative__fail", rcvNameParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("receive_name_len_greater_than_mem__fail", rcvNameParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- Try reading outside memory with parameter
          ("parameter_negative__fail", rcvNameParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("parameter_greater_than_mem__fail", rcvNameParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- Try reading outside memory with parameter_len
          -- the reason this fails with out of energy is because we charge
          -- based on the length of data. "Negative" lengths are interpreted as very large numbers
          -- due to two's complement representation. The "negative" is just a point of view,
          -- we use unsigned integers for lengths all the time.
          ("parameter_len_negative__fail", rcvNameParam, Helpers.assertRejectWithReason OutOfEnergy),
          ("parameter_len_greater_than_mem__fail", rcvNameParam, Helpers.assertRejectWithReason RuntimeFailure),
          -- Amount tests
            ( "amount_negative_one__fail",
              rcvNameParam,
              Helpers.assertRejectWithReason $
                AmountTooLarge
                    (Types.AddressContract $ ContractAddress 0 0)
                    (Types.Amount 18_446_744_073_709_551_615) -- Expected amount after negative overflow of -1 for u64.
            ),
            ( "amount_10001_greater_than_balance_of_10K__fail_with_AmountTooLarge",
              rcvNameParam,
              Helpers.assertRejectWithReason $
                AmountTooLarge
                    (Types.AddressContract $ ContractAddress 0 0)
                    (Types.Amount 10_001)
            )
        ]
  where
    rcvNameParam = BSS.toShort "test.accept"

actionTreeTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
actionTreeTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": action-tree-tests")
        "./testdata/contracts/action-tree-tests.wasm"
        [ -- Combine 'and' and 'or'
          ("complex_combine__transfer_2microGTU_and_succeed", encodedAccountAddress1, Helpers.assertSuccessWhere $ expectCCDTransferred 2),
          ("complex_combine__transfer_3microGTU_and_succeed", encodedAccountAddress1, Helpers.assertSuccessWhere $ expectCCDTransferred 3),
          ("complex_combine__transfer_6microGTU_and_succeed", encodedAccountAddress1, Helpers.assertSuccessWhere $ expectCCDTransferred 6),
          -- Deep nesting of combine_and
          ("combine_100_transfers_with_and_then_accept__transfer_100_and_succeed", encodedAccountAddress1, Helpers.assertSuccessWhere $ expectCCDTransferred 100),
          -- Invalid parameters for combine_and
          ("combine_and__first_invalid__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("combine_and__second_invalid__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("combine_and__both_params_invalid__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("combine_and__own_action_id__fail", encodedAccountAddress1, Helpers.assertRejectWithReason RuntimeFailure),
          -- Deep nesting of combine_or
          ("combine_100_failing_transfers_with_or_then_accept__transfer_0_and_succeed", encodedAccountAddress1, Helpers.assertSuccessWhere $ expectCCDTransferred 0),
          -- Invalid parameters for combine_or
          ("combine_or__first_invalid__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("combine_or__second_invalid__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("combine_or__both_params_invalid__fail", emptyParam, Helpers.assertRejectWithReason RuntimeFailure),
          ("combine_or__own_action_id__fail", encodedAccountAddress1, Helpers.assertRejectWithReason RuntimeFailure)
        ]

memoryTestCases ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
memoryTestCases spv pvString =
    runReceiveTestsFromFile
        spv
        (pvString ++ ": memory-tests")
        "./testdata/contracts/memory-tests.wasm"
        [("memory_size_is_correct_and_growable__succeed", emptyParam, Helpers.assertSuccess)]
