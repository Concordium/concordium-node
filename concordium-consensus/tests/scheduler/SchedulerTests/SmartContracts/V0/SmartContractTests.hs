{-# LANGUAGE OverloadedStrings #-}
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
module SchedulerTests.SmartContracts.V0.SmartContractTests where

import Concordium.Crypto.DummyData
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.DummyData
import Concordium.ID.Types (addressFromText)
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner (PayloadJSON (..), TransactionJSON (..))
import Concordium.Scheduler.Types (Amount, ContractAddress (..), Event, RejectReason (..))
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types.DummyData
import Concordium.Wasm (WasmVersion (..))
import Data.ByteString.Short as BSS
import Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import SchedulerTests.TestUtils
import Test.Hspec

-- ** Test runners **

-- | Run a number of init tests from a specific file.
runInitTestsFromFile :: FilePath -> [(Text, ShortByteString, TResultSpec)] -> [TestCase PV1]
runInitTestsFromFile testFile = map f
  where
    f (testName, initParam, resSpec) =
        TestCase
            { tcName = Text.unpack testName,
              tcParameters = (defaultParams @PV1){tpInitialBlockState = initialBlockState},
              tcTransactions =
                [   ( TJSON
                        { payload = DeployModule V0 testFile,
                          metadata = makeDummyHeader alesAccount 1 100000,
                          keys = [(0, [(0, alesKP)])]
                        },
                      (Success emptyExpect, emptySpec)
                    ),
                    ( TJSON
                        { payload = InitContract 0 V0 testFile testName initParam,
                          metadata = makeDummyHeader alesAccount 2 100000,
                          keys = [(0, [(0, alesKP)])]
                        },
                      (resSpec, emptySpec)
                    )
                ]
            }

-- | Run a number of receive tests from a specific file.
--   Each test is run in isolation, i.e., with a new state.
--   The file is expected to have an init function named 'init_test'
--   and a number of receive functions, each prefixed with 'test.'.
runReceiveTestsFromFile :: FilePath -> [(Text, ShortByteString, TResultSpec)] -> [TestCase PV1]
runReceiveTestsFromFile testFile = map f
  where
    f (testName, rcvParam, resSpec) =
        TestCase
            { tcName = Text.unpack testName,
              tcParameters = (defaultParams @PV1){tpInitialBlockState = initialBlockState},
              tcTransactions =
                [   ( TJSON
                        { payload = DeployModule V0 testFile,
                          metadata = makeDummyHeader alesAccount 1 100000,
                          keys = [(0, [(0, alesKP)])]
                        },
                      (Success emptyExpect, emptySpec)
                    ),
                    ( TJSON
                        { payload = InitContract 10000 V0 testFile "init_test" "",
                          metadata = makeDummyHeader alesAccount 2 100000,
                          keys = [(0, [(0, alesKP)])]
                        },
                      (Success emptyExpect, emptySpec)
                    ),
                    ( TJSON
                        { payload = Update 0 (ContractAddress 0 0) ("test." <> testName) rcvParam,
                          metadata = makeDummyHeader alesAccount 3 100000,
                          keys = [(0, [(0, alesKP)])]
                        },
                      (resSpec, emptySpec)
                    )
                ]
            }

-- ** Helper Functions **

initialBlockState :: BlockState PV1
initialBlockState =
    blockStateWithAlesAccount
        100000000
        (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

emptyParam :: ShortByteString
emptyParam = ""

mkParamOfSize :: Int -> ShortByteString
mkParamOfSize n = BSS.pack $ replicate n 1

encodedThomasAcc :: BSS.ShortByteString
encodedThomasAcc = BSS.toShort . S.encode $ thomasAccount

expectGTUTransferred :: Amount -> [Event] -> Expectation
expectGTUTransferred expectedAmnt events = foldr sumTransfers 0 events `shouldBe` expectedAmnt
  where
    sumTransfers e acc = case e of
        Types.Transferred{..} -> acc + etAmount
        _ -> acc

-- ** Tests **

logEventTestCases :: [TestCase PV1]
logEventTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/log-event-tests.wasm"
        [ -- Try to read memory before position 0.
          ("start_negative__fail", emptyParam, Reject RuntimeFailure),
          ("length_negative__fail", emptyParam, Reject RuntimeFailure),
          ("start_and_length_negative__fail", emptyParam, Reject RuntimeFailure),
          -- Try to read after end of memory
          ("start_greater_than_mem__fail", emptyParam, Reject RuntimeFailure),
          ("length_greater_than_mem__fail", emptyParam, Reject RuntimeFailure)
        ]

getParameterSizeTestCases :: [TestCase PV1]
getParameterSizeTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/get-parameter-size-tests.wasm"
        [ ("size_is_0__return_0_and_succeed", emptyParam, Success emptyExpect),
          ("size_is_max__return_max_and_succeed", maxSizedParam, Success emptyExpect)
        ]
  where
    -- Value must stay in sync with MAX_PARAMETER_SIZE from 'wasm-chain-integration/src/constants.src'
    maxSizedParam = mkParamOfSize 1024

getParameterSectionTestCases :: [TestCase PV1]
getParameterSectionTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/get-parameter-section-tests.wasm"
        [ -- Try to read before parameter section begins
          ("offset_negative__fail", param100, Reject RuntimeFailure),
          -- the reason this fails with out of energy is because we charge
          -- based on the length of data. "Negative" lengths are interpreted as very large numbers
          -- due to two's complement representation. The "negative" is just a point of view,
          -- we use unsigned integers for lengths all the time.
          ("length_negative__fail", param100, Reject OutOfEnergy),
          -- Read whole parameter section
          ("length_equal_to_param_size_100__return_100_and_succeed", param100, Success emptyExpect),
          -- Try to read after parameter section ends
          ("length_greater_than_param_size_100__return_100_and_succeed", param100, Success emptyExpect),
          ("offset_greater_than_param_size_100__fail", param100, Reject RuntimeFailure),
          -- Try to write outside bounds of linear memory
          ("write_location_negative__fail", param100, Reject RuntimeFailure),
          ("write_location_greater_than_mem__fail", param100, Reject RuntimeFailure)
        ]
  where
    param100 = mkParamOfSize 100

stateSizeTestCases :: [TestCase PV1]
stateSizeTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/state-size-tests.wasm"
        [ ("size_is_0__return_0_and_succeed", emptyParam, Success emptyExpect),
          ("size_is_max__return_max_and_succeed", emptyParam, Success emptyExpect)
        ]

loadStateTestCases :: [TestCase PV1]
loadStateTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/load-state-tests.wasm"
        [ -- Positive tests
          ("load_all_of_state__return_state_size_and_succeed", emptyParam, Success emptyExpect),
          ("load_max_sized_state__return_state_size_and_succeed", emptyParam, Success emptyExpect),
          ("length_greater_than_state_size__return_state_size_and_succeed", emptyParam, Success emptyExpect),
          -- Try to read outside of state
          ("offset_negative__fail", emptyParam, Reject RuntimeFailure),
          ("offset_greater_than_state_size__fail", emptyParam, Reject RuntimeFailure),
          -- the reason this fails with out of energy is because we charge
          -- based on the length of data. "Negative" lengths are interpreted as very large numbers
          -- due to two's complement representation. The "negative" is just a point of view,
          -- we use unsigned integers for lengths all the time.
          ("length_negative__fail", emptyParam, Reject OutOfEnergy),
          -- Try to write outside bounds of linear memory
          ("write_location_negative__fail", emptyParam, Reject RuntimeFailure),
          ("write_location_greater_than_mem__fail", emptyParam, Reject RuntimeFailure)
        ]

writeStateTestCases :: [TestCase PV1]
writeStateTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/write-state-tests.wasm"
        [ -- Positive tests
          ("write_100_bytes__return_100_and_succeed", emptyParam, Success emptyExpect),
          -- Read before start of memory
          ("read_location_negative__fail", emptyParam, Reject RuntimeFailure),
          ("length_negative__fail", emptyParam, Reject OutOfEnergy),
          -- Read after end of memory
          ("length_greater_than_mem__fail", emptyParam, Reject RuntimeFailure),
          ("read_location_greater_than_mem__fail", emptyParam, Reject RuntimeFailure),
          -- Write outside of state bounds
          ("offset_negative__fail", emptyParam, Reject RuntimeFailure),
          ("offset_greater_than_current_state_size__fail", emptyParam, Reject RuntimeFailure)
        ]

resizeStateTestCases :: [TestCase PV1]
resizeStateTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/resize-state-tests.wasm"
        -- resize_state returns 0 on failure and 1 on success.
        [ ("new_size_negative__return_zero_and_succeed", emptyParam, Success emptyExpect),
          ("new_size_zero__return_one_and_succeed", emptyParam, Success emptyExpect),
          ("new_size_max__return_one_and_succeed", emptyParam, Success emptyExpect),
          ("new_size_greater_than_max__return_zero_and_succeed", emptyParam, Success emptyExpect)
        ]

onlyInInitTestCases :: [TestCase PV1]
onlyInInitTestCases =
    runInitTestsFromFile
        file
        [ ("init_get_init_origin__valid_param__succeed", emptyParam, Success emptyExpect),
          ("init_get_init_origin__write_location_negative__fail", emptyParam, Reject RuntimeFailure),
          ("init_get_init_origin__write_location_greater_than_mem__fail", emptyParam, Reject RuntimeFailure)
        ]
        ++ runReceiveTestsFromFile
            file
            [("get_init_origin__valid_param_from_receive__fail", emptyParam, Reject RuntimeFailure)]
  where
    file = "./testdata/contracts/only-in-init-tests.wasm"

onlyInReceiveTestCases :: [TestCase PV1]
onlyInReceiveTestCases =
    runInitTestsFromFile
        file
        [ -- Try calling functions from init with valid parameters.
          ("init_get_receive_invoker__valid_param_from_init__fail", emptyParam, Reject RuntimeFailure),
          ("init_get_receive_sender__valid_param_from_init__fail", emptyParam, Reject RuntimeFailure),
          ("init_get_receive_self_address__valid_param_from_init__fail", emptyParam, Reject RuntimeFailure),
          ("init_get_receive_owner__valid_param_from_init__fail", emptyParam, Reject RuntimeFailure),
          ("init_get_receive_self_balance__valid_param_from_init__fail", emptyParam, Reject RuntimeFailure)
        ]
        ++ runReceiveTestsFromFile
            file
            [ -- get_receive_invoker
              ("get_receive_invoker__valid_param__succeed", emptyParam, Success emptyExpect),
              ("get_receive_invoker__start_negative__fail", emptyParam, Reject RuntimeFailure),
              ("get_receive_invoker__start_greater_than_mem__fail", emptyParam, Reject RuntimeFailure),
              -- get_receive_sender
              ("get_receive_sender__valid_param__succeed", emptyParam, Success emptyExpect),
              ("get_receive_sender__start_negative__fail", emptyParam, Reject RuntimeFailure),
              ("get_receive_sender__start_greater_than_mem__fail", emptyParam, Reject RuntimeFailure),
              -- get_receive_self_address
              ("get_receive_self_address__valid_param__succeed", emptyParam, Success emptyExpect),
              ("get_receive_self_address__start_negative__fail", emptyParam, Reject RuntimeFailure),
              ("get_receive_self_address__start_greater_than_mem__fail", emptyParam, Reject RuntimeFailure),
              -- get_receive_owner
              ("get_receive_owner__valid_param__succeed", emptyParam, Success emptyExpect),
              ("get_receive_owner__start_negative__fail", emptyParam, Reject RuntimeFailure),
              ("get_receive_owner__start_greater_than_mem__fail", emptyParam, Reject RuntimeFailure),
              -- get_receive_self_balance
              ("get_receive_self_balance__send_10K_microGTU__balance_is_10K_and_succeed", emptyParam, Success emptyExpect)
            ]
  where
    file = "./testdata/contracts/only-in-receive-tests.wasm"

simpleTransferTestCases :: [TestCase PV1]
simpleTransferTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/simple-transfer-tests.wasm"
        [ -- Ensure that only the _last_ transfer is processed.
          ("multiple_transfers__transfer_4_and_succeed", encodedThomasAcc, Success $ expectGTUTransferred 4),
          -- Try reading memory outside of memory bounds.
          ("addr_bytes_negative__fail", encodedThomasAcc, Reject RuntimeFailure),
          ("addr_bytes_greater_than_mem__fail", encodedThomasAcc, Reject RuntimeFailure),
          -- Ensure correct error message for invalid acc ref.
            ( "invalid_addr__fail_with_InvalidAccountReference",
              emptyParam,
              Reject $
                InvalidAccountReference accRef
            ),
          -- Amount tests.
            ( "amount_negative_one__fail_with_AmountTooLarge",
              encodedThomasAcc,
              Reject $
                AmountTooLarge
                    contractAddr
                    (Types.Amount 18446744073709551615) -- Expected amount after negative overflow of -1 for u64.
            ),
            ( "amount_10001_greater_than_balance_of_10K__fail_with_AmountTooLarge",
              encodedThomasAcc,
              Reject $
                AmountTooLarge contractAddr (Types.Amount 10001)
            )
        ]
  where
    contractAddr = Types.AddressContract $ ContractAddress 0 0

    -- When using pointing to 32 0s in memory, this is the address that is used.
    Right accRef = addressFromText "2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3"

sendTestCases :: [TestCase PV1]
sendTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/send-tests.wasm"
        [ -- Positive test to ensure correctness of test setup
          ("self_message_with_accept__succeed", rcvNameParam, Success emptyExpect),
          -- Run with invalid address
            ( "invalid_address__fail_with_InvalidContractAddress_42_42",
              rcvNameParam,
              Reject $ InvalidContractAddress $ ContractAddress 42 42
            ),
          -- Try reading outside memory with receive_name
          ("receive_name_negative__fail", rcvNameParam, Reject RuntimeFailure),
          ("receive_name_greater_than_mem__fail", rcvNameParam, Reject RuntimeFailure),
          -- Try reading outside memory with receive_name_len
          ("receive_name_len_negative__fail", rcvNameParam, Reject RuntimeFailure),
          ("receive_name_len_greater_than_mem__fail", rcvNameParam, Reject RuntimeFailure),
          -- Try reading outside memory with parameter
          ("parameter_negative__fail", rcvNameParam, Reject RuntimeFailure),
          ("parameter_greater_than_mem__fail", rcvNameParam, Reject RuntimeFailure),
          -- Try reading outside memory with parameter_len
          -- the reason this fails with out of energy is because we charge
          -- based on the length of data. "Negative" lengths are interpreted as very large numbers
          -- due to two's complement representation. The "negative" is just a point of view,
          -- we use unsigned integers for lengths all the time.
          ("parameter_len_negative__fail", rcvNameParam, Reject OutOfEnergy),
          ("parameter_len_greater_than_mem__fail", rcvNameParam, Reject RuntimeFailure),
          -- Amount tests
            ( "amount_negative_one__fail",
              rcvNameParam,
              Reject $
                AmountTooLarge
                    (Types.AddressContract $ ContractAddress 0 0)
                    (Types.Amount 18446744073709551615) -- Expected amount after negative overflow of -1 for u64.
            ),
            ( "amount_10001_greater_than_balance_of_10K__fail_with_AmountTooLarge",
              rcvNameParam,
              Reject $
                AmountTooLarge
                    (Types.AddressContract $ ContractAddress 0 0)
                    (Types.Amount 10001)
            )
        ]
  where
    rcvNameParam = BSS.toShort "test.accept"

actionTreeTestCases :: [TestCase PV1]
actionTreeTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/action-tree-tests.wasm"
        [ -- Combine 'and' and 'or'
          ("complex_combine__transfer_2microGTU_and_succeed", encodedThomasAcc, Success $ expectGTUTransferred 2),
          ("complex_combine__transfer_3microGTU_and_succeed", encodedThomasAcc, Success $ expectGTUTransferred 3),
          ("complex_combine__transfer_6microGTU_and_succeed", encodedThomasAcc, Success $ expectGTUTransferred 6),
          -- Deep nesting of combine_and
          ("combine_100_transfers_with_and_then_accept__transfer_100_and_succeed", encodedThomasAcc, Success $ expectGTUTransferred 100),
          -- Invalid parameters for combine_and
          ("combine_and__first_invalid__fail", emptyParam, Reject RuntimeFailure),
          ("combine_and__second_invalid__fail", emptyParam, Reject RuntimeFailure),
          ("combine_and__both_params_invalid__fail", emptyParam, Reject RuntimeFailure),
          ("combine_and__own_action_id__fail", encodedThomasAcc, Reject RuntimeFailure),
          -- Deep nesting of combine_or
          ("combine_100_failing_transfers_with_or_then_accept__transfer_0_and_succeed", encodedThomasAcc, Success $ expectGTUTransferred 0),
          -- Invalid parameters for combine_or
          ("combine_or__first_invalid__fail", emptyParam, Reject RuntimeFailure),
          ("combine_or__second_invalid__fail", emptyParam, Reject RuntimeFailure),
          ("combine_or__both_params_invalid__fail", emptyParam, Reject RuntimeFailure),
          ("combine_or__own_action_id__fail", encodedThomasAcc, Reject RuntimeFailure)
        ]

memoryTestCases :: [TestCase PV1]
memoryTestCases =
    runReceiveTestsFromFile
        "./testdata/contracts/memory-tests.wasm"
        [("memory_size_is_correct_and_growable__succeed", emptyParam, Success emptyExpect)]

tests :: Spec
tests = do
    describe "log-event-tests" $
        mkSpecs logEventTestCases
    describe "get-parameter-size-tests" $
        mkSpecs getParameterSizeTestCases
    describe "get-parameter-section-tests" $
        mkSpecs getParameterSectionTestCases
    describe "state-size-tests" $
        mkSpecs stateSizeTestCases
    describe "load-state-tests" $
        mkSpecs loadStateTestCases
    describe "write-state-tests" $
        mkSpecs writeStateTestCases
    describe "resize-state-tests" $
        mkSpecs resizeStateTestCases
    describe "only-in-init-tests" $
        mkSpecs onlyInInitTestCases
    describe "only-in-receive-tests" $
        mkSpecs onlyInReceiveTestCases
    describe "simple-transfer-tests" $
        mkSpecs simpleTransferTestCases
    describe "send-tests" $
        mkSpecs sendTestCases
    describe "action-tree-tests" $
        mkSpecs actionTreeTestCases
    describe "memory-tests" $
        mkSpecs memoryTestCases
