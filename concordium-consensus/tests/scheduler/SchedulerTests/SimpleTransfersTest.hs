{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module SchedulerTests.SimpleTransfersTest where

import Test.HUnit
import Test.Hspec

import Control.Monad.IO.Class
import Lens.Micro.Platform

import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.TransactionVerification

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState.Invariants

import Concordium.Crypto.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Scheduler.DummyData
import Concordium.Types.DummyData
import qualified Data.ByteString.Short as BSS

import Concordium.Types.ProtocolVersion (IsProtocolVersion)
import SchedulerTests.Helpers
import SchedulerTests.TestUtils

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState PV1
initialBlockState =
    blockStateWithAlesAccount
        1000000
        (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 1000000) Acc.emptyAccounts)

initialBlockState2 :: BlockState PV2
initialBlockState2 =
    blockStateWithAlesAccount
        1000000
        (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 1000000) Acc.emptyAccounts)

initialBlockState3 :: BlockState PV3
initialBlockState3 =
    blockStateWithAlesAccount
        1000000
        (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 1000000) Acc.emptyAccounts)

-- simple transfers to be tested with protocol version 2
transactionsInput2 :: [TransactionJSON]
transactionsInput2 =
    -- transfer 10000 from A to A
    [ TJSON
        { payload = Transfer{toaddress = alesAccount, amount = 10000},
          metadata = makeDummyHeader alesAccount 1 simpleTransferCost,
          keys = [(0, [(0, alesKP)])]
        },
      -- transfer 8800 from A to T
      TJSON
        { payload = Transfer{toaddress = thomasAccount, amount = 8800},
          metadata = makeDummyHeader alesAccount 2 simpleTransferCost,
          keys = [(0, [(0, alesKP)])]
        },
      -- transfer everything from from A to T
      -- the (100 *) is conversion between NRG and GTU
      TJSON
        { payload = Transfer{toaddress = thomasAccount, amount = 1000000 - 8800 - 3 * 100 * fromIntegral simpleTransferCost},
          metadata = makeDummyHeader alesAccount 3 simpleTransferCost,
          keys = [(0, [(0, alesKP)])]
        },
      -- transfer 10000 back from T to A
      TJSON
        { payload = Transfer{toaddress = alesAccount, amount = 100 * fromIntegral simpleTransferCost},
          metadata = makeDummyHeader thomasAccount 1 simpleTransferCost,
          keys = [(0, [(0, thomasKP)])]
        },
      -- the next transaction should fail because the balance on A is now exactly enough to cover the transfer cost
      TJSON
        { payload = Transfer{toaddress = thomasAccount, amount = 1},
          metadata = makeDummyHeader alesAccount 4 simpleTransferCost,
          keys = [(0, [(0, alesKP)])]
        }
    ]

-- simple transfers to be tested with protocol version 3
transactionsInput3 :: [TransactionJSON]
transactionsInput3 =
    -- transfer 10000 from A to A
    [ TJSON
        { payload = Transfer{toaddress = createAlias alesAccount 0, amount = 10000},
          metadata = makeDummyHeader alesAccount 1 simpleTransferCost,
          keys = [(0, [(0, alesKP)])]
        },
      -- transfer 8800 from A to T
      TJSON
        { payload = Transfer{toaddress = createAlias thomasAccount 0, amount = 8800},
          metadata = makeDummyHeader alesAccount 2 simpleTransferCost,
          keys = [(0, [(0, alesKP)])]
        },
      -- transfer everything from from A to T
      -- the (100 *) is conversion between NRG and GTU
      TJSON
        { payload = Transfer{toaddress = createAlias thomasAccount 1, amount = 1000000 - 8800 - 3 * 100 * fromIntegral simpleTransferCost},
          metadata = makeDummyHeader alesAccount 3 simpleTransferCost,
          keys = [(0, [(0, alesKP)])]
        },
      -- transfer 10000 back from T to A
      TJSON
        { payload = Transfer{toaddress = createAlias alesAccount 1, amount = 100 * fromIntegral simpleTransferCost},
          metadata = makeDummyHeader thomasAccount 1 simpleTransferCost,
          keys = [(0, [(0, thomasKP)])]
        },
      -- the next transaction should fail because the balance on A is now exactly enough to cover the transfer cost
      TJSON
        { payload = Transfer{toaddress = createAlias thomasAccount 2, amount = 1},
          metadata = makeDummyHeader (createAlias alesAccount 4) 4 simpleTransferCost,
          keys = [(0, [(0, alesKP)])]
        }
    ]

-- simple transfers to be tested with protocol version 1
transactionsInput :: [TransactionJSON]
transactionsInput =
    -- transfer with memo - should get rejected because protocol version is 1
    transactionsInput2
        ++ [ TJSON
                { payload = TransferWithMemo{twmToAddress = alesAccount, twmAmount = 1, twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]},
                  metadata = makeDummyHeader thomasAccount 2 simpleTransferCost,
                  keys = [(0, [(0, thomasKP)])]
                }
           ]

-- simple transfers with memo to be tested with protocol version 2
transactionsInput2Memo :: [TransactionJSON]
transactionsInput2Memo =
    -- transfer 10000 from A to A
    [ TJSON
        { payload = TransferWithMemo{twmToAddress = alesAccount, twmAmount = 10000, twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]},
          metadata = makeDummyHeader alesAccount 1 $ simpleTransferCostWithMemo2 4,
          keys = [(0, [(0, alesKP)])]
        },
      -- transfer 8800 from A to T
      TJSON
        { payload = TransferWithMemo{twmToAddress = thomasAccount, twmAmount = 8800, twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]},
          metadata = makeDummyHeader alesAccount 2 $ simpleTransferCostWithMemo2 4,
          keys = [(0, [(0, alesKP)])]
        },
      -- transfer everything from from A to T
      -- the (100 *) is conversion between NRG and GTU
      TJSON
        { payload = TransferWithMemo{twmToAddress = thomasAccount, twmAmount = 1000000 - 8800 - 3 * 100 * fromIntegral (simpleTransferCostWithMemo2 4), twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]},
          metadata = makeDummyHeader alesAccount 3 $ simpleTransferCostWithMemo2 4,
          keys = [(0, [(0, alesKP)])]
        },
      -- transfer 10000 back from T to A
      TJSON
        { payload = TransferWithMemo{twmToAddress = alesAccount, twmAmount = 100 * fromIntegral (simpleTransferCostWithMemo2 4), twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]},
          metadata = makeDummyHeader thomasAccount 1 $ simpleTransferCostWithMemo2 4,
          keys = [(0, [(0, thomasKP)])]
        },
      -- the next transaction should fail because the balance on A is now exactly enough to cover the transfer cost
      TJSON
        { payload = TransferWithMemo{twmToAddress = thomasAccount, twmAmount = 1, twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]},
          metadata = makeDummyHeader alesAccount 4 $ simpleTransferCostWithMemo2 4,
          keys = [(0, [(0, alesKP)])]
        }
    ]

type TestResult =
    ( [(BlockItemWithStatus, Types.ValidResult)],
      [(TransactionWithStatus, Types.FailureKind)],
      Types.Amount,
      Types.Amount
    )

testSimpleTransfer :: IsProtocolVersion pv => BlockState pv -> [TransactionJSON] -> IO TestResult
testSimpleTransfer initialBs tInput = do
    transactions <- processUngroupedTransactions tInput
    let (Sch.FilteredTransactions{..}, finState) =
            Types.runSI
                (Sch.filterTransactions dummyBlockSize dummyBlockTimeout transactions)
                dummyChainMeta
                maxBound
                maxBound
                initialBs
    let gstate = finState ^. Types.ssBlockState
    case invariantBlockState gstate (finState ^. Types.schedulerExecutionCosts) of
        Left f -> liftIO $ assertFailure f
        Right _ -> return ()
    return
        ( getResults ftAdded,
          ftFailed,
          gstate ^. blockAccounts . singular (ix alesAccount) . accountAmount,
          gstate ^. blockAccounts . singular (ix thomasAccount) . accountAmount
        )

checkSimpleTransferResult :: TestResult -> Assertion
checkSimpleTransferResult (suc, fails, alesamount, thomasamount) = do
    assertEqual "There should be no failed transactions." [] fails
    rejectLast
    rejectSecondToLast
    assertBool "Initial transactions are accepted." nonreject
    assertEqual "Amount on the A account." 0 alesamount
    assertEqual ("Amount on the T account." ++ show thomasamount) (2000000 - 5 * 100 * fromIntegral simpleTransferCost - 100 * fromIntegral (simpleTransferCostWithMemo1 4)) thomasamount
  where
    nonreject =
        all
            ( \case
                (_, Types.TxSuccess{}) -> True
                (_, Types.TxReject{}) -> False
            )
            (init $ init suc)
    rejectLast = case last suc of
        (_, Types.TxReject Types.SerializationFailure) -> return ()
        err -> assertFailure $ "Incorrect result of the first transaction: " ++ show (snd err)
    rejectSecondToLast = case last $ init suc of
        (_, Types.TxReject (Types.AmountTooLarge addr amnt)) -> do
            assertEqual "Sending from A" (Types.AddressAccount alesAccount) addr
            assertEqual "Exactly 1microGTU" 1 amnt
        err -> assertFailure $ "Incorrect result of the last transaction: " ++ show (snd err)

checkSimpleTransferResult2 :: TestResult -> Assertion
checkSimpleTransferResult2 (suc, fails, alesamount, thomasamount) = do
    assertEqual "There should be no failed transactions." [] fails
    rejectLast
    assertBool "Initial transactions are accepted." nonreject
    assertEqual "Amount on the A account." 0 alesamount
    assertEqual ("Amount on the T account." ++ show thomasamount) (2000000 - 5 * 100 * fromIntegral simpleTransferCost) thomasamount
  where
    nonreject =
        all
            ( \case
                (_, Types.TxSuccess{}) -> True
                (_, Types.TxReject{}) -> False
            )
            (init suc)
    rejectLast = case last suc of
        (_, Types.TxReject (Types.AmountTooLarge addr amnt)) -> do
            assertEqual "Sending from A" (Types.AddressAccount alesAccount) addr
            assertEqual "Exactly 1microGTU" 1 amnt
        err -> assertFailure $ "Incorrect result of the last transaction: " ++ show (snd err)

checkSimpleTransferResult2Memo :: TestResult -> Assertion
checkSimpleTransferResult2Memo (suc, fails, alesamount, thomasamount) = do
    assertEqual "There should be no failed transactions." [] fails
    rejectLast
    assertBool "Initial transactions are accepted." nonreject
    assertEqual "Amount on the A account." 0 alesamount
    assertEqual ("Amount on the T account." ++ show thomasamount) (2000000 - 5 * 100 * fromIntegral (simpleTransferCostWithMemo2 4)) thomasamount
  where
    nonreject =
        all
            ( \case
                (_, Types.TxSuccess{..}) -> case last vrEvents of
                    Types.TransferMemo memo -> memo == Types.Memo (BSS.pack [0, 1, 2, 3])
                    _ -> False
                (_, Types.TxReject{}) -> False
            )
            (init suc)
    rejectLast = case last suc of
        (_, Types.TxReject (Types.AmountTooLarge addr amnt)) -> do
            assertEqual "Sending from A" (Types.AddressAccount alesAccount) addr
            assertEqual "Exactly 1microGTU" 1 amnt
        err -> assertFailure $ "Incorrect result of the last transaction: " ++ show (snd err)

checkSimpleTransferResult3 :: TestResult -> Assertion
checkSimpleTransferResult3 (suc, fails, alesamount, thomasamount) = do
    --  assertEqual "Actual result" suc []
    assertEqual "There should be no failed transactions." [] fails
    rejectLast
    assertBool "Initial transactions are accepted." nonreject
    assertEqual "Amount on the A account." 0 alesamount
    assertEqual ("Amount on the T account." ++ show thomasamount) (2000000 - 5 * 100 * fromIntegral simpleTransferCost) thomasamount
  where
    nonreject =
        all
            ( \case
                (_, Types.TxSuccess{}) -> True
                (_, Types.TxReject{}) -> False
            )
            (init suc)
    rejectLast = case last suc of
        (_, Types.TxReject (Types.AmountTooLarge addr amnt)) -> do
            assertEqual "Sending from A" (Types.AddressAccount (createAlias alesAccount 4)) addr
            assertEqual "Exactly 1microGTU" 1 amnt
        err -> assertFailure $ "Incorrect result of the last transaction: " ++ show (snd err)

tests :: SpecWith ()
tests =
    do
        describe "Simple transfers test (with protocol version 1):" $
            specify "4 successful and 2 failed transactions" $
                testSimpleTransfer initialBlockState transactionsInput >>= checkSimpleTransferResult
        describe "Simple transfers test (with protocol version 2):" $
            specify "4 successful and 1 failed transaction" $
                testSimpleTransfer initialBlockState2 transactionsInput2 >>= checkSimpleTransferResult2
        describe "Simple transfers test with memo (with protocol version 2):" $
            specify "4 successful and 1 failed transaction" $
                testSimpleTransfer initialBlockState2 transactionsInput2Memo >>= checkSimpleTransferResult2Memo
        describe "Simple transfers test (with protocol version 3):" $
            specify "4 successful and 1 failed transaction" $
                testSimpleTransfer initialBlockState3 transactionsInput3 >>= checkSimpleTransferResult3
