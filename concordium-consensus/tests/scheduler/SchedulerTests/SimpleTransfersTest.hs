{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module SchedulerTests.SimpleTransfersTest (tests) where

import Control.Monad (when)
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types.ProtocolVersion (IsProtocolVersion)
import qualified Data.ByteString.Short as BSS
import qualified SchedulerTests.Helpers as Helpers
import SchedulerTests.TestUtils

initialBlockState ::
    (IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed 1_000_000 0,
          Helpers.makeTestAccountFromSeed 1_000_000 1
        ]

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

accountAddress1 :: Types.AccountAddress
accountAddress1 = Helpers.accountAddressFromSeed 1

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

keyPair1 :: SigScheme.KeyPair
keyPair1 = Helpers.keyPairFromSeed 1

transferWithMemoRejectTestP1 ::
    Spec
transferWithMemoRejectTestP1 = specify
    "P1: Transfer with memo - should get rejected because protocol version is 1"
    $ do
        let transactions =
                [ TJSON
                    { payload =
                        TransferWithMemo
                            { twmToAddress = accountAddress0,
                              twmAmount = 1,
                              twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]
                            },
                      metadata = makeDummyHeader accountAddress1 1 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair1)])]
                    }
                ]
        (_, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                @'Types.P1
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                transactions
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState PV1 ->
        Helpers.PersistentBSM PV1 Assertion
    checkState result state = do
        doCheckBlockStateInvariants <-
            Helpers.assertBlockStateInvariantsH
                state
                (Helpers.srExecutionCosts result)
        maybeAccount0 <- BS.bsoGetAccount state accountAddress0
        maybeAmount0 <- case maybeAccount0 of
            Nothing -> return Nothing
            Just (_, account) -> Just <$> BS.accountAmount account
        maybeAccount1 <- BS.bsoGetAccount state accountAddress1
        maybeAmount1 <- case maybeAccount1 of
            Nothing -> return Nothing
            Just (_, account) -> Just <$> BS.accountAmount account
        return $ do
            let Sch.FilteredTransactions{..} = Helpers.srTransactions result
            assertEqual "There should be no failed transactions." [] ftFailed

            case Helpers.getResults ftAdded of
                [(_, Types.TxReject Types.SerializationFailure)] -> return ()
                err -> assertFailure $ "Incorrect transaction result: " ++ show err

            assertEqual "Amount on account0." (Just 1_000_000) maybeAmount0
            let transactionCost = 100 * fromIntegral (Helpers.simpleTransferCostWithMemo1 4)
            assertEqual
                ("Amount on account1." ++ show maybeAmount1)
                (Just (1_000_000 - transactionCost))
                maybeAmount1

            doCheckBlockStateInvariants

simpleTransferTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
simpleTransferTest _ pvString = specify
    (pvString ++ ": 4 successful and 1 failed transaction")
    $ do
        let transactions =
                -- transfer 10000 from account0 to account0
                [ TJSON
                    { payload = Transfer{toaddress = accountAddress0, amount = 10_000},
                      metadata = makeDummyHeader accountAddress0 1 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair0)])]
                    },
                  -- transfer 8800 from account0 to account1
                  TJSON
                    { payload = Transfer{toaddress = accountAddress1, amount = 8_800},
                      metadata = makeDummyHeader accountAddress0 2 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair0)])]
                    },
                  -- transfer everything from from account0 to account1
                  -- the (100 *) is conversion between NRG and CCD
                  TJSON
                    { payload =
                        Transfer
                            { toaddress = accountAddress1,
                              amount = 1_000_000 - 8_800 - 3 * 100 * fromIntegral Helpers.simpleTransferCost
                            },
                      metadata = makeDummyHeader accountAddress0 3 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair0)])]
                    },
                  -- transfer 10000 back from account1 to account0
                  TJSON
                    { payload =
                        Transfer
                            { toaddress = accountAddress0,
                              amount = 100 * fromIntegral Helpers.simpleTransferCost
                            },
                      metadata = makeDummyHeader accountAddress1 1 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair1)])]
                    },
                  -- the next transaction should fail because the balance on account0 is now exactly
                  -- enough to cover the transfer cost
                  TJSON
                    { payload = Transfer{toaddress = accountAddress1, amount = 1},
                      metadata = makeDummyHeader accountAddress0 4 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair0)])]
                    }
                ]

        (_, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                transactions
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result state = do
        doCheckBlockStateInvariants <-
            Helpers.assertBlockStateInvariantsH
                state
                (Helpers.srExecutionCosts result)
        maybeAccount0 <- BS.bsoGetAccount state accountAddress0
        maybeAmount0 <- case maybeAccount0 of
            Nothing -> return Nothing
            Just (_, account) -> Just <$> BS.accountAmount account
        maybeAccount1 <- BS.bsoGetAccount state accountAddress1
        maybeAmount1 <- case maybeAccount1 of
            Nothing -> return Nothing
            Just (_, account) -> Just <$> BS.accountAmount account
        return $ do
            let Sch.FilteredTransactions{..} = Helpers.srTransactions result
            let results = Helpers.getResults ftAdded
            assertEqual "There should be no failed transactions." [] ftFailed

            case last results of
                (_, Types.TxReject (Types.AmountTooLarge addr amnt)) -> do
                    assertEqual "Sending from account 0" (Types.AddressAccount accountAddress0) addr
                    assertEqual "Exactly 1 microCCD" 1 amnt
                err -> assertFailure $ "Incorrect result of the last transaction: " ++ show (snd err)

            let nonreject =
                    all
                        ( \case
                            (_, Types.TxSuccess{}) -> True
                            (_, Types.TxReject{}) -> False
                        )
                        (init results)
            assertBool "Initial transactions are accepted." nonreject

            assertEqual "Amount on account0." (Just 0) maybeAmount0
            let transactionCost =
                    5
                        * 100
                        * fromIntegral Helpers.simpleTransferCost

            assertEqual
                ("Amount on account1." ++ show maybeAmount1)
                (Just (2_000_000 - transactionCost))
                maybeAmount1

            doCheckBlockStateInvariants

simpleTransferWithMemoTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
simpleTransferWithMemoTest _ pvString = specify
    (pvString ++ ": simple transfers with memo")
    $ do
        let transactions =
                -- transfer 10000 from A0 to A0
                [ TJSON
                    { payload =
                        TransferWithMemo
                            { twmToAddress = accountAddress0,
                              twmAmount = 10_000,
                              twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]
                            },
                      metadata = makeDummyHeader accountAddress0 1 $ Helpers.simpleTransferCostWithMemo2 4,
                      keys = [(0, [(0, keyPair0)])]
                    },
                  -- transfer 8800 from A to T
                  TJSON
                    { payload =
                        TransferWithMemo
                            { twmToAddress = accountAddress1,
                              twmAmount = 8_800,
                              twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]
                            },
                      metadata = makeDummyHeader accountAddress0 2 $ Helpers.simpleTransferCostWithMemo2 4,
                      keys = [(0, [(0, keyPair0)])]
                    },
                  -- transfer everything from from A0 to A1
                  -- the (100 *) is conversion between NRG and CCD
                  TJSON
                    { payload =
                        TransferWithMemo
                            { twmToAddress = accountAddress1,
                              twmAmount =
                                1_000_000
                                    - 8_800
                                    - 3 * 100 * fromIntegral (Helpers.simpleTransferCostWithMemo2 4),
                              twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]
                            },
                      metadata = makeDummyHeader accountAddress0 3 $ Helpers.simpleTransferCostWithMemo2 4,
                      keys = [(0, [(0, keyPair0)])]
                    },
                  -- transfer 10000 back from A1 to A0
                  TJSON
                    { payload =
                        TransferWithMemo
                            { twmToAddress = accountAddress0,
                              twmAmount = 100 * fromIntegral (Helpers.simpleTransferCostWithMemo2 4),
                              twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]
                            },
                      metadata = makeDummyHeader accountAddress1 1 $ Helpers.simpleTransferCostWithMemo2 4,
                      keys = [(0, [(0, keyPair1)])]
                    },
                  -- the next transaction should fail because the balance on A0 is now exactly enough to cover
                  -- the transfer cost
                  TJSON
                    { payload =
                        TransferWithMemo
                            { twmToAddress = accountAddress1,
                              twmAmount = 1,
                              twmMemo = Types.Memo $ BSS.pack [0, 1, 2, 3]
                            },
                      metadata = makeDummyHeader accountAddress0 4 $ Helpers.simpleTransferCostWithMemo2 4,
                      keys = [(0, [(0, keyPair0)])]
                    }
                ]

        (_, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                transactions
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result state = do
        doCheckBlockStateInvariants <-
            Helpers.assertBlockStateInvariantsH
                state
                (Helpers.srExecutionCosts result)
        maybeAccount0 <- BS.bsoGetAccount state accountAddress0
        maybeAmount0 <- case maybeAccount0 of
            Nothing -> return Nothing
            Just (_, account) -> Just <$> BS.accountAmount account
        maybeAccount1 <- BS.bsoGetAccount state accountAddress1
        maybeAmount1 <- case maybeAccount1 of
            Nothing -> return Nothing
            Just (_, account) -> Just <$> BS.accountAmount account
        return $ do
            let Sch.FilteredTransactions{..} = Helpers.srTransactions result
            let results = Helpers.getResults ftAdded
            assertEqual "There should be no failed transactions." [] ftFailed

            case last results of
                (_, Types.TxReject (Types.AmountTooLarge addr amnt)) -> do
                    assertEqual "Sending from account 0" (Types.AddressAccount accountAddress0) addr
                    assertEqual "Exactly 1 microCCD" 1 amnt
                err -> assertFailure $ "Incorrect result of the last transaction: " ++ show (snd err)

            let nonreject =
                    all
                        ( \case
                            (_, Types.TxSuccess{}) -> True
                            (_, Types.TxReject{}) -> False
                        )
                        (init results)
            assertBool "Initial transactions are accepted." nonreject

            assertEqual "Amount on account0." (Just 0) maybeAmount0
            let transactionCost =
                    5
                        * 100
                        * fromIntegral (Helpers.simpleTransferCostWithMemo2 4)

            assertEqual
                ("Amount on account1." ++ show maybeAmount1)
                (Just (2_000_000 - transactionCost))
                maybeAmount1

            doCheckBlockStateInvariants

simpleTransferUsingAccountAliasesTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    Spec
simpleTransferUsingAccountAliasesTest _ pvString = specify
    (pvString ++ ": 4 successful and 1 failed transaction using aliases")
    $ do
        let transactions =
                -- transfer 10000 from account0 to account0
                [ TJSON
                    { payload = Transfer{toaddress = Types.createAlias accountAddress0 0, amount = 10_000},
                      metadata = makeDummyHeader accountAddress0 1 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair0)])]
                    },
                  -- transfer 8800 from account0 to account1
                  TJSON
                    { payload = Transfer{toaddress = Types.createAlias accountAddress1 0, amount = 8_800},
                      metadata = makeDummyHeader accountAddress0 2 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair0)])]
                    },
                  -- transfer everything from from account0 to account1
                  -- the (100 *) is conversion between NRG and CCD
                  TJSON
                    { payload =
                        Transfer
                            { toaddress = Types.createAlias accountAddress1 1,
                              amount =
                                1_000_000
                                    - 8_800
                                    - 3 * 100 * fromIntegral Helpers.simpleTransferCost
                            },
                      metadata = makeDummyHeader accountAddress0 3 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair0)])]
                    },
                  -- transfer 10000 back from account1 to account0
                  TJSON
                    { payload =
                        Transfer
                            { toaddress = Types.createAlias accountAddress0 1,
                              amount = 100 * fromIntegral Helpers.simpleTransferCost
                            },
                      metadata = makeDummyHeader accountAddress1 1 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair1)])]
                    },
                  -- the next transaction should fail because the balance on account0 is now exactly enough to cover the transfer cost
                  TJSON
                    { payload = Transfer{toaddress = Types.createAlias accountAddress1 2, amount = 1},
                      metadata = makeDummyHeader (Types.createAlias accountAddress0 4) 4 Helpers.simpleTransferCost,
                      keys = [(0, [(0, keyPair0)])]
                    }
                ]

        (_, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                transactions
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result state = do
        doCheckBlockStateInvariants <-
            Helpers.assertBlockStateInvariantsH
                state
                (Helpers.srExecutionCosts result)
        maybeAccount0 <- BS.bsoGetAccount state accountAddress0
        maybeAmount0 <- case maybeAccount0 of
            Nothing -> return Nothing
            Just (_, account) -> Just <$> BS.accountAmount account
        maybeAccount1 <- BS.bsoGetAccount state accountAddress1
        maybeAmount1 <- case maybeAccount1 of
            Nothing -> return Nothing
            Just (_, account) -> Just <$> BS.accountAmount account
        return $ do
            let Sch.FilteredTransactions{..} = Helpers.srTransactions result
            let results = Helpers.getResults ftAdded
            assertEqual "There should be no failed transactions." [] ftFailed

            case last results of
                (_, Types.TxReject (Types.AmountTooLarge addr amnt)) -> do
                    assertEqual "Sending from account0" (Types.AddressAccount (Types.createAlias accountAddress0 4)) addr
                    assertEqual "Exactly 1 microCCD" 1 amnt
                err -> assertFailure $ "Incorrect result of the last transaction: " ++ show (snd err)

            let nonreject =
                    all
                        ( \case
                            (_, Types.TxSuccess{}) -> True
                            (_, Types.TxReject{}) -> False
                        )
                        (init results)
            assertBool "Initial transactions are accepted." nonreject

            assertEqual "Amount on account0." (Just 0) maybeAmount0
            let transactionCost =
                    5
                        * 100
                        * fromIntegral Helpers.simpleTransferCost

            assertEqual
                ("Amount on account1." ++ show maybeAmount1)
                (Just (2_000_000 - transactionCost))
                maybeAmount1

            doCheckBlockStateInvariants

tests :: Spec
tests =
    describe "Simple transfers test." $ do
        transferWithMemoRejectTestP1
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                simpleTransferTest spv pvString
                when (Types.supportsMemo spv) $
                    simpleTransferWithMemoTest spv pvString
                when (Types.supportsAccountAliases spv) $
                    simpleTransferUsingAccountAliasesTest spv pvString
