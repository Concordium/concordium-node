{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.EncryptedTransfersTest where

import qualified Data.ByteString.Short as BSS
import Data.Maybe
import qualified Data.Sequence as Seq

import Concordium.Crypto.EncryptedTransfers
import Concordium.Crypto.FFIDataTypes (ElgamalSecretKey)

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.GlobalState.DummyData
import Concordium.ID.DummyData (dummyEncryptionSecretKey)
import qualified Concordium.ID.Types as ID

import Concordium.Scheduler.DummyData
import qualified Concordium.Scheduler.Runner as Runner
import qualified Concordium.Scheduler.Types as Types

import qualified SchedulerTests.Helpers as Helpers

import Control.Monad
import Test.HUnit
import Test.Hspec

-- This test will perform the following transactions and check that the resulting
-- blockstate is correct:
--
--- |------------------------------------+-----------------+-----------+---------------|
--- | After transaction                  |                 |  Account0 |      Account1 |
--- |------------------------------------+-----------------+-----------+---------------|
--- | A0, PubToSec(1000)                 | selfAmount      |      1000 |             0 |
--- |                                    | startIdx        |         0 |             0 |
--- |                                    | incomingAmounts |        [] |            [] |
--- |------------------------------------+-----------------+-----------+---------------|
--- | A0->A1, Send 100 from self         | selfAmount      |       900 |             0 |
--- |                                    | startIdx        |         0 |             0 |
--- |                                    | incomingAmounts |        [] |         [100] |
--- |------------------------------------+-----------------+-----------+---------------|
--- | A0->A1, Send 100 from self         | selfAmount      |       800 |             0 |
--- |                                    | startIdx        |         0 |             0 |
--- |                                    | incomingAmounts |        [] |     [100,100] |
--- |------------------------------------+-----------------+-----------+---------------|
--- | A0->A1, Send 100 from self         | selfAmount      |       700 |             0 |
--- |                                    | startIdx        |         0 |             0 |
--- |                                    | incomingAmounts |        [] | [100,100,100] |
--- |------------------------------------+-----------------+-----------+---------------|
--- | A1->A0, Send 150 combining up to 2 | selfAmount      |       700 |            50 |
--- |                                    | startIdx        |         0 |             2 |
--- |                                    | incomingAmounts |     [150] |         [100] |
--- |------------------------------------+-----------------+-----------+---------------|
--- | A1->A0, Send 150 combining up to 3 | selfAmount      |       700 |             0 |
--- |                                    | startIdx        |         0 |             3 |
--- |                                    | incomingAmounts | [150,150] |            [] |
--- |------------------------------------+-----------------+-----------+---------------|
--- | A0, SecToPub(650)                  | selfAmount      |        50 |             0 |
--- |                                    | startIdx        |         0 |             3 |
--- |                                    | incomingAmounts | [150,150] |            [] |
--- |------------------------------------+-----------------+-----------+---------------|
--- | A0, SecToPub(150)                  | selfAmount      |        50 |             0 |
--- |                                    | startIdx        |         1 |             3 |
--- |                                    | incomingAmounts |     [150] |            [] |
--- |------------------------------------+-----------------+-----------+---------------|
--- | A0, SecToPub(200)                  | selfAmount      |         0 |             0 |
--- |                                    | startIdx        |         2 |             3 |
--- |                                    | incomingAmounts |        [] |            [] |
--- |------------------------------------+-----------------+-----------+---------------|

tests :: Spec
tests = do
    describe "Encrypted transfers:" $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                testCase0 spv pvString
                testCase1 spv pvString
                testCase2 spv pvString

initialBlockState ::
    Types.IsProtocolVersion pv =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed 10_000_000_000 0,
          Helpers.makeTestAccountFromSeed 10_000_000_000 1
        ]

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

accountAddress1 :: Types.AccountAddress
accountAddress1 = Helpers.accountAddressFromSeed 1

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

keyPair1 :: SigScheme.KeyPair
keyPair1 = Helpers.keyPairFromSeed 1

encryptionSecretKey0 :: ElgamalSecretKey
encryptionSecretKey0 = dummyEncryptionSecretKey dummyCryptographicParameters accountAddress0

encryptionSecretKey1 :: ElgamalSecretKey
encryptionSecretKey1 = dummyEncryptionSecretKey dummyCryptographicParameters accountAddress1

encryptionPublicKey0 :: ID.AccountEncryptionKey
encryptionPublicKey0 =
    ID.makeEncryptionKey dummyCryptographicParameters $
        ID.credId $
            Helpers.makeTestCredentialFromSeed 0

encryptionPublicKey1 :: ID.AccountEncryptionKey
encryptionPublicKey1 =
    ID.makeEncryptionKey dummyCryptographicParameters $
        ID.credId $
            Helpers.makeTestCredentialFromSeed 1

assertEncryptedBalance ::
    Types.IsProtocolVersion pv =>
    Types.AccountEncryptedAmount ->
    Types.AccountAddress ->
    BS.PersistentBlockState pv ->
    Helpers.PersistentBSM pv Assertion
assertEncryptedBalance expectedEncryptedAmount address blockState = do
    maybeAccount <- BS.bsoGetAccount blockState address
    case maybeAccount of
        Nothing -> return $ assertFailure $ "No account found for address: " ++ show address
        Just (_, account) -> do
            accountEncryptedAmount <- BS.accountEncryptedAmount account
            return $
                assertEqual
                    "Encrypted amounts matches"
                    accountEncryptedAmount
                    expectedEncryptedAmount

createEncryptedTransferData ::
    ID.AccountEncryptionKey ->
    ElgamalSecretKey ->
    AggregatedDecryptedAmount ->
    Types.Amount ->
    IO (Maybe EncryptedAmountTransferData)
createEncryptedTransferData (ID.AccountEncryptionKey receiverPK) =
    makeEncryptedAmountTransferData dummyCryptographicParameters receiverPK

createSecToPubTransferData :: ElgamalSecretKey -> AggregatedDecryptedAmount -> Types.Amount -> IO (Maybe SecToPubAmountTransferData)
createSecToPubTransferData = makeSecToPubAmountTransferData dummyCryptographicParameters

-- | Test running a series of encrypted transfers as described in this module documentation.
testCase0 ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
testCase0 _ pvString = specify
    (pvString ++ ": Chain of encrypted transfer")
    $ do
        transactionsAndAssertions <- makeTransactions
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactionsAndAssertions
  where
    makeTransactions :: IO [Helpers.TransactionAndAssertion pv]
    makeTransactions = do
        -- Transaction 1. Pub to sec (1000)
        let encryptedAmount1000 :: EncryptedAmount
            encryptedAmount1000 = encryptAmountZeroRandomness dummyCryptographicParameters 1_000

        -- Transaction 2. EncTransfer (A0->A1, 100) with previous amounts
        let aggregatedDecryptedAmount1 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount1 = makeAggregatedDecryptedAmount encryptedAmount1000 1_000 0

        encryptedTransferData1 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey1
                    encryptionSecretKey0
                    aggregatedDecryptedAmount1
                    100

        let incomingAmounts1 :: Seq.Seq EncryptedAmount
            incomingAmounts1 = Seq.singleton $ eatdTransferAmount encryptedTransferData1

        -- Transaction 3. EncTransfer (A0->A1, 100) with previous amounts
        let aggregatedDecryptedAmount2 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount2 =
                makeAggregatedDecryptedAmount
                    (eatdRemainingAmount encryptedTransferData1)
                    900
                    0

        encryptedTransferData2 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey1
                    encryptionSecretKey0
                    aggregatedDecryptedAmount2
                    100

        let incomingAmounts2 :: Seq.Seq EncryptedAmount
            incomingAmounts2 = incomingAmounts1 Seq.:|> eatdTransferAmount encryptedTransferData2

        -- Transaction 4. EncTransfer (A0->A1, 100) with previous amounts
        let aggregatedDecryptedAmount3 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount3 =
                makeAggregatedDecryptedAmount
                    (eatdRemainingAmount encryptedTransferData2)
                    800
                    0

        encryptedTransferData3 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey1
                    encryptionSecretKey0
                    aggregatedDecryptedAmount3
                    100

        let incomingAmounts3 :: Seq.Seq EncryptedAmount
            incomingAmounts3 = incomingAmounts2 Seq.:|> eatdTransferAmount encryptedTransferData3

        -- Transaction 5. EncTransfer (A1->A0, 150) with previous amounts
        let aggregatedEncryptedAmount4 :: EncryptedAmount
            aggregatedEncryptedAmount4 =
                aggregateAmounts mempty $
                    aggregateAmounts
                        (eatdTransferAmount encryptedTransferData1)
                        (eatdTransferAmount encryptedTransferData2)

            aggregatedDecryptedAmount4 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount4 =
                makeAggregatedDecryptedAmount
                    aggregatedEncryptedAmount4
                    200
                    2

        encryptedTransferData4 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey0
                    encryptionSecretKey1
                    aggregatedDecryptedAmount4
                    150

        let incomingAmounts4account0 :: Seq.Seq EncryptedAmount
            incomingAmounts4account0 = Seq.singleton $ eatdTransferAmount encryptedTransferData4

            incomingAmounts4account1 :: Seq.Seq EncryptedAmount
            incomingAmounts4account1 = Seq.singleton $ eatdTransferAmount encryptedTransferData3

        -- Transaction 6. EncTransfer (T->A, 150) with previous amounts
        let aggregatedEncryptedAmount5 :: EncryptedAmount
            aggregatedEncryptedAmount5 =
                aggregateAmounts
                    (eatdRemainingAmount encryptedTransferData4)
                    (eatdTransferAmount encryptedTransferData3)
            aggregatedDecryptedAmount5 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount5 =
                makeAggregatedDecryptedAmount
                    aggregatedEncryptedAmount5
                    150
                    3

        encryptedTransferData5 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey0
                    encryptionSecretKey1
                    aggregatedDecryptedAmount5
                    150

        let incomingAmounts5account0 :: Seq.Seq EncryptedAmount
            incomingAmounts5account0 =
                incomingAmounts4account0
                    Seq.:|> eatdTransferAmount encryptedTransferData5

            incomingAmounts5account1 :: Seq.Seq EncryptedAmount
            incomingAmounts5account1 = Seq.empty

        -- Transaction 7. Sec to Pub 650 (to not consume fully the selfAmount which is 700 rn)
        let aggregatedDecryptedAmount6 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount6 =
                makeAggregatedDecryptedAmount
                    (eatdRemainingAmount encryptedTransferData3)
                    700
                    0

        secToPubTransferData1 :: SecToPubAmountTransferData <-
            fromJust <$> createSecToPubTransferData encryptionSecretKey0 aggregatedDecryptedAmount6 650

        let incomingAmounts7account0 :: Seq.Seq EncryptedAmount
            incomingAmounts7account0 = Seq.singleton $ eatdTransferAmount encryptedTransferData5

        -- Transaction 8. Sec to Pub 150
        let aggregatedEncryptedAmount7 :: EncryptedAmount
            aggregatedEncryptedAmount7 =
                aggregateAmounts
                    (stpatdRemainingAmount secToPubTransferData1)
                    (eatdTransferAmount encryptedTransferData4)

            aggregatedDecryptedAmount7 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount7 = makeAggregatedDecryptedAmount aggregatedEncryptedAmount7 200 1

        secToPubTransferData2 :: SecToPubAmountTransferData <-
            fromJust
                <$> createSecToPubTransferData
                    encryptionSecretKey0
                    aggregatedDecryptedAmount7
                    150

        -- Transaction 9. Sec to Pub 200
        let aggregatedEncryptedAmount8 :: EncryptedAmount
            aggregatedEncryptedAmount8 =
                aggregateAmounts
                    (stpatdRemainingAmount secToPubTransferData2)
                    (eatdTransferAmount encryptedTransferData5)

            aggregatedDecryptedAmount8 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount8 =
                makeAggregatedDecryptedAmount
                    aggregatedEncryptedAmount8
                    200
                    2

        secToPubTransferData3 :: SecToPubAmountTransferData <-
            fromJust
                <$> createSecToPubTransferData
                    encryptionSecretKey0
                    aggregatedDecryptedAmount8
                    200

        let incomingAmounts8account0 :: Seq.Seq EncryptedAmount
            incomingAmounts8account0 = Seq.empty

        return
            [ Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload = Runner.TransferToEncrypted 1_000,
                          metadata = makeDummyHeader accountAddress0 1 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doInvariantAssertions <-
                        Helpers.assertBlockStateInvariantsH
                            state
                            srExecutionCosts
                    doEncryptedBalanceAssertions <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = encryptedAmount1000
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct encrypt self amount event is produced"
                                    [ Types.EncryptedSelfAmountAdded
                                        { eaaAccount = accountAddress0,
                                          eaaNewAmount = encryptedAmount1000,
                                          eaaAmount = 1_000
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "First transaction should succeed"
                        doInvariantAssertions
                        doEncryptedBalanceAssertions
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload =
                            Runner.EncryptedAmountTransfer
                                accountAddress1
                                encryptedTransferData1,
                          metadata = makeDummyHeader accountAddress0 2 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertionSender <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData1
                                }
                            accountAddress0
                            state
                    doEncryptedBalanceAssertionReceiver <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts =
                                    incomingAmounts1
                                }
                            accountAddress1
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 0,
                                          earNewAmount = eatdRemainingAmount encryptedTransferData1,
                                          earInputAmount = encryptedAmount1000
                                        },
                                      Types.NewEncryptedAmount
                                        { neaAccount = accountAddress1,
                                          neaNewIndex = 0,
                                          neaEncryptedAmount =
                                            eatdTransferAmount
                                                encryptedTransferData1
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Third transaction should succeed"
                        doEncryptedBalanceAssertionSender
                        doEncryptedBalanceAssertionReceiver
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload =
                            Runner.EncryptedAmountTransfer
                                accountAddress1
                                encryptedTransferData2,
                          metadata = makeDummyHeader accountAddress0 3 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertionSender <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData2
                                }
                            accountAddress0
                            state
                    doEncryptedBalanceAssertionReceiver <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts = incomingAmounts2
                                }
                            accountAddress1
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 0,
                                          earNewAmount = eatdRemainingAmount encryptedTransferData2,
                                          earInputAmount =
                                            eatdRemainingAmount
                                                encryptedTransferData1
                                        },
                                      Types.NewEncryptedAmount
                                        { neaAccount = accountAddress1,
                                          neaNewIndex = 1,
                                          neaEncryptedAmount =
                                            eatdTransferAmount
                                                encryptedTransferData2
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Forth transaction should succeed"
                        doEncryptedBalanceAssertionSender
                        doEncryptedBalanceAssertionReceiver
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload =
                            Runner.EncryptedAmountTransfer
                                accountAddress1
                                encryptedTransferData3,
                          metadata = makeDummyHeader accountAddress0 4 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertionSender <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData3
                                }
                            accountAddress0
                            state
                    doEncryptedBalanceAssertionReceiver <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts = incomingAmounts3
                                }
                            accountAddress1
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 0,
                                          earNewAmount = eatdRemainingAmount encryptedTransferData3,
                                          earInputAmount =
                                            eatdRemainingAmount
                                                encryptedTransferData2
                                        },
                                      Types.NewEncryptedAmount
                                        { neaAccount = accountAddress1,
                                          neaNewIndex = 2,
                                          neaEncryptedAmount =
                                            eatdTransferAmount
                                                encryptedTransferData3
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Fifth transaction should succeed"
                        doEncryptedBalanceAssertionSender
                        doEncryptedBalanceAssertionReceiver
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload = Runner.EncryptedAmountTransfer accountAddress0 encryptedTransferData4,
                          metadata = makeDummyHeader accountAddress1 1 100_000,
                          keys = [(0, [(0, keyPair1)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertionSender <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData4,
                                  Types._startIndex = 2,
                                  Types._incomingEncryptedAmounts = incomingAmounts4account1
                                }
                            accountAddress1
                            state
                    doEncryptedBalanceAssertionReceiver <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData3,
                                  Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts = incomingAmounts4account0
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress1,
                                          earUpToIndex = 2,
                                          earNewAmount = eatdRemainingAmount encryptedTransferData4,
                                          earInputAmount = aggregatedEncryptedAmount4
                                        },
                                      Types.NewEncryptedAmount
                                        { neaAccount = accountAddress0,
                                          neaNewIndex = 0,
                                          neaEncryptedAmount =
                                            eatdTransferAmount
                                                encryptedTransferData4
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Sixth transaction should succeed"
                        doEncryptedBalanceAssertionSender
                        doEncryptedBalanceAssertionReceiver
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload =
                            Runner.EncryptedAmountTransfer
                                accountAddress0
                                encryptedTransferData5,
                          metadata = makeDummyHeader accountAddress1 2 100_000,
                          keys = [(0, [(0, keyPair1)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertionSender <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData5,
                                  Types._startIndex = 3,
                                  Types._incomingEncryptedAmounts = incomingAmounts5account1
                                }
                            accountAddress1
                            state
                    doEncryptedBalanceAssertionReceiver <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData3,
                                  Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts = incomingAmounts5account0
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress1,
                                          earUpToIndex = 3,
                                          earNewAmount = eatdRemainingAmount encryptedTransferData5,
                                          earInputAmount = aggregatedEncryptedAmount5
                                        },
                                      Types.NewEncryptedAmount
                                        { neaAccount = accountAddress0,
                                          neaNewIndex = 1,
                                          neaEncryptedAmount =
                                            eatdTransferAmount
                                                encryptedTransferData5
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Seventh transaction should succeed"
                        doEncryptedBalanceAssertionSender
                        doEncryptedBalanceAssertionReceiver
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload = Runner.TransferToPublic secToPubTransferData1,
                          metadata = makeDummyHeader accountAddress0 5 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertion <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = stpatdRemainingAmount secToPubTransferData1,
                                  Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts = incomingAmounts5account0
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 0,
                                          earNewAmount =
                                            stpatdRemainingAmount
                                                secToPubTransferData1,
                                          earInputAmount =
                                            eatdRemainingAmount
                                                encryptedTransferData3
                                        },
                                      Types.AmountAddedByDecryption
                                        { aabdAccount = accountAddress0,
                                          aabdAmount = 650
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Eigth transaction should succeed"
                        doEncryptedBalanceAssertion
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload = Runner.TransferToPublic secToPubTransferData2,
                          metadata = makeDummyHeader accountAddress0 6 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertion <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = stpatdRemainingAmount secToPubTransferData2,
                                  Types._startIndex = 1,
                                  Types._incomingEncryptedAmounts = incomingAmounts7account0
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 1,
                                          earNewAmount =
                                            stpatdRemainingAmount
                                                secToPubTransferData2,
                                          earInputAmount = aggregatedEncryptedAmount7
                                        },
                                      Types.AmountAddedByDecryption
                                        { aabdAccount = accountAddress0,
                                          aabdAmount = 150
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Nineth transaction should succeed"
                        doEncryptedBalanceAssertion
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload = Runner.TransferToPublic secToPubTransferData3,
                          metadata = makeDummyHeader accountAddress0 7 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertion <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = stpatdRemainingAmount secToPubTransferData3,
                                  Types._startIndex = 2,
                                  Types._incomingEncryptedAmounts = incomingAmounts8account0
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 2,
                                          earNewAmount =
                                            stpatdRemainingAmount
                                                secToPubTransferData3,
                                          earInputAmount = aggregatedEncryptedAmount8
                                        },
                                      Types.AmountAddedByDecryption
                                        { aabdAccount = accountAddress0,
                                          aabdAmount = 200
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Tenth transaction should succeed"
                        doEncryptedBalanceAssertion
                }
            ]

-- | Test ensuring an encrypted transfer with memo fails correctly in protocol versions not
-- supporting memos.
testCase1 ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
testCase1 spv pvString =
    unless (Types.supportsMemo spv)
        $ specify
            (pvString ++ ": Fail deserializing encrypted transfer with memo")
        $ do
            transactionsAndAssertions <- makeTransactions
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    makeTransactions :: IO [Helpers.TransactionAndAssertion pv]
    makeTransactions = do
        -- Transaction 1. Pub to sec (1000)
        let encryptedAmount1000 :: EncryptedAmount
            encryptedAmount1000 = encryptAmountZeroRandomness dummyCryptographicParameters 1_000

        -- Transaction 2. EncTransfer (A0->A1, 100) with previous amounts
        let aggregatedDecryptedAmount1 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount1 = makeAggregatedDecryptedAmount encryptedAmount1000 1_000 0

        encryptedTransferData1 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey1
                    encryptionSecretKey0
                    aggregatedDecryptedAmount1
                    100

        let memo = Types.Memo $ BSS.pack [0, 1, 2, 3]

        return
            [ Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload = Runner.TransferToEncrypted 1_000,
                          metadata = makeDummyHeader accountAddress0 1 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doInvariantAssertions <-
                        Helpers.assertBlockStateInvariantsH
                            state
                            srExecutionCosts
                    doEncryptedBalanceAssertions <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = encryptedAmount1000
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct encrypt self amount event is produced"
                                    [ Types.EncryptedSelfAmountAdded
                                        { eaaAccount = accountAddress0,
                                          eaaNewAmount = encryptedAmount1000,
                                          eaaAmount = 1_000
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "First transaction should succeed"
                        doInvariantAssertions
                        doEncryptedBalanceAssertions
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload =
                            Runner.EncryptedAmountTransferWithMemo
                                accountAddress1
                                memo
                                encryptedTransferData1,
                          metadata = makeDummyHeader accountAddress0 2 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertions <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = encryptedAmount1000
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxReject{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    Types.SerializationFailure
                                    vrRejectReason
                            _ -> assertFailure "Second transaction should reject"
                        doEncryptedBalanceAssertions
                }
            ]

-- | Test running a series of encrypted transfers with memo as described in this module
-- documentation.
testCase2 ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
testCase2 spv pvString =
    when (Types.supportsMemo spv)
        $ specify
            (pvString ++ ": Chain of encrypted transfer with memo")
        $ do
            transactionsAndAssertions <- makeTransactions
            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                Helpers.defaultTestConfig
                initialBlockState
                transactionsAndAssertions
  where
    makeTransactions = do
        -- Transaction 1. Pub to sec (1000)
        let encryptedAmount1000 :: EncryptedAmount
            encryptedAmount1000 = encryptAmountZeroRandomness dummyCryptographicParameters 1_000

        -- Transaction 2. EncTransfer (A0->A1, 100) with previous amounts
        let aggregatedDecryptedAmount1 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount1 = makeAggregatedDecryptedAmount encryptedAmount1000 1_000 0

        encryptedTransferData1 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey1
                    encryptionSecretKey0
                    aggregatedDecryptedAmount1
                    100

        let incomingAmounts1 :: Seq.Seq EncryptedAmount
            incomingAmounts1 = Seq.singleton $ eatdTransferAmount encryptedTransferData1

        -- Transaction 3. EncTransfer (A0->A1, 100) with previous amounts
        let aggregatedDecryptedAmount2 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount2 =
                makeAggregatedDecryptedAmount
                    (eatdRemainingAmount encryptedTransferData1)
                    900
                    0

        encryptedTransferData2 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey1
                    encryptionSecretKey0
                    aggregatedDecryptedAmount2
                    100

        let incomingAmounts2 :: Seq.Seq EncryptedAmount
            incomingAmounts2 = incomingAmounts1 Seq.:|> eatdTransferAmount encryptedTransferData2

        -- Transaction 4. EncTransfer (A0->A1, 100) with previous amounts
        let aggregatedDecryptedAmount3 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount3 =
                makeAggregatedDecryptedAmount
                    (eatdRemainingAmount encryptedTransferData2)
                    800
                    0

        encryptedTransferData3 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey1
                    encryptionSecretKey0
                    aggregatedDecryptedAmount3
                    100

        let incomingAmounts3 :: Seq.Seq EncryptedAmount
            incomingAmounts3 = incomingAmounts2 Seq.:|> eatdTransferAmount encryptedTransferData3

        -- Transaction 5. EncTransfer (A1->A0, 150) with previous amounts
        let aggregatedEncryptedAmount4 :: EncryptedAmount
            aggregatedEncryptedAmount4 =
                aggregateAmounts mempty $
                    aggregateAmounts
                        (eatdTransferAmount encryptedTransferData1)
                        (eatdTransferAmount encryptedTransferData2)

            aggregatedDecryptedAmount4 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount4 =
                makeAggregatedDecryptedAmount
                    aggregatedEncryptedAmount4
                    200
                    2

        encryptedTransferData4 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey0
                    encryptionSecretKey1
                    aggregatedDecryptedAmount4
                    150

        let incomingAmounts4account0 :: Seq.Seq EncryptedAmount
            incomingAmounts4account0 = Seq.singleton $ eatdTransferAmount encryptedTransferData4

            incomingAmounts4account1 :: Seq.Seq EncryptedAmount
            incomingAmounts4account1 = Seq.singleton $ eatdTransferAmount encryptedTransferData3

        -- Transaction 6. EncTransfer (T->A, 150) with previous amounts
        let aggregatedEncryptedAmount5 :: EncryptedAmount
            aggregatedEncryptedAmount5 =
                aggregateAmounts
                    (eatdRemainingAmount encryptedTransferData4)
                    (eatdTransferAmount encryptedTransferData3)
            aggregatedDecryptedAmount5 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount5 =
                makeAggregatedDecryptedAmount
                    aggregatedEncryptedAmount5
                    150
                    3

        encryptedTransferData5 :: EncryptedAmountTransferData <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey0
                    encryptionSecretKey1
                    aggregatedDecryptedAmount5
                    150

        let incomingAmounts5account0 :: Seq.Seq EncryptedAmount
            incomingAmounts5account0 =
                incomingAmounts4account0
                    Seq.:|> eatdTransferAmount encryptedTransferData5

            incomingAmounts5account1 :: Seq.Seq EncryptedAmount
            incomingAmounts5account1 = Seq.empty

        -- Transaction 7. Sec to Pub 650 (to not consume fully the selfAmount which is 700 rn)
        let aggregatedDecryptedAmount6 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount6 =
                makeAggregatedDecryptedAmount
                    (eatdRemainingAmount encryptedTransferData3)
                    700
                    0

        secToPubTransferData1 :: SecToPubAmountTransferData <-
            fromJust <$> createSecToPubTransferData encryptionSecretKey0 aggregatedDecryptedAmount6 650

        let incomingAmounts7account0 :: Seq.Seq EncryptedAmount
            incomingAmounts7account0 = Seq.singleton $ eatdTransferAmount encryptedTransferData5

        -- Transaction 8. Sec to Pub 150
        let aggregatedEncryptedAmount7 :: EncryptedAmount
            aggregatedEncryptedAmount7 =
                aggregateAmounts
                    (stpatdRemainingAmount secToPubTransferData1)
                    (eatdTransferAmount encryptedTransferData4)

            aggregatedDecryptedAmount7 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount7 = makeAggregatedDecryptedAmount aggregatedEncryptedAmount7 200 1

        secToPubTransferData2 :: SecToPubAmountTransferData <-
            fromJust
                <$> createSecToPubTransferData
                    encryptionSecretKey0
                    aggregatedDecryptedAmount7
                    150

        -- Transaction 9. Sec to Pub 200
        let aggregatedEncryptedAmount8 :: EncryptedAmount
            aggregatedEncryptedAmount8 =
                aggregateAmounts
                    (stpatdRemainingAmount secToPubTransferData2)
                    (eatdTransferAmount encryptedTransferData5)

            aggregatedDecryptedAmount8 :: AggregatedDecryptedAmount
            aggregatedDecryptedAmount8 =
                makeAggregatedDecryptedAmount
                    aggregatedEncryptedAmount8
                    200
                    2

        secToPubTransferData3 :: SecToPubAmountTransferData <-
            fromJust
                <$> createSecToPubTransferData
                    encryptionSecretKey0
                    aggregatedDecryptedAmount8
                    200

        let incomingAmounts8account0 :: Seq.Seq EncryptedAmount
            incomingAmounts8account0 = Seq.empty

        let memo = Types.Memo $ BSS.pack [0, 1, 2, 3]

        return
            [ Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload = Runner.TransferToEncrypted 1_000,
                          metadata = makeDummyHeader accountAddress0 1 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doInvariantAssertions <-
                        Helpers.assertBlockStateInvariantsH
                            state
                            srExecutionCosts
                    doEncryptedBalanceAssertions <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = encryptedAmount1000
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct encrypt self amount event is produced"
                                    [ Types.EncryptedSelfAmountAdded
                                        { eaaAccount = accountAddress0,
                                          eaaNewAmount = encryptedAmount1000,
                                          eaaAmount = 1_000
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "First transaction should succeed"
                        doInvariantAssertions
                        doEncryptedBalanceAssertions
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload =
                            Runner.EncryptedAmountTransferWithMemo
                                accountAddress1
                                memo
                                encryptedTransferData1,
                          metadata = makeDummyHeader accountAddress0 2 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertionSender <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData1
                                }
                            accountAddress0
                            state
                    doEncryptedBalanceAssertionReceiver <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts =
                                    incomingAmounts1
                                }
                            accountAddress1
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 0,
                                          earNewAmount = eatdRemainingAmount encryptedTransferData1,
                                          earInputAmount = encryptedAmount1000
                                        },
                                      Types.NewEncryptedAmount
                                        { neaAccount = accountAddress1,
                                          neaNewIndex = 0,
                                          neaEncryptedAmount =
                                            eatdTransferAmount
                                                encryptedTransferData1
                                        },
                                      Types.TransferMemo memo
                                    ]
                                    vrEvents
                            _ -> assertFailure "Third transaction should succeed"
                        doEncryptedBalanceAssertionSender
                        doEncryptedBalanceAssertionReceiver
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload =
                            Runner.EncryptedAmountTransferWithMemo
                                accountAddress1
                                memo
                                encryptedTransferData2,
                          metadata = makeDummyHeader accountAddress0 3 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertionSender <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData2
                                }
                            accountAddress0
                            state
                    doEncryptedBalanceAssertionReceiver <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts = incomingAmounts2
                                }
                            accountAddress1
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 0,
                                          earNewAmount = eatdRemainingAmount encryptedTransferData2,
                                          earInputAmount =
                                            eatdRemainingAmount
                                                encryptedTransferData1
                                        },
                                      Types.NewEncryptedAmount
                                        { neaAccount = accountAddress1,
                                          neaNewIndex = 1,
                                          neaEncryptedAmount =
                                            eatdTransferAmount
                                                encryptedTransferData2
                                        },
                                      Types.TransferMemo memo
                                    ]
                                    vrEvents
                            _ -> assertFailure "Forth transaction should succeed"
                        doEncryptedBalanceAssertionSender
                        doEncryptedBalanceAssertionReceiver
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload =
                            Runner.EncryptedAmountTransferWithMemo
                                accountAddress1
                                memo
                                encryptedTransferData3,
                          metadata = makeDummyHeader accountAddress0 4 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertionSender <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData3
                                }
                            accountAddress0
                            state
                    doEncryptedBalanceAssertionReceiver <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts = incomingAmounts3
                                }
                            accountAddress1
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 0,
                                          earNewAmount = eatdRemainingAmount encryptedTransferData3,
                                          earInputAmount =
                                            eatdRemainingAmount
                                                encryptedTransferData2
                                        },
                                      Types.NewEncryptedAmount
                                        { neaAccount = accountAddress1,
                                          neaNewIndex = 2,
                                          neaEncryptedAmount =
                                            eatdTransferAmount
                                                encryptedTransferData3
                                        },
                                      Types.TransferMemo memo
                                    ]
                                    vrEvents
                            _ -> assertFailure "Fifth transaction should succeed"
                        doEncryptedBalanceAssertionSender
                        doEncryptedBalanceAssertionReceiver
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload =
                            Runner.EncryptedAmountTransferWithMemo
                                accountAddress0
                                memo
                                encryptedTransferData4,
                          metadata = makeDummyHeader accountAddress1 1 100_000,
                          keys = [(0, [(0, keyPair1)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertionSender <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData4,
                                  Types._startIndex = 2,
                                  Types._incomingEncryptedAmounts = incomingAmounts4account1
                                }
                            accountAddress1
                            state
                    doEncryptedBalanceAssertionReceiver <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData3,
                                  Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts = incomingAmounts4account0
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress1,
                                          earUpToIndex = 2,
                                          earNewAmount = eatdRemainingAmount encryptedTransferData4,
                                          earInputAmount = aggregatedEncryptedAmount4
                                        },
                                      Types.NewEncryptedAmount
                                        { neaAccount = accountAddress0,
                                          neaNewIndex = 0,
                                          neaEncryptedAmount =
                                            eatdTransferAmount
                                                encryptedTransferData4
                                        },
                                      Types.TransferMemo memo
                                    ]
                                    vrEvents
                            _ -> assertFailure "Sixth transaction should succeed"
                        doEncryptedBalanceAssertionSender
                        doEncryptedBalanceAssertionReceiver
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload =
                            Runner.EncryptedAmountTransferWithMemo
                                accountAddress0
                                memo
                                encryptedTransferData5,
                          metadata = makeDummyHeader accountAddress1 2 100_000,
                          keys = [(0, [(0, keyPair1)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertionSender <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData5,
                                  Types._startIndex = 3,
                                  Types._incomingEncryptedAmounts = incomingAmounts5account1
                                }
                            accountAddress1
                            state
                    doEncryptedBalanceAssertionReceiver <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = eatdRemainingAmount encryptedTransferData3,
                                  Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts = incomingAmounts5account0
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress1,
                                          earUpToIndex = 3,
                                          earNewAmount = eatdRemainingAmount encryptedTransferData5,
                                          earInputAmount = aggregatedEncryptedAmount5
                                        },
                                      Types.NewEncryptedAmount
                                        { neaAccount = accountAddress0,
                                          neaNewIndex = 1,
                                          neaEncryptedAmount =
                                            eatdTransferAmount
                                                encryptedTransferData5
                                        },
                                      Types.TransferMemo memo
                                    ]
                                    vrEvents
                            _ -> assertFailure "Seventh transaction should succeed"
                        doEncryptedBalanceAssertionSender
                        doEncryptedBalanceAssertionReceiver
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload = Runner.TransferToPublic secToPubTransferData1,
                          metadata = makeDummyHeader accountAddress0 5 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertion <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = stpatdRemainingAmount secToPubTransferData1,
                                  Types._startIndex = 0,
                                  Types._incomingEncryptedAmounts = incomingAmounts5account0
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 0,
                                          earNewAmount =
                                            stpatdRemainingAmount
                                                secToPubTransferData1,
                                          earInputAmount =
                                            eatdRemainingAmount
                                                encryptedTransferData3
                                        },
                                      Types.AmountAddedByDecryption
                                        { aabdAccount = accountAddress0,
                                          aabdAmount = 650
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Eigth transaction should succeed"
                        doEncryptedBalanceAssertion
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload = Runner.TransferToPublic secToPubTransferData2,
                          metadata = makeDummyHeader accountAddress0 6 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertion <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = stpatdRemainingAmount secToPubTransferData2,
                                  Types._startIndex = 1,
                                  Types._incomingEncryptedAmounts = incomingAmounts7account0
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 1,
                                          earNewAmount =
                                            stpatdRemainingAmount
                                                secToPubTransferData2,
                                          earInputAmount = aggregatedEncryptedAmount7
                                        },
                                      Types.AmountAddedByDecryption
                                        { aabdAccount = accountAddress0,
                                          aabdAmount = 150
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Nineth transaction should succeed"
                        doEncryptedBalanceAssertion
                },
              Helpers.TransactionAndAssertion
                { taaTransaction =
                    Runner.TJSON
                        { payload = Runner.TransferToPublic secToPubTransferData3,
                          metadata = makeDummyHeader accountAddress0 7 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        },
                  taaAssertion = \Helpers.SchedulerResult{..} state -> do
                    doEncryptedBalanceAssertion <-
                        assertEncryptedBalance
                            Types.initialAccountEncryptedAmount
                                { Types._selfAmount = stpatdRemainingAmount secToPubTransferData3,
                                  Types._startIndex = 2,
                                  Types._incomingEncryptedAmounts = incomingAmounts8account0
                                }
                            accountAddress0
                            state
                    return $ do
                        case Helpers.getResults $ Sch.ftAdded srTransactions of
                            [(_, Types.TxSuccess{..})] ->
                                assertEqual
                                    "The correct events are produced"
                                    [ Types.EncryptedAmountsRemoved
                                        { earAccount = accountAddress0,
                                          earUpToIndex = 2,
                                          earNewAmount =
                                            stpatdRemainingAmount
                                                secToPubTransferData3,
                                          earInputAmount = aggregatedEncryptedAmount8
                                        },
                                      Types.AmountAddedByDecryption
                                        { aabdAccount = accountAddress0,
                                          aabdAmount = 200
                                        }
                                    ]
                                    vrEvents
                            _ -> assertFailure "Tenth transaction should succeed"
                        doEncryptedBalanceAssertion
                }
            ]
