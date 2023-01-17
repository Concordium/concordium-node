{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.MaxIncomingAmountsTest (tests) where

{-
This test will:
- send 1000 tokens to Ales' private balance
- send numberOfTransactions encrypted transfers of 10 tokens to Thomas' account
- send (numberOfTransactions * 10) tokens from Thomas' private balance to his public balance

As `maxNumIncoming` is the maximum number of incoming amounts that an
account can hold before combining the ones that are first in the list, this
will cross this threshold and test that the initial incoming amounts are
actually combined and the amounts are right.

For now `numberOfTransactions == maxNumIncoming + 2`.
-}

import Data.Foldable
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Test.HUnit
import Test.Hspec

import Concordium.Constants
import Concordium.Crypto.EncryptedTransfers
import Concordium.Crypto.FFIDataTypes (ElgamalSecretKey)
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.ID.DummyData (dummyEncryptionSecretKey)
import Concordium.ID.Types (AccountEncryptionKey (..))
import qualified Concordium.ID.Types as ID
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import qualified Concordium.Scheduler.Runner as Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types

import qualified SchedulerTests.Helpers as Helpers

tests :: Spec
tests = do
    describe "Encrypted transfers with aggregation." $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                testCase0 spv pvString

testCase0 ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
testCase0 _ pvString = specify
    (pvString ++ ": Makes a chain of encrypted transfers testing maxNumIncoming")
    $ do
        transactionsAndAssertions <- makeTransactionsAndAssertions
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactionsAndAssertions
  where
    makeTransactionsAndAssertions :: IO [Helpers.TransactionAndAssertion pv]
    makeTransactionsAndAssertions = do
        -- Transaction 1. Pub to sec (1000)
        let encryptedAmount1000 :: EncryptedAmount
            encryptedAmount1000 = encryptAmountZeroRandomness dummyCryptographicParameters 1_000

        encryptedTransferData1 <-
            fromJust
                <$> createEncryptedTransferData
                    encryptionPublicKey1
                    encryptionSecretKey0
                    (makeAggregatedDecryptedAmount encryptedAmount1000 1_000 0)
                    10

        allTransferData <-
            fmap fst
                <$> iterateLimitM
                    numberOfTransactions
                    makeNext
                    (encryptedTransferData1, 990)
        -- A normal transaction is a transfer before maxNumIncoming amounts have been received.
        let (normal, interesting) = splitAt maxNumIncoming $ zip [1 ..] allTransferData

        let generatedTransactions =
                ( fmap
                    ( \(idx, x) ->
                        makeTransaction
                            (makeNormalBlockStateAssertion allTransferData x idx)
                            x
                            idx
                    )
                    normal
                )
                    ++ fmap
                        ( \(idx, x) ->
                            makeTransaction
                                (makeInterestingBlockStateAssertion allTransferData x idx)
                                x
                                idx
                        )
                        interesting

        -- Transaction that will transfer 340 to account1 public balancesecToPubTransferData.

        let allAmounts = foldl' aggregateAmounts mempty (eatdTransferAmount <$> allTransferData)
            aggAmount =
                makeAggregatedDecryptedAmount
                    allAmounts
                    (numberOfTransactions * 10)
                    numberOfTransactions

        secToPubTransferData <-
            fromJust
                <$> createSecToPubTransferData
                    encryptionSecretKey1
                    aggAmount
                    (numberOfTransactions * 10)

        return $
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
                            [(_, Types.TxSuccess events)] ->
                                assertEqual
                                    "The correct encrypt self amount event is produced"
                                    [ Types.EncryptedSelfAmountAdded
                                        { eaaAccount = accountAddress0,
                                          eaaNewAmount = encryptedAmount1000,
                                          eaaAmount = 1_000
                                        }
                                    ]
                                    events
                            _ -> assertFailure "First transaction should succeed"
                        doInvariantAssertions
                        doEncryptedBalanceAssertions
                }
            ]
                -- Now send 34 transactions of 10 tokens from account0 to account1
                ++ generatedTransactions
                -- Send the encrypted 340 tokens on account1 to public balance
                ++ [ Helpers.TransactionAndAssertion
                        { taaTransaction =
                            Runner.TJSON
                                { payload = Runner.TransferToPublic secToPubTransferData,
                                  metadata = makeDummyHeader accountAddress1 1 100_000,
                                  keys = [(0, [(0, keyPair1)])]
                                },
                          taaAssertion = \Helpers.SchedulerResult{..} state -> do
                            doEncryptedBalanceAssertions <-
                                assertEncryptedBalance
                                    Types.initialAccountEncryptedAmount
                                        { _selfAmount = stpatdRemainingAmount secToPubTransferData,
                                          _startIndex = numberOfTransactions,
                                          _incomingEncryptedAmounts = Seq.empty
                                        }
                                    accountAddress1
                                    state
                            return $ do
                                case Helpers.getResults $ Sch.ftAdded srTransactions of
                                    [ ( _,
                                        Types.TxSuccess
                                            [ Types.EncryptedAmountsRemoved{..},
                                              Types.AmountAddedByDecryption{..}
                                                ]
                                        )
                                        ] -> do
                                            assertEqual
                                                "Account encrypted amounts removed"
                                                earAccount
                                                accountAddress1
                                            assertEqual
                                                "Used up indices"
                                                earUpToIndex
                                                numberOfTransactions
                                            assertEqual "New amount" earNewAmount $
                                                stpatdRemainingAmount secToPubTransferData
                                            assertEqual
                                                "Decryption address"
                                                aabdAccount
                                                accountAddress1
                                            assertEqual "Amount added" aabdAmount $
                                                numberOfTransactions * 10
                                    [(_, Types.TxSuccess e)] ->
                                        assertFailure $ "Unexpected final outcome: " ++ show e
                                    other ->
                                        assertFailure $
                                            "Last transaction should succeed: " ++ show other
                                doEncryptedBalanceAssertions
                        }
                   ]

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

encryptionPublicKey1 :: ID.AccountEncryptionKey
encryptionPublicKey1 =
    ID.makeEncryptionKey dummyCryptographicParameters $
        ID.credId $
            Helpers.makeTestCredentialFromSeed 1

iterateLimitM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateLimitM n f = go 0
  where
    go m x
        | m == n = return []
        | otherwise = do
            y <- f x
            (x :) <$> go (m + 1) y

-- Helpers for creating the transfer datas
createEncryptedTransferData ::
    -- | Public key of the receiver
    AccountEncryptionKey ->
    -- | Secret key of the sender
    ElgamalSecretKey ->
    -- | Amount to use as input.
    AggregatedDecryptedAmount ->
    -- | Amount to send.
    Amount ->
    IO (Maybe EncryptedAmountTransferData)
createEncryptedTransferData (AccountEncryptionKey receiverPK) =
    makeEncryptedAmountTransferData dummyCryptographicParameters receiverPK

createSecToPubTransferData :: ElgamalSecretKey -> AggregatedDecryptedAmount -> Types.Amount -> IO (Maybe SecToPubAmountTransferData)
createSecToPubTransferData = makeSecToPubAmountTransferData dummyCryptographicParameters

-- Helper for checking the encrypted balance of an account.
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

numberOfTransactions :: Num a => a
numberOfTransactions = fromIntegral maxNumIncoming + 2

-- Function used to generate all the transfer datas based on encryptedTransferData1
-- each transaction sends 10 tokens from Ales' self amount to Thomas
makeNext :: (EncryptedAmountTransferData, Amount) -> IO (EncryptedAmountTransferData, Amount)
makeNext (d, a) = do
    newData <-
        fromJust
            <$> createEncryptedTransferData
                encryptionPublicKey1
                encryptionSecretKey0
                (makeAggregatedDecryptedAmount (eatdRemainingAmount d) a 0)
                10
    return (newData, a - 10)

makeNormalBlockStateAssertion ::
    (Types.IsProtocolVersion pv) =>
    [EncryptedAmountTransferData] ->
    EncryptedAmountTransferData ->
    EncryptedAmountAggIndex ->
    Helpers.TransactionAssertion pv
makeNormalBlockStateAssertion allTxs transferData idx = \_ state -> do
    -- Account0 amount should be the remaining amount on the transaction
    doEncryptedBalanceAssertionsSender <-
        assertEncryptedBalance
            initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount transferData}
            accountAddress0
            state
    doEncryptedBalanceAssertionsReceiver <-
        assertEncryptedBalance
            initialAccountEncryptedAmount
                { -- no amounts have been used on account1.
                  _startIndex = 0,
                  -- It should have all the received amounts until this index.
                  _incomingEncryptedAmounts =
                    Seq.fromList $ take (fromIntegral idx) (eatdTransferAmount <$> allTxs)
                }
            accountAddress1
            state

    return $ do
        doEncryptedBalanceAssertionsSender
        doEncryptedBalanceAssertionsReceiver

-- An interesting transaction is a transfer after maxNumIncoming amounts have been received.
makeInterestingBlockStateAssertion ::
    (Types.IsProtocolVersion pv) =>
    [EncryptedAmountTransferData] ->
    EncryptedAmountTransferData ->
    EncryptedAmountAggIndex ->
    Helpers.TransactionAssertion pv
makeInterestingBlockStateAssertion allTxs transferData idx = \_ state -> do
    -- Account0 amount should be the remaining amount on the transaction
    doEncryptedBalanceAssertionsSender <-
        assertEncryptedBalance
            initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount transferData}
            accountAddress0
            state

    let (combined, kept) =
            Seq.splitAt
                ((fromIntegral idx - fromIntegral maxNumIncoming) + 1)
                -- get which amounts should be combined into combinedAmount
                (Seq.fromList $ take (fromIntegral idx) (eatdTransferAmount <$> allTxs))

        combinedAmount = foldl' aggregateAmounts mempty combined -- combine them
    doEncryptedBalanceAssertionsReceiver <-
        assertEncryptedBalance
            initialAccountEncryptedAmount
                { -- the start index should have increased
                  _startIndex = idx - fromIntegral maxNumIncoming,
                  -- the list of incoming amounts will hold the `rest` of the amounts
                  _incomingEncryptedAmounts = kept,
                  -- the combined amount goes into the `_aggregatedAmount` field together with the
                  -- number of aggregated amounts until this point.
                  _aggregatedAmount =
                    Just
                        ( combinedAmount,
                          fromIntegral (idx - fromIntegral maxNumIncoming + 1)
                        )
                }
            accountAddress1
            state
    return $ do
        doEncryptedBalanceAssertionsSender
        doEncryptedBalanceAssertionsReceiver

makeTransaction ::
    (Types.IsProtocolVersion pv) =>
    Helpers.TransactionAssertion pv ->
    EncryptedAmountTransferData ->
    EncryptedAmountAggIndex ->
    Helpers.TransactionAndAssertion pv
makeTransaction blockStateChecks transferData idx =
    Helpers.TransactionAndAssertion
        { taaTransaction =
            Runner.TJSON
                { payload = Runner.EncryptedAmountTransfer accountAddress1 transferData, -- create an encrypted transfer to account1
                  metadata = makeDummyHeader accountAddress0 (fromIntegral idx + 1) 100_000, -- from account0 with nonce idx + 1
                  keys = [(0, [(0, keyPair0)])]
                },
          taaAssertion = \result state -> do
            doBlockStateChecks <- blockStateChecks result state
            return $ do
                case Helpers.getResults $ Sch.ftAdded $ Helpers.srTransactions result of
                    [(_, Types.TxSuccess events)] ->
                        case events of
                            [Types.EncryptedAmountsRemoved{..}, Types.NewEncryptedAmount{..}] -> do
                                assertEqual "Account encrypted amounts removed" earAccount accountAddress0
                                assertEqual "Used up indices" earUpToIndex 0
                                assertEqual "New amount" earNewAmount (eatdRemainingAmount transferData)
                                assertEqual "Receiver address" neaAccount accountAddress1
                                assertEqual "New receiver index" neaNewIndex $ fromIntegral idx - 1
                                assertEqual "Received amount" neaEncryptedAmount (eatdTransferAmount transferData)
                            e -> assertFailure $ "Unexpected outcome: " ++ show e
                    e -> assertFailure $ "Transaction should succeed: " ++ show e
                doBlockStateChecks
        }
