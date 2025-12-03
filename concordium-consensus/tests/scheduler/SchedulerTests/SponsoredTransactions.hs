{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module implements tests covering sponsored transactions.
module SchedulerTests.SponsoredTransactions (tests) where

import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Types
import Concordium.ID.Types
import Concordium.Scheduler.Types
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types.HashableTo

import Concordium.Common.Time
import Concordium.GlobalState.Classes
import Concordium.ID.DummyData (dummyCredential)
import qualified SchedulerTests.Helpers as Helpers

type SignKeys = [(CredentialIndex, [(KeyIndex, SigScheme.KeyPair)])]

signAccountTransactionV1 :: (PayloadSize -> TransactionHeaderV1) -> EncodedPayload -> SignKeys -> Maybe SignKeys -> AccountTransactionV1
signAccountTransactionV1 mkHeader payload sender sponsor =
    AccountTransactionV1
        { atrv1Signature =
            TransactionSignaturesV1
                { tsv1Sender = txSign sender,
                  tsv1Sponsor = txSign <$> sponsor
                },
          atrv1Header = header,
          atrv1Payload = payload,
          atrv1SignHash = signHash
        }
  where
    header = mkHeader (payloadSize payload)
    signHash = transactionV1SignHashFromHeaderPayload header payload
    signHashBytes = transactionSignHashToByteString signHash
    credSign = Map.fromList . map (\(idx, key) -> (idx, SigScheme.sign key signHashBytes))
    txSign = TransactionSignature . Map.fromList . map (\(idx, credKeys) -> (idx, credSign credKeys))

makeHeaderV1 ::
    AccountAddress ->
    Maybe AccountAddress ->
    Nonce ->
    Energy ->
    PayloadSize ->
    TransactionHeaderV1
makeHeaderV1 sender sponsor nonce energy pSize =
    TransactionHeaderV1
        { thv1HeaderV0 =
            TransactionHeader
                { thSender = sender,
                  thNonce = nonce,
                  thEnergyAmount = energy,
                  thExpiry = TransactionTime maxBound,
                  thPayloadSize = pSize
                },
          thv1Sponsor = sponsor
        }

initialAmounts :: [Amount]
initialAmounts = [1_000_000, 1_000_000, 500, 0]

constructInitialBlockState :: forall pv. (IsProtocolVersion pv) => Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
constructInitialBlockState =
    Helpers.createTestBlockStateWithAccountsM $
        fmap
            (uncurry Helpers.makeTestAccountFromSeed)
            (zip initialAmounts [0 ..])

-- | Create an 'Expectation' that asserts the account balance in the given block
--  state is as expected.
assertAccountBalance :: (BlockStateOperations m) => UpdatableBlockState m -> AccountIndex -> Amount -> m Expectation
assertAccountBalance bs accIndex expectAmount = do
    bsoGetAccountByIndex bs accIndex >>= \case
        Nothing -> return $ assertFailure $ "Missing account at index " ++ show accIndex
        Just acc -> do
            balance <- getAccountAmount acc
            return $
                assertEqual
                    ("Balance of account " ++ show accIndex)
                    expectAmount
                    balance

-- | Create an 'Expectation' that asserts that the accounts have the corresponding balances
--  (in order of account index, starting from 0).
assertAccountBalances :: (BlockStateOperations m) => UpdatableBlockState m -> [Amount] -> m Expectation
assertAccountBalances bs expectAmounts =
    sequence_
        <$> mapM (uncurry (assertAccountBalance bs)) (zip [0 ..] expectAmounts)

-- | Test that a sponsored transaction is discarded in P9.
testSponsoredTransferP9 :: Expectation
testSponsoredTransferP9 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P9
        (result, _finalState) <- Helpers.runScheduler Helpers.defaultTestConfig initialState transactions
        return $ do
            assertEqual
                "Failed transactions"
                [((testTransaction, Nothing), NotSupportedAtCurrentProtocolVersion)]
                (ftFailed (Helpers.srTransactions result))
            assertEqual "Added transactions" [] (ftAdded (Helpers.srTransactions result))
    expectation
  where
    transactions = [TGAccountTransactions [(testTransaction, Nothing)]]
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 3) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 50))
                [(0, [(0, keypair 3)])]
                (Just [(0, [(0, keypair 0)])])

-- | Test that an extended (but not sponsored) transaction is discarded in P9.
testExtendedTransactionP9 :: Expectation
testExtendedTransactionP9 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P9
        (result, _finalState) <- Helpers.runScheduler Helpers.defaultTestConfig initialState transactions
        return $ do
            assertEqual
                "Failed transactions"
                [((testTransaction, Nothing), NotSupportedAtCurrentProtocolVersion)]
                (ftFailed (Helpers.srTransactions result))
            assertEqual "Added transactions" [] (ftAdded (Helpers.srTransactions result))
    expectation
  where
    transactions = [TGAccountTransactions [(testTransaction, Nothing)]]
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 3) Nothing 1 1000)
                (encodePayload (Transfer (acc 1) 50))
                [(0, [(0, keypair 3)])]
                Nothing

-- | Test that a sponsored transaction where the sender has insufficient balance to cover the
--   transfer amount is rejected (but added to the block) in P10.
testSponsoredTransferRejectP10 :: Expectation
testSponsoredTransferRejectP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig initialState transactions
        expAccBalances <-
            assertAccountBalances
                finalState
                (initialAmounts & ix 0 -~ 63500)
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual "Failed transactions" [] (ftFailed resultTransactions)
            assertEqual
                "Added transactions"
                [((toBlockItem testTransaction, Nothing), summary)]
                (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    transactions = [TGAccountTransactions [(testTransaction, Nothing)]]
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 3) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 50))
                [(0, [(0, keypair 3)])]
                (Just [(0, [(0, keypair 0)])])
    summary =
        TransactionSummary
            { tsSender = Just (acc 3),
              tsHash = getHash testTransaction,
              tsCost = 0,
              tsEnergyCost = 635,
              tsType = TSTAccountTransaction (Just TTTransfer),
              tsResult = TxReject{vrRejectReason = AmountTooLarge (AddressAccount $ acc 3) 50},
              tsIndex = 0,
              tsSponsorDetails =
                CTrue
                    ( Just
                        ( SponsorDetails
                            { sdSponsor = acc 0,
                              sdCost = 63500
                            }
                        )
                    )
            }

-- | Test a sponsored transaction success case.
testSponsoredTransferSuccessP10 :: Expectation
testSponsoredTransferSuccessP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig initialState transactions
        expAccBalances <-
            assertAccountBalances
                finalState
                (initialAmounts & ix 0 -~ 63500 & ix 2 -~ 500 & ix 1 +~ 500)
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual "Failed transactions" [] (ftFailed resultTransactions)
            assertEqual
                "Added transactions"
                [((toBlockItem testTransaction, Nothing), summary)]
                (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    transactions = [TGAccountTransactions [(testTransaction, Nothing)]]
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 2) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 500))
                [(0, [(0, keypair 2)])]
                (Just [(0, [(0, keypair 0)])])
    summary =
        TransactionSummary
            { tsSender = Just (acc 2),
              tsHash = getHash testTransaction,
              tsCost = 0,
              tsEnergyCost = 635,
              tsType = TSTAccountTransaction (Just TTTransfer),
              tsResult =
                TxSuccess
                    { vrEvents =
                        [ Transferred
                            { etFrom = AddressAccount (acc 2),
                              etTo = AddressAccount (acc 1),
                              etAmount = 500
                            }
                        ]
                    },
              tsIndex = 0,
              tsSponsorDetails =
                CTrue
                    ( Just
                        ( SponsorDetails
                            { sdSponsor = acc 0,
                              sdCost = 63500
                            }
                        )
                    )
            }

-- | Test an extended transaction success case in P10.
testExtendedTransactionSuccessP10 :: Expectation
testExtendedTransactionSuccessP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig initialState transactions
        expAccBalances <-
            assertAccountBalances
                finalState
                (initialAmounts & ix 0 -~ 50300 & ix 0 -~ 500 & ix 1 +~ 500)
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual "Failed transactions" [] (ftFailed resultTransactions)
            assertEqual
                "Added transactions"
                [((toBlockItem testTransaction, Nothing), summary)]
                (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    transactions = [TGAccountTransactions [(testTransaction, Nothing)]]
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 0) Nothing 1 1000)
                (encodePayload (Transfer (acc 1) 500))
                [(0, [(0, keypair 0)])]
                (Nothing)
    summary =
        TransactionSummary
            { tsSender = Just (acc 0),
              tsHash = getHash testTransaction,
              tsCost = 50300,
              tsEnergyCost = 503,
              tsType = TSTAccountTransaction (Just TTTransfer),
              tsResult =
                TxSuccess
                    { vrEvents =
                        [ Transferred
                            { etFrom = AddressAccount (acc 0),
                              etTo = AddressAccount (acc 1),
                              etAmount = 500
                            }
                        ]
                    },
              tsIndex = 0,
              tsSponsorDetails = CTrue Nothing
            }

-- | Test a sponsored transaction success case where the sender is the sponsor.
testSelfSponsoredTransferSuccessP10 :: Expectation
testSelfSponsoredTransferSuccessP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig initialState transactions
        expAccBalances <-
            assertAccountBalances
                finalState
                (initialAmounts & ix 0 -~ 63500 & ix 0 -~ 500 & ix 1 +~ 500)
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual "Failed transactions" [] (ftFailed resultTransactions)
            assertEqual
                "Added transactions"
                [((toBlockItem testTransaction, Nothing), summary)]
                (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    transactions = [TGAccountTransactions [(testTransaction, Nothing)]]
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 0) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 500))
                [(0, [(0, keypair 0)])]
                (Just [(0, [(0, keypair 0)])])
    summary =
        TransactionSummary
            { tsSender = Just (acc 0),
              tsHash = getHash testTransaction,
              tsCost = 0,
              tsEnergyCost = 635,
              tsType = TSTAccountTransaction (Just TTTransfer),
              tsResult =
                TxSuccess
                    { vrEvents =
                        [ Transferred
                            { etFrom = AddressAccount (acc 0),
                              etTo = AddressAccount (acc 1),
                              etAmount = 500
                            }
                        ]
                    },
              tsIndex = 0,
              tsSponsorDetails =
                CTrue
                    ( Just
                        ( SponsorDetails
                            { sdSponsor = acc 0,
                              sdCost = 63500
                            }
                        )
                    )
            }

newtype TestAccountNonceQueryT bs m a = TestAccountNonceQueryT {runTestAccountNonceQueryT :: bs -> m a}
    deriving (Functor, Applicative, Monad) via (ReaderT bs m)
    deriving (MonadTrans) via (ReaderT bs)

-- Instance for deducing the protocol version from the parameterized @m@ of the 'AccountNonceQueryT'.
deriving via (MGSTrans (TestAccountNonceQueryT bs) m) instance (MonadProtocolVersion m) => MonadProtocolVersion (TestAccountNonceQueryT bs m)

-- Instances required in order to use the 'AccountNonceQueryT' monad from within a block state context.
deriving via (MGSTrans (TestAccountNonceQueryT bs) m) instance BlockStateTypes (TestAccountNonceQueryT bs m)
deriving via (MGSTrans (TestAccountNonceQueryT bs) m) instance (TokenStateOperations ts m) => TokenStateOperations ts (TestAccountNonceQueryT bs m)
deriving via (MGSTrans (TestAccountNonceQueryT bs) m) instance (PLTQuery bs' ts m) => PLTQuery bs' ts (TestAccountNonceQueryT bs m)
deriving via (MGSTrans (TestAccountNonceQueryT bs) m) instance (BlockStateQuery m) => BlockStateQuery (TestAccountNonceQueryT bs m)
deriving via (MGSTrans (TestAccountNonceQueryT bs) m) instance (ContractStateOperations m) => ContractStateOperations (TestAccountNonceQueryT bs m)
deriving via (MGSTrans (TestAccountNonceQueryT bs) m) instance (AccountOperations m) => AccountOperations (TestAccountNonceQueryT bs m)
deriving via (MGSTrans (TestAccountNonceQueryT bs) m) instance (ModuleQuery m) => ModuleQuery (TestAccountNonceQueryT bs m)

instance (bs ~ BlockState m, BlockStateQuery m) => AccountNonceQuery (TestAccountNonceQueryT bs m) where
    getNextAccountNonce ::
        (bs ~ BlockState m, BlockStateQuery m) =>
        AccountAddressEq -> TestAccountNonceQueryT bs m (Nonce, Bool)
    getNextAccountNonce (AccountAddressEq addr) = TestAccountNonceQueryT $ \bs -> do
        getAccount bs addr >>= \case
            Nothing -> return (minNonce, True)
            Just (_, acct) -> do
                nonce <- getAccountNonce acct
                return (nonce, True)

verifyTx :: (BlockStateQuery m, BlockState m ~ BS.HashedPersistentBlockState (MPV m)) => BlockState m -> Transaction -> m TVer.VerificationResult
verifyTx bs tx = do
    runTestAccountNonceQueryT (runTransactionVerifierT (TVer.verifyExtendedTransaction tx) ctx) bs
  where
    ctx =
        Context
            { _ctxTransactionOrigin = TVer.Individual,
              _ctxMaxBlockEnergy = 10_000,
              _ctxBs = bs
            }

-- | Test a sponsored transaction success case where the transaction is first verified.
testVerifySponsoredTransferSuccessP10 :: Expectation
testVerifySponsoredTransferSuccessP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        verRes <- verifyTx initialState testTransaction
        case verRes of
            TVer.Ok (TVer.ExtendedTransactionSuccess{}) -> return ()
            _ -> liftIO . assertFailure $ "Expected ExtendedTransactionSuccess{..} but saw " ++ show verRes
        let transactions = [TGAccountTransactions [(testTransaction, Just verRes)]]
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig initialState transactions
        expAccBalances <-
            assertAccountBalances
                finalState
                (initialAmounts & ix 0 -~ 63500 & ix 2 -~ 500 & ix 1 +~ 500)
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual "Failed transactions" [] (ftFailed resultTransactions)
            assertEqual
                "Added transactions"
                [((toBlockItem testTransaction, Just verRes), summary)]
                (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 2) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 500))
                [(0, [(0, keypair 2)])]
                (Just [(0, [(0, keypair 0)])])
    summary =
        TransactionSummary
            { tsSender = Just (acc 2),
              tsHash = getHash testTransaction,
              tsCost = 0,
              tsEnergyCost = 635,
              tsType = TSTAccountTransaction (Just TTTransfer),
              tsResult =
                TxSuccess
                    { vrEvents =
                        [ Transferred
                            { etFrom = AddressAccount (acc 2),
                              etTo = AddressAccount (acc 1),
                              etAmount = 500
                            }
                        ]
                    },
              tsIndex = 0,
              tsSponsorDetails =
                CTrue
                    ( Just
                        ( SponsorDetails
                            { sdSponsor = acc 0,
                              sdCost = 63500
                            }
                        )
                    )
            }

-- | Test a sponsored transaction success case where the transaction is first verified, then the
--  sponsor's keys are changed (while keeping the signature valid).
testVerifyChangeSponsorKeySponsoredTransferSuccessP10 :: Expectation
testVerifyChangeSponsorKeySponsoredTransferSuccessP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        verRes <- verifyTx initialState testTransaction
        case verRes of
            TVer.Ok (TVer.ExtendedTransactionSuccess{}) -> return ()
            _ -> liftIO . assertFailure $ "Expected ExtendedTransactionSuccess{..} but saw " ++ show verRes
        let transactions = [TGAccountTransactions [(testTransaction, Just verRes)]]
        keyChangeState <- do
            mbs0 <- thawBlockState initialState
            let newKeys =
                    CredentialPublicKeys
                        { credKeys =
                            Map.fromList
                                [ (0, SigScheme.correspondingVerifyKey (keypair 0)),
                                  (1, SigScheme.correspondingVerifyKey (keypair 100))
                                ],
                          credThreshold = 1
                        }
            -- Add a new key to credential 0 of the sponsor.
            mbs1 <- bsoSetAccountCredentialKeys mbs0 0 0 newKeys
            gc <- bsoGetCryptoParams mbs1
            let cred = dummyCredential gc (acc 0) (SigScheme.correspondingVerifyKey (keypair 200)) (YearMonth 3000 1) (YearMonth 2000 1)
            let addCreds = Map.singleton 1 cred
            -- Add a new credential to the sponsor account.
            mbs2 <- bsoUpdateAccountCredentials mbs1 0 [] addCreds 1
            freezeBlockState mbs2
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig keyChangeState transactions
        expAccBalances <-
            assertAccountBalances
                finalState
                (initialAmounts & ix 0 -~ 63500 & ix 2 -~ 500 & ix 1 +~ 500)
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual "Failed transactions" [] (ftFailed resultTransactions)
            assertEqual
                "Added transactions"
                [((toBlockItem testTransaction, Just verRes), summary)]
                (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 2) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 500))
                [(0, [(0, keypair 2)])]
                (Just [(0, [(0, keypair 0)])])
    summary =
        TransactionSummary
            { tsSender = Just (acc 2),
              tsHash = getHash testTransaction,
              tsCost = 0,
              tsEnergyCost = 635,
              tsType = TSTAccountTransaction (Just TTTransfer),
              tsResult =
                TxSuccess
                    { vrEvents =
                        [ Transferred
                            { etFrom = AddressAccount (acc 2),
                              etTo = AddressAccount (acc 1),
                              etAmount = 500
                            }
                        ]
                    },
              tsIndex = 0,
              tsSponsorDetails =
                CTrue
                    ( Just
                        ( SponsorDetails
                            { sdSponsor = acc 0,
                              sdCost = 63500
                            }
                        )
                    )
            }

-- | Test a sponsored transaction case where the transaction is first verified, then the
--  sponsor's keys are changed increasing the threshold and rendering the transaction invalid.
testVerifyChangeSponsorKeySponsoredTransferFailureIncreaseThresholdP10 :: Expectation
testVerifyChangeSponsorKeySponsoredTransferFailureIncreaseThresholdP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        verRes <- verifyTx initialState testTransaction
        case verRes of
            TVer.Ok (TVer.ExtendedTransactionSuccess{}) -> return ()
            _ -> liftIO . assertFailure $ "Expected ExtendedTransactionSuccess{..} but saw " ++ show verRes
        let transactions = [TGAccountTransactions [(testTransaction, Just verRes)]]
        keyChangeState <- do
            mbs0 <- thawBlockState initialState
            let newKeys =
                    CredentialPublicKeys
                        { credKeys =
                            Map.fromList
                                [ (0, SigScheme.correspondingVerifyKey (keypair 0)),
                                  (1, SigScheme.correspondingVerifyKey (keypair 100))
                                ],
                          credThreshold = 2
                        }
            -- Add a new key to credential 0 of the sponsor, updating the threshold to 2.
            mbs1 <- bsoSetAccountCredentialKeys mbs0 0 0 newKeys
            freezeBlockState mbs1
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig keyChangeState transactions
        expAccBalances <- assertAccountBalances finalState initialAmounts
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual
                "Failed transactions"
                [((testTransaction, Just verRes), IncorrectSignature)]
                (ftFailed resultTransactions)
            assertEqual "Added transactions" [] (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 2) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 500))
                [(0, [(0, keypair 2)])]
                (Just [(0, [(0, keypair 0)])])

-- | Test a sponsored transaction case where the transaction is first verified, then the
--  sponsor's keys are changed increasing the credential threshold and rendering the transaction invalid.
testVerifyChangeSponsorKeySponsoredTransferFailureIncreaseCredentialThresholdP10 :: Expectation
testVerifyChangeSponsorKeySponsoredTransferFailureIncreaseCredentialThresholdP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        verRes <- verifyTx initialState testTransaction
        case verRes of
            TVer.Ok (TVer.ExtendedTransactionSuccess{}) -> return ()
            _ -> liftIO . assertFailure $ "Expected ExtendedTransactionSuccess{..} but saw " ++ show verRes
        let transactions = [TGAccountTransactions [(testTransaction, Just verRes)]]
        keyChangeState <- do
            mbs0 <- thawBlockState initialState
            gc <- bsoGetCryptoParams mbs0
            let cred = dummyCredential gc (acc 0) (SigScheme.correspondingVerifyKey (keypair 200)) (YearMonth 3000 1) (YearMonth 2000 1)
            let addCreds = Map.singleton 1 cred
            -- Add a new credential to the sponsor account, updating the threshold to 2.
            mbs1 <- bsoUpdateAccountCredentials mbs0 0 [] addCreds 2
            freezeBlockState mbs1
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig keyChangeState transactions
        expAccBalances <- assertAccountBalances finalState initialAmounts
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual
                "Failed transactions"
                [((testTransaction, Just verRes), IncorrectSignature)]
                (ftFailed resultTransactions)
            assertEqual "Added transactions" [] (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 2) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 500))
                [(0, [(0, keypair 2)])]
                (Just [(0, [(0, keypair 0)])])

-- | Test a sponsored transaction success case where the transaction is first verified, then the
--  sender's keys are changed (while keeping the signature valid).
testVerifyChangeSenderKeySponsoredTransferSuccessP10 :: Expectation
testVerifyChangeSenderKeySponsoredTransferSuccessP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        verRes <- verifyTx initialState testTransaction
        case verRes of
            TVer.Ok (TVer.ExtendedTransactionSuccess{}) -> return ()
            _ -> liftIO . assertFailure $ "Expected ExtendedTransactionSuccess{..} but saw " ++ show verRes
        let transactions = [TGAccountTransactions [(testTransaction, Just verRes)]]
        keyChangeState <- do
            mbs0 <- thawBlockState initialState
            let newKeys =
                    CredentialPublicKeys
                        { credKeys =
                            Map.fromList
                                [ (0, SigScheme.correspondingVerifyKey (keypair 2)),
                                  (1, SigScheme.correspondingVerifyKey (keypair 100))
                                ],
                          credThreshold = 1
                        }
            -- Add a new key to credential 0 of the sender.
            mbs1 <- bsoSetAccountCredentialKeys mbs0 2 0 newKeys
            gc <- bsoGetCryptoParams mbs1
            let cred = dummyCredential gc (acc 0) (SigScheme.correspondingVerifyKey (keypair 200)) (YearMonth 3000 1) (YearMonth 2000 1)
            let addCreds = Map.singleton 1 cred
            -- Add a new credential to the sender account.
            mbs2 <- bsoUpdateAccountCredentials mbs1 2 [] addCreds 1
            freezeBlockState mbs2
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig keyChangeState transactions
        expAccBalances <-
            assertAccountBalances
                finalState
                (initialAmounts & ix 0 -~ 63500 & ix 2 -~ 500 & ix 1 +~ 500)
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual "Failed transactions" [] (ftFailed resultTransactions)
            assertEqual
                "Added transactions"
                [((toBlockItem testTransaction, Just verRes), summary)]
                (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 2) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 500))
                [(0, [(0, keypair 2)])]
                (Just [(0, [(0, keypair 0)])])
    summary =
        TransactionSummary
            { tsSender = Just (acc 2),
              tsHash = getHash testTransaction,
              tsCost = 0,
              tsEnergyCost = 635,
              tsType = TSTAccountTransaction (Just TTTransfer),
              tsResult =
                TxSuccess
                    { vrEvents =
                        [ Transferred
                            { etFrom = AddressAccount (acc 2),
                              etTo = AddressAccount (acc 1),
                              etAmount = 500
                            }
                        ]
                    },
              tsIndex = 0,
              tsSponsorDetails =
                CTrue
                    ( Just
                        ( SponsorDetails
                            { sdSponsor = acc 0,
                              sdCost = 63500
                            }
                        )
                    )
            }

-- | Test a sponsored transaction case where the transaction is first verified, then the
--  sender's keys are changed increasing the threshold and rendering the transaction invalid.
testVerifyChangeSenderKeySponsoredTransferFailureIncreaseThresholdP10 :: Expectation
testVerifyChangeSenderKeySponsoredTransferFailureIncreaseThresholdP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        verRes <- verifyTx initialState testTransaction
        case verRes of
            TVer.Ok (TVer.ExtendedTransactionSuccess{}) -> return ()
            _ -> liftIO . assertFailure $ "Expected ExtendedTransactionSuccess{..} but saw " ++ show verRes
        let transactions = [TGAccountTransactions [(testTransaction, Just verRes)]]
        keyChangeState <- do
            mbs0 <- thawBlockState initialState
            let newKeys =
                    CredentialPublicKeys
                        { credKeys =
                            Map.fromList
                                [ (0, SigScheme.correspondingVerifyKey (keypair 2)),
                                  (1, SigScheme.correspondingVerifyKey (keypair 100))
                                ],
                          credThreshold = 2
                        }
            -- Add a new key to credential 0 of the sender increasing the threshold to 2.
            mbs1 <- bsoSetAccountCredentialKeys mbs0 2 0 newKeys
            freezeBlockState mbs1
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig keyChangeState transactions
        expAccBalances <- assertAccountBalances finalState initialAmounts
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual
                "Failed transactions"
                [((testTransaction, Just verRes), IncorrectSignature)]
                (ftFailed resultTransactions)
            assertEqual "Added transactions" [] (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 2) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 500))
                [(0, [(0, keypair 2)])]
                (Just [(0, [(0, keypair 0)])])

-- | Test a sponsored transaction case where the transaction is first verified, then the
--  sender's keys are changed increasing the credential threshold and rendering the transaction invalid.
testVerifyChangeSenderKeySponsoredTransferFailureIncreaseCredentialThresholdP10 :: Expectation
testVerifyChangeSenderKeySponsoredTransferFailureIncreaseCredentialThresholdP10 = do
    expectation <- Helpers.runTestBlockState $ do
        initialState <- constructInitialBlockState @'P10
        verRes <- verifyTx initialState testTransaction
        case verRes of
            TVer.Ok (TVer.ExtendedTransactionSuccess{}) -> return ()
            _ -> liftIO . assertFailure $ "Expected ExtendedTransactionSuccess{..} but saw " ++ show verRes
        let transactions = [TGAccountTransactions [(testTransaction, Just verRes)]]
        keyChangeState <- do
            mbs0 <- thawBlockState initialState
            gc <- bsoGetCryptoParams mbs0
            let dummyPubKey = SigScheme.correspondingVerifyKey (keypair 200)
            let cred = dummyCredential gc (acc 2) dummyPubKey (YearMonth 3000 1) (YearMonth 2000 1)
            let addCreds = Map.singleton 1 cred
            -- Add a new credential to the sender increasing the threshold to 2.
            mbs1 <- bsoUpdateAccountCredentials mbs0 2 [] addCreds 2
            freezeBlockState mbs1
        (result, finalState) <- Helpers.runScheduler Helpers.defaultTestConfig keyChangeState transactions
        expAccBalances <- assertAccountBalances finalState initialAmounts
        return $ do
            let resultTransactions = Helpers.srTransactions result
            assertEqual
                "Failed transactions"
                [((testTransaction, Just verRes), IncorrectSignature)]
                (ftFailed resultTransactions)
            assertEqual "Added transactions" [] (ftAdded resultTransactions)
            expAccBalances
    expectation
  where
    acc = Helpers.accountAddressFromSeed
    keypair = Helpers.keyPairFromSeed
    testTransaction =
        fromAccountTransactionV1 0 $
            signAccountTransactionV1
                (makeHeaderV1 (acc 2) (Just (acc 0)) 1 1000)
                (encodePayload (Transfer (acc 1) 500))
                [(0, [(0, keypair 2)])]
                (Just [(0, [(0, keypair 0)])])

tests :: Spec
tests = parallel $ do
    it "Sponsored transfer @P9" testSponsoredTransferP9
    it "Extended transaction (transfer, no sponsor) @P9" testExtendedTransactionP9

    it "Sponsored transfer (reject) @P10" testSponsoredTransferRejectP10
    it "Sponsored transfer (success) @P10" testSponsoredTransferSuccessP10
    it "Extended transaction (transfer, no sponsor, success) @P10" testExtendedTransactionSuccessP10
    it "Self-sponsored transfer (success) @P10" testSelfSponsoredTransferSuccessP10
    it "Pre-verified sponosored transfer (success) @P10" testVerifySponsoredTransferSuccessP10
    it "Verify sponsored then change sponsor keys (success) @P10" testVerifyChangeSponsorKeySponsoredTransferSuccessP10
    it "Verify sponsored then change sponsor keys - increase threshold (failure) @P10" testVerifyChangeSponsorKeySponsoredTransferFailureIncreaseThresholdP10
    it "Verify sponsored then change sponsor keys - increase credential threshold (failure) @P10" testVerifyChangeSponsorKeySponsoredTransferFailureIncreaseCredentialThresholdP10
    it "Verify sponsored then change sender keys (success) @P10" testVerifyChangeSenderKeySponsoredTransferSuccessP10
    it "Verify sponsored then change sender keys - increase threshold (failure) @P10" testVerifyChangeSenderKeySponsoredTransferFailureIncreaseThresholdP10
    it "Verify sponsored then change sender keys - increase credential threshold (failure) @P10" testVerifyChangeSenderKeySponsoredTransferFailureIncreaseCredentialThresholdP10
