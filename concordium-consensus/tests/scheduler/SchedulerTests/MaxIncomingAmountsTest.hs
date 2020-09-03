module SchedulerTests.MaxIncomingAmountsTest where

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

import qualified Test.HUnit as HUnit
import Test.Hspec

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.Runner as Runner

import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import Concordium.Crypto.EncryptedTransfers
import Data.Foldable

import SchedulerTests.TestUtils

import Lens.Micro.Platform

import Concordium.Types
import Concordium.Crypto.FFIDataTypes (ElgamalSecond, ElgamalSecondSecret)
import Data.Word (Word64)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Concordium.ID.DummyData (dummyEncryptionSecretKey)
import Data.Maybe (fromJust)
import Concordium.ID.Types (AccountEncryptionKey(..),CredentialRegistrationID(RegIdCred))
import qualified Data.Sequence as Seq

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    10000000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 10000000000) Acc.emptyAccounts)

-- Ales' keys
alesEncryptionSecretKey :: ElgamalSecondSecret
alesEncryptionSecretKey = dummyEncryptionSecretKey alesAccount
alesEncryptionPublicKeyUnwrapped :: ElgamalSecond
alesEncryptionPublicKeyUnwrapped = let AccountEncryptionKey (RegIdCred s) = alesEncryptionPublicKey in s
alesEncryptionPublicKey :: AccountEncryptionKey
alesEncryptionPublicKey = (fromJust $ Acc.getAccount alesAccount (initialBlockState ^. blockAccounts)) ^. accountPersisting . accountEncryptionKey

-- Thomas' keys
thomasEncryptionSecretKey :: ElgamalSecondSecret
thomasEncryptionSecretKey = dummyEncryptionSecretKey thomasAccount
thomasEncryptionPublicKeyUnwrapped :: ElgamalSecond
thomasEncryptionPublicKeyUnwrapped = let AccountEncryptionKey (RegIdCred s) = thomasEncryptionPublicKey in s
thomasEncryptionPublicKey :: AccountEncryptionKey
thomasEncryptionPublicKey = (fromJust $ Acc.getAccount thomasAccount (initialBlockState ^. blockAccounts)) ^. accountPersisting . accountEncryptionKey

-- Helpers for creating the transfer datas
createEncryptedTransferData :: ElgamalSecond -> ElgamalSecondSecret -> AggregatedDecryptedAmount -> Word64 -> Maybe EncryptedAmountTransferData
createEncryptedTransferData second secret aggDecAmount amount = unsafeDupablePerformIO $ makeEncryptedAmountTransferData (initialBlockState ^. blockCryptographicParameters . unhashed) second secret aggDecAmount amount
createSecToPubTransferData :: ElgamalSecondSecret -> AggregatedDecryptedAmount -> Word64 -> Maybe SecToPubAmountTransferData
createSecToPubTransferData secret aggAmount amount = unsafeDupablePerformIO $ makeSecToPubAmountTransferData (initialBlockState ^. blockCryptographicParameters . unhashed) secret aggAmount amount

-- Helper for checking the encrypted balance of an account.
checkEncryptedBalance :: AccountEncryptedAmount -> AccountAddress -> BlockState -> SpecWith ()
checkEncryptedBalance accEncAmount acc = (\bs -> specify ("Correct final balance on " ++ show acc) $
           case Acc.getAccount acc (bs ^. blockAccounts) of
             Nothing -> HUnit.assertFailure $ "Account with id '" ++ show acc ++ "' not found"
             Just account -> HUnit.assertEqual "Expected encrypted amount matches"  accEncAmount (account ^. accountEncryptedAmount))

--------------------------------- Transactions ---------------------------------

numberOfTransactions :: Num a => a
numberOfTransactions = fromIntegral maxNumIncoming + 2

-- Transaction 1. Pub to sec (1000)
encryptedAmount1000 :: EncryptedAmount
encryptedAmount1000 = encryptAmountZeroRandomness (initialBlockState ^. blockCryptographicParameters . unhashed) 1000

-- Initial transfer transaction
encryptedTransferData1 :: EncryptedAmountTransferData
encryptedTransferData1 = fromJust $ createEncryptedTransferData thomasEncryptionPublicKeyUnwrapped alesEncryptionSecretKey (makeAggregatedDecryptedAmount encryptedAmount1000 1000 0) 10

-- Function used to generate all the transfer datas based on encryptedTransferData1
-- each transaction sends 10 tokens from Ales' self amount to Thomas
makeNext :: (EncryptedAmountTransferData, Amount) -> (EncryptedAmountTransferData, Amount)
makeNext (d,a) = (fromJust $ createEncryptedTransferData thomasEncryptionPublicKeyUnwrapped alesEncryptionSecretKey (makeAggregatedDecryptedAmount (eatdRemainingAmount d) (_amount a) 0) 10, a - 10)

-- Infinite list of transfer datas that send 10 from Ales' self amount to Thomas
allTxs :: [(EncryptedAmountTransferData, Amount)]
allTxs = iterate makeNext (encryptedTransferData1, 990)

-- Infinite list of transaction specs that before `maxNumIncoming` transactions considers that transferred amounts are added to the incoming amounts and after that it checks that the initial incoming amounts are being automatically aggregated.
transactions :: [(Runner.TransactionJSON, (TResultSpec, BlockState -> Spec))]
transactions =
  [ makeNormalTransaction x idx | (idx, (x, _)) <- zip [1..] normal ] ++ [ makeInterestingTransaction tx (fromIntegral idx) | (idx, (tx, _)) <- zip [maxNumIncoming + 1..] interesting ]
 where
       (normal, interesting) = splitAt maxNumIncoming allTxs

       -- A normal transaction is a transfer before maxNumIncoming amounts have been received.
       makeNormalTransaction :: EncryptedAmountTransferData -> EncryptedAmountAggIndex -> (Runner.TransactionJSON, (TResultSpec, BlockState -> Spec))
       makeNormalTransaction x idx = makeTransaction x idx $
         \bs -> do
           checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount x} alesAccount bs -- Ales amount should be the remaining amount on the transaction
           checkEncryptedBalance initialAccountEncryptedAmount{_startIndex = 0,                                    -- no amounts have been used on Thomas account
                                                               _incomingEncryptedAmounts = Seq.fromList [ eatdTransferAmount n | (n, _) <- take (fromIntegral idx) normal ] -- It should have all the received amounts until this index
                                                              } thomasAccount bs

       -- An interesting transaction is a transfer after maxNumIncoming amounts have been received.
       makeInterestingTransaction :: EncryptedAmountTransferData -> EncryptedAmountAggIndex -> (Runner.TransactionJSON, (TResultSpec, BlockState -> Spec))
       makeInterestingTransaction x idx = makeTransaction x idx $
         \bs -> do
           checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount x} alesAccount bs -- Ales amount should be the remaining amount on the transaction
           let (combined, kept) = Seq.splitAt ((fromIntegral idx - fromIntegral maxNumIncoming) + 1) (Seq.fromList [ eatdTransferAmount n | (n, _) <- take (fromIntegral idx) allTxs ]) -- get which amounts should be combined into combinedAmount
               combinedAmount = foldl' aggregateAmounts mempty combined -- combine them
           checkEncryptedBalance initialAccountEncryptedAmount{_startIndex = idx - fromIntegral maxNumIncoming, -- the start index should have increased
                                                               _incomingEncryptedAmounts =  combinedAmount Seq.:<| kept, -- the first amount should be the combined one
                                                               _numAggregated = Just $ fromIntegral (idx - fromIntegral maxNumIncoming + 1) -- the number of aggregated amounts should be this one
                                                              } thomasAccount bs

       makeTransaction :: EncryptedAmountTransferData -> EncryptedAmountAggIndex -> (BlockState -> Spec) -> (Runner.TransactionJSON, (TResultSpec, BlockState -> Spec))
       makeTransaction x@EncryptedAmountTransferData{..} idx checks =
         ( Runner.TJSON { payload = Runner.EncryptedAmountTransfer thomasAccount x -- create an encrypted transfer to Thomas
                       , metadata = makeDummyHeader alesAccount (fromIntegral idx + 1) 100000 -- from Ales with nonce idx + 1
                       , keys = [(0, alesKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = alesAccount,
                        earUpToIndex = 0,
                        earNewAmount = eatdRemainingAmount -- the new self amount
                        },
                      Types.NewEncryptedAmount {
                        neaAccount = thomasAccount,
                        neaNewIndex = fromIntegral idx - 1,
                        neaEncryptedAmount = eatdTransferAmount -- the transferred amount
                        }
                    ], checks
          )
        )

-- Transaction that will transfer 340 to Thomas's public balance
secToPubTransferData :: SecToPubAmountTransferData
secToPubTransferData = fromJust $ createSecToPubTransferData thomasEncryptionSecretKey (makeAggregatedDecryptedAmount (foldl' aggregateAmounts mempty [ eatdTransferAmount n | (n, _) <- take numberOfTransactions allTxs ]) (numberOfTransactions * 10) numberOfTransactions) (numberOfTransactions * 10)

------------------------------------- Test -------------------------------------

testCases :: [TestCase]
testCases =
  [ TestCase
    { tcName = "Makes an encrypted transfer"
    , tcParameters = defaultParams { tpInitialBlockState = initialBlockState }
    , tcTransactions =
        -- First transfer some money to Ales' private balance
        ( Runner.TJSON { payload = Runner.TransferToEncrypted 1000
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0, alesKP)]
                       }
        , (SuccessE [Types.EncryptedSelfAmountAdded {
                        eaaAccount = alesAccount,
                        eaaNewAmount = encryptedAmount1000,
                        eaaAmount = 1000
                        }
                    ], checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = encryptedAmount1000} alesAccount
          )
        ) :
        -- Now send 34 transactions of 10 tokens from Ales to Thomas
        take numberOfTransactions transactions ++
        -- Send the encrypted 340 tokens on Thomas' account to his public balance
        [ ( Runner.TJSON { payload = Runner.TransferToPublic secToPubTransferData
                       , metadata = makeDummyHeader thomasAccount 1 100000
                       , keys = [(0, thomasKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = thomasAccount,
                        earUpToIndex = numberOfTransactions,
                        earNewAmount = stpatdRemainingAmount secToPubTransferData
                        },
                      Types.AmountAddedByDecryption {
                        aabdAccount = thomasAccount,
                        aabdAmount = numberOfTransactions * 10
                        }
                    ],
            checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = stpatdRemainingAmount secToPubTransferData,
                                                                _startIndex = numberOfTransactions,
                                                                _incomingEncryptedAmounts = Seq.empty} thomasAccount
          )
        )
        ]
    }
  ]

tests :: Spec
tests = describe "TransferToEncrypted." $ mkSpecs testCases
