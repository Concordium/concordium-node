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

import Lens.Micro.Platform
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Data.Foldable

import SchedulerTests.TestUtils

import qualified Test.HUnit as HUnit
import Test.Hspec

import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import Concordium.Crypto.EncryptedTransfers
import Concordium.Types
import Concordium.Crypto.FFIDataTypes (ElgamalSecretKey)
import Concordium.ID.DummyData (dummyEncryptionSecretKey)
import Concordium.ID.Types (AccountEncryptionKey(..))

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.DummyData

iterateLimitM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateLimitM n f = go 0 where
  go m x | m == n = return []
         | otherwise = do
             y <- f x
             (x:) <$> go (m+1) y

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    10000000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 10000000000) Acc.emptyAccounts)

-- Ales' keys
alesEncryptionSecretKey :: ElgamalSecretKey
alesEncryptionSecretKey = dummyEncryptionSecretKey dummyCryptographicParameters alesAccount
alesEncryptionPublicKey :: AccountEncryptionKey
alesEncryptionPublicKey = fromJust (Acc.getAccount alesAccount (initialBlockState ^. blockAccounts)) ^. accountPersisting . accountEncryptionKey

-- Thomas' keys
thomasEncryptionSecretKey :: ElgamalSecretKey
thomasEncryptionSecretKey = dummyEncryptionSecretKey dummyCryptographicParameters thomasAccount
thomasEncryptionPublicKey :: AccountEncryptionKey
thomasEncryptionPublicKey = fromJust (Acc.getAccount thomasAccount (initialBlockState ^. blockAccounts)) ^. accountPersisting . accountEncryptionKey

-- Helpers for creating the transfer datas
createEncryptedTransferData ::
  AccountEncryptionKey -- ^ Public key of the receiver
  -> ElgamalSecretKey  -- ^ Secret key of the sender
  -> AggregatedDecryptedAmount -- ^ Amount to use as input.
  -> Amount   -- ^ Amount to send.
  -> IO (Maybe EncryptedAmountTransferData)
createEncryptedTransferData (AccountEncryptionKey receiverPK) =
  makeEncryptedAmountTransferData (initialBlockState ^. blockCryptographicParameters . unhashed) receiverPK


createSecToPubTransferData :: ElgamalSecretKey -> AggregatedDecryptedAmount -> Amount -> IO (Maybe SecToPubAmountTransferData)
createSecToPubTransferData =
  makeSecToPubAmountTransferData (initialBlockState ^. blockCryptographicParameters . unhashed)

-- Helper for checking the encrypted balance of an account.
checkEncryptedBalance :: AccountEncryptedAmount -> AccountAddress -> BlockState -> SpecWith ()
checkEncryptedBalance accEncAmount acc bs =
  specify ("Correct final balance on " ++ show acc) $
    case Acc.getAccount acc (bs ^. blockAccounts) of
      Nothing -> HUnit.assertFailure $ "Account with id '" ++ show acc ++ "' not found"
      Just account -> HUnit.assertEqual "Expected encrypted amount matches" accEncAmount (account ^. accountEncryptedAmount)

--------------------------------- Transactions ---------------------------------

numberOfTransactions :: Num a => a
numberOfTransactions = fromIntegral maxNumIncoming + 2

-- Transaction 1. Pub to sec (1000)
encryptedAmount1000 :: EncryptedAmount
encryptedAmount1000 = encryptAmountZeroRandomness (initialBlockState ^. blockCryptographicParameters . unhashed) 1000

-- Initial transfer transaction
encryptedTransferData1 :: IO EncryptedAmountTransferData
encryptedTransferData1 = fromJust <$> createEncryptedTransferData thomasEncryptionPublicKey alesEncryptionSecretKey (makeAggregatedDecryptedAmount encryptedAmount1000 1000 0) 10

-- Function used to generate all the transfer datas based on encryptedTransferData1
-- each transaction sends 10 tokens from Ales' self amount to Thomas
makeNext :: (EncryptedAmountTransferData, Amount) -> IO (EncryptedAmountTransferData, Amount)
makeNext (d,a) = do
  newData <- fromJust <$> createEncryptedTransferData thomasEncryptionPublicKey alesEncryptionSecretKey (makeAggregatedDecryptedAmount (eatdRemainingAmount d) a 0) 10
  return (newData, a - 10)

-- A list of all the input transactions 'numberOfTransactions'.
allTxsIO :: IO [(EncryptedAmountTransferData, Amount)]
allTxsIO = do
  start <- encryptedTransferData1
  iterateLimitM numberOfTransactions makeNext (start, 990)

-- Infinite list of transaction specs that before `maxNumIncoming` transactions
-- considers that transferred amounts are added to the incoming amounts and
-- after that it checks that the initial incoming amounts are being
-- automatically aggregated.
transactionsIO :: IO ([(Runner.TransactionJSON, (TResultSpec, BlockState -> Spec))],
                     [(EncryptedAmountTransferData, Amount)])
transactionsIO = do
  allTxs <- allTxsIO
  let (normal, interesting) = splitAt maxNumIncoming allTxs
   -- A normal transaction is a transfer before maxNumIncoming amounts have been received.
  let makeNormalTransaction :: EncryptedAmountTransferData -> EncryptedAmountAggIndex -> (Runner.TransactionJSON, (TResultSpec, BlockState -> Spec))
      makeNormalTransaction x idx = makeTransaction x idx $
        \bs -> do
          checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount x} alesAccount bs -- Ales amount should be the remaining amount on the transaction
          checkEncryptedBalance initialAccountEncryptedAmount{
            _startIndex = 0, -- no amounts have been used on Thomas account
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
                                                              _incomingEncryptedAmounts =  kept, -- the list of incoming amounts will hold the `rest` of the amounts
                                                              _aggregatedAmount = Just (combinedAmount, fromIntegral (idx - fromIntegral maxNumIncoming + 1)) -- the combined amount goes into the `_aggregatedAmount` field together with the number of aggregated amounts until this point.
                                                             } thomasAccount bs
      makeTransaction :: EncryptedAmountTransferData -> EncryptedAmountAggIndex -> (BlockState -> Spec) -> (Runner.TransactionJSON, (TResultSpec, BlockState -> Spec))
      makeTransaction x@EncryptedAmountTransferData{..} idx checks =
        ( Runner.TJSON { payload = Runner.EncryptedAmountTransfer thomasAccount x -- create an encrypted transfer to Thomas
                      , metadata = makeDummyHeader alesAccount (fromIntegral idx + 1) 100000 -- from Ales with nonce idx + 1
                      , keys = [(0,[(0, alesKP)])]
                      }
       , (Success $ \case [Types.EncryptedAmountsRemoved {..}, Types.NewEncryptedAmount {..}] -> do
                            HUnit.assertEqual "Account encrypted amounts removed" earAccount alesAccount
                            HUnit.assertEqual "Used up indices" earUpToIndex 0
                            HUnit.assertEqual "New amount" earNewAmount eatdRemainingAmount
                            HUnit.assertEqual "Receiver address" neaAccount thomasAccount
                            HUnit.assertEqual "New receiver index" neaNewIndex $ fromIntegral idx - 1
                            HUnit.assertEqual "Received amount" neaEncryptedAmount eatdTransferAmount
                          e -> HUnit.assertFailure $ "Unexpected outcome: " ++ show e
         , checks
         )
       )

  return ([ makeNormalTransaction x idx | (idx, (x, _)) <- zip [1..] normal ] ++
            [ makeInterestingTransaction tx (fromIntegral idx) | (idx, (tx, _)) <- zip [maxNumIncoming + 1..] interesting ],
            allTxs)


-- Transaction that will transfer 340 to Thomas's public balancesecToPubTransferData :: IO SecToPubAmountTransferData
mkSecToPubTransferData :: [(EncryptedAmountTransferData, a)] -> IO SecToPubAmountTransferData
mkSecToPubTransferData transactions = fromJust <$> createSecToPubTransferData thomasEncryptionSecretKey aggAmount (numberOfTransactions * 10)
  where aggAmount = makeAggregatedDecryptedAmount allAmounts (numberOfTransactions * 10) numberOfTransactions
        allAmounts = foldl' aggregateAmounts mempty [ eatdTransferAmount n | (n, _) <- transactions ]

------------------------------------- Test -------------------------------------

testCases :: [(Runner.TransactionJSON, (TResultSpec, BlockState -> Spec))] -> [TestCase]
testCases transactions =
  [ TestCase
    { tcName = "Makes an encrypted transfer"
    , tcParameters = defaultParams { tpInitialBlockState = initialBlockState }
    , tcTransactions =
        -- First transfer some money to Ales' private balance
        ( Runner.TJSON { payload = Runner.TransferToEncrypted 1000
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
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
        transactions
    }
  ]

tests :: Spec
tests = do
  (transactions, allTxs) <- runIO transactionsIO
  lastTransaction <- runIO $ do
    secToPubTransferData <- mkSecToPubTransferData allTxs
    -- Send the encrypted 340 tokens on Thomas' account to his public balance
    return
        [ ( Runner.TJSON { payload = Runner.TransferToPublic secToPubTransferData
                       , metadata = makeDummyHeader thomasAccount 1 100000
                       , keys = [(0,[(0, thomasKP)])]
                       }
        , (Success $ \case  [Types.EncryptedAmountsRemoved {..}, Types.AmountAddedByDecryption {..} ] -> do
                              HUnit.assertEqual "Account encrypted amounts removed" earAccount thomasAccount
                              HUnit.assertEqual "Used up indices" earUpToIndex numberOfTransactions
                              HUnit.assertEqual "New amount" earNewAmount $ stpatdRemainingAmount secToPubTransferData
                              HUnit.assertEqual "Decryption address" aabdAccount thomasAccount
                              HUnit.assertEqual "Amount added" aabdAmount $ numberOfTransactions * 10
                            e -> HUnit.assertFailure $ "Unexpected final outcome: " ++ show e,

            checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = stpatdRemainingAmount secToPubTransferData,
                                                                _startIndex = numberOfTransactions,
                                                                _incomingEncryptedAmounts = Seq.empty} thomasAccount
          )
        )
        ]
  describe "Encrypted transfers with aggregation." $ mkSpecs (testCases (transactions ++ lastTransaction))
