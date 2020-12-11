module Main where

import qualified SchedulerTests.InitPoliciesTest(tests)
import qualified SchedulerTests.SimpleTransfersTest(tests)
import qualified SchedulerTests.ChainMetatest(tests)
import qualified SchedulerTests.InitContextTest(tests)
import qualified SchedulerTests.ReceiveContextTest(tests)
import qualified SchedulerTests.TrySendTest(tests)
import qualified SchedulerTests.FibonacciSelfMessageTest(tests)
import qualified SchedulerTests.AccountTransactionSpecs(tests)
import qualified SchedulerTests.InitialAccountCreationSpec(tests)
import qualified SchedulerTests.BakerTransactions(tests)
import qualified SchedulerTests.RandomBakerTransactions(tests)
import qualified SchedulerTests.CredentialTest(tests)
import qualified SchedulerTests.TransactionExpirySpec(tests)
import qualified SchedulerTests.BlockEnergyLimitSpec(tests)
import qualified SchedulerTests.TransactionGroupingSpec2(tests)
import qualified SchedulerTests.SimpleTransferSpec(tests)
import qualified SchedulerTests.UpdateAccountKeys(tests)
import qualified SchedulerTests.EncryptedTransfersTest(tests)
import qualified SchedulerTests.MaxIncomingAmountsTest(tests)

import Test.Hspec

main :: IO ()
main = hspec $ do
         SchedulerTests.InitPoliciesTest.tests
         SchedulerTests.SimpleTransfersTest.tests
         SchedulerTests.ChainMetatest.tests
         SchedulerTests.InitContextTest.tests
         SchedulerTests.ReceiveContextTest.tests
         SchedulerTests.TrySendTest.tests
         SchedulerTests.FibonacciSelfMessageTest.tests
         SchedulerTests.AccountTransactionSpecs.tests
         SchedulerTests.InitialAccountCreationSpec.tests
         SchedulerTests.BakerTransactions.tests
         SchedulerTests.RandomBakerTransactions.tests
         SchedulerTests.CredentialTest.tests
         SchedulerTests.TransactionExpirySpec.tests
         SchedulerTests.BlockEnergyLimitSpec.tests
         SchedulerTests.TransactionGroupingSpec2.tests
         SchedulerTests.SimpleTransferSpec.tests
         SchedulerTests.UpdateAccountKeys.tests
         SchedulerTests.EncryptedTransfersTest.tests
         SchedulerTests.MaxIncomingAmountsTest.tests
