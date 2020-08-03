module Main where

-- import qualified SchedulerTests.ContractCommSpec(tests)
import qualified SchedulerTests.SimpleTransfersTest(tests)
import qualified SchedulerTests.ChainMetatest(tests)
import qualified SchedulerTests.InitContextTest(tests)
import qualified SchedulerTests.ReceiveContextTest(tests)
-- import qualified SchedulerTests.FibonacciTest(tests)
import qualified SchedulerTests.AccountTransactionSpecs(tests)
import qualified SchedulerTests.BakerTransactions(tests)
import qualified SchedulerTests.Delegation(tests)
-- import qualified SchedulerTests.ContractSimpleTransfersSpec(tests)
import qualified SchedulerTests.CredentialTest(tests)
import qualified SchedulerTests.TransactionExpirySpec(tests)
import qualified SchedulerTests.BlockEnergyLimitSpec(tests)
import qualified SchedulerTests.TransactionGroupingSpec2(tests)
import qualified SchedulerTests.UpdateElectionDifficultySpec(tests)
import qualified SchedulerTests.SimpleTransferSpec(tests)
import qualified SchedulerTests.UpdateAccountKeys(tests)

import Test.Hspec

main :: IO ()
main = hspec $ do
         -- SchedulerTests.ContractCommSpec.tests
         SchedulerTests.SimpleTransfersTest.tests
         SchedulerTests.ChainMetatest.tests
         SchedulerTests.InitContextTest.tests
         SchedulerTests.ReceiveContextTest.tests
         -- SchedulerTests.FibonacciTest.tests
         SchedulerTests.AccountTransactionSpecs.tests
         SchedulerTests.BakerTransactions.tests
         SchedulerTests.Delegation.tests
         -- SchedulerTests.ContractSimpleTransfersSpec.tests
         SchedulerTests.CredentialTest.tests
         SchedulerTests.TransactionExpirySpec.tests
         SchedulerTests.BlockEnergyLimitSpec.tests
         SchedulerTests.TransactionGroupingSpec2.tests
         SchedulerTests.UpdateElectionDifficultySpec.tests
         SchedulerTests.SimpleTransferSpec.tests
         SchedulerTests.UpdateAccountKeys.tests
