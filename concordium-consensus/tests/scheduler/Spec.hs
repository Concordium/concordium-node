module Main where

import qualified SchedulerTests.AccountTransactionSpecs (tests)
import qualified SchedulerTests.BakerTransactions (tests)
import qualified SchedulerTests.BlockEnergyLimitSpec (tests)
import qualified SchedulerTests.ChainMetatest (tests)
import qualified SchedulerTests.ConfigureBaker (tests)
import qualified SchedulerTests.Delegation (tests)
import qualified SchedulerTests.EncryptedTransfersTest (tests)
import qualified SchedulerTests.FibonacciSelfMessageTest (tests)
import qualified SchedulerTests.InitContextTest (tests)
import qualified SchedulerTests.InitPoliciesTest (tests)
import qualified SchedulerTests.InitialAccountCreationSpec (tests)
import qualified SchedulerTests.MaxIncomingAmountsTest (tests)
import qualified SchedulerTests.Payday (tests)
import qualified SchedulerTests.RandomBakerTransactions (tests)
import qualified SchedulerTests.ReceiveContextTest (tests)
import qualified SchedulerTests.RejectReasons (tests)
import qualified SchedulerTests.RejectReasonsRustContract (tests)
import qualified SchedulerTests.SimpleTransferSpec (tests)
import qualified SchedulerTests.SimpleTransfersTest (tests)
import qualified SchedulerTests.StakedAmountLocked (tests)
import qualified SchedulerTests.TokenModule (tests)
import qualified SchedulerTests.TransactionExpirySpec (tests)
import qualified SchedulerTests.TransactionGroupingSpec2 (tests)
import qualified SchedulerTests.TransfersWithScheduleTest (tests)
import qualified SchedulerTests.TrySendTest (tests)
import qualified SchedulerTests.UpdateAccountKeys (tests)
import qualified SchedulerTests.UpdateCredentials (tests)

import qualified SchedulerTests.SmartContracts.V0.RelaxedRestrictions (tests)
import qualified SchedulerTests.SmartContracts.V0.SmartContractTests (tests)

import qualified SchedulerTests.SmartContracts.Invoke (tests)
import qualified SchedulerTests.SmartContracts.V1.AccountSignatureChecks (tests)
import qualified SchedulerTests.SmartContracts.V1.AllNewHostFunctions (tests)
import qualified SchedulerTests.SmartContracts.V1.Caller (tests)
import qualified SchedulerTests.SmartContracts.V1.Checkpointing (tests)
import qualified SchedulerTests.SmartContracts.V1.Counter (tests)
import qualified SchedulerTests.SmartContracts.V1.CrossMessaging (tests)
import qualified SchedulerTests.SmartContracts.V1.CustomSectionSize (tests)
import qualified SchedulerTests.SmartContracts.V1.ErrorCodes (tests)
import qualified SchedulerTests.SmartContracts.V1.Fallback (tests)
import qualified SchedulerTests.SmartContracts.V1.InspectModuleReferenceAndContractName (tests)
import qualified SchedulerTests.SmartContracts.V1.Iterator (tests)
import qualified SchedulerTests.SmartContracts.V1.P6WasmFeatures (tests)
import qualified SchedulerTests.SmartContracts.V1.Queries (tests)
import qualified SchedulerTests.SmartContracts.V1.QueriesPersistent (tests)
import qualified SchedulerTests.SmartContracts.V1.Recorder (tests)
import qualified SchedulerTests.SmartContracts.V1.RelaxedRestrictions (tests)
import qualified SchedulerTests.SmartContracts.V1.SelfBalance (tests)
import qualified SchedulerTests.SmartContracts.V1.Transfer (tests)
import qualified SchedulerTests.SmartContracts.V1.TransfersPersistent (tests)
import qualified SchedulerTests.SmartContracts.V1.Upgrading (tests)
import qualified SchedulerTests.SmartContracts.V1.UpgradingPersistent (tests)
import qualified SchedulerTests.SmartContracts.V1.ValidInvalidModules (tests)

import qualified SchedulerTests.KonsensusV1.EpochTransition (tests)

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
    SchedulerTests.TransactionExpirySpec.tests
    SchedulerTests.BlockEnergyLimitSpec.tests
    SchedulerTests.TransactionGroupingSpec2.tests
    SchedulerTests.SimpleTransferSpec.tests
    SchedulerTests.UpdateAccountKeys.tests
    SchedulerTests.UpdateCredentials.tests
    SchedulerTests.TransfersWithScheduleTest.tests
    SchedulerTests.EncryptedTransfersTest.tests
    SchedulerTests.MaxIncomingAmountsTest.tests
    SchedulerTests.StakedAmountLocked.tests
    SchedulerTests.RejectReasons.tests
    SchedulerTests.RejectReasonsRustContract.tests
    SchedulerTests.SmartContracts.V0.SmartContractTests.tests
    SchedulerTests.SmartContracts.V0.RelaxedRestrictions.tests
    SchedulerTests.SmartContracts.V1.Counter.tests
    SchedulerTests.SmartContracts.V1.Transfer.tests
    SchedulerTests.SmartContracts.V1.CrossMessaging.tests
    SchedulerTests.SmartContracts.Invoke.tests
    SchedulerTests.SmartContracts.V1.ErrorCodes.tests
    SchedulerTests.SmartContracts.V1.ValidInvalidModules.tests
    SchedulerTests.SmartContracts.V1.SelfBalance.tests
    SchedulerTests.SmartContracts.V1.Recorder.tests
    SchedulerTests.SmartContracts.V1.Iterator.tests
    SchedulerTests.SmartContracts.V1.Fallback.tests
    SchedulerTests.SmartContracts.V1.Checkpointing.tests
    SchedulerTests.SmartContracts.V1.AllNewHostFunctions.tests
    SchedulerTests.SmartContracts.V1.Upgrading.tests
    SchedulerTests.SmartContracts.V1.Queries.tests
    SchedulerTests.SmartContracts.V1.RelaxedRestrictions.tests
    SchedulerTests.SmartContracts.V1.UpgradingPersistent.tests
    SchedulerTests.SmartContracts.V1.TransfersPersistent.tests
    SchedulerTests.SmartContracts.V1.QueriesPersistent.tests
    SchedulerTests.Payday.tests
    SchedulerTests.Delegation.tests
    SchedulerTests.ConfigureBaker.tests
    SchedulerTests.SmartContracts.V1.P6WasmFeatures.tests
    SchedulerTests.SmartContracts.V1.CustomSectionSize.tests
    SchedulerTests.SmartContracts.V1.AccountSignatureChecks.tests
    SchedulerTests.SmartContracts.V1.InspectModuleReferenceAndContractName.tests
    SchedulerTests.SmartContracts.V1.Caller.tests
    SchedulerTests.KonsensusV1.EpochTransition.tests
    SchedulerTests.TokenModule.tests
