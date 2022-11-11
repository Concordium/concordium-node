{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-| This module tests the chain queries which were implemented as part of P5. -}
module SchedulerTests.SmartContracts.V1.Queries (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import Lens.Micro.Platform
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Control.Monad
import Data.Serialize(runPut, Serialize (put), putWord64le)
import qualified Data.Text as T

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Scheduler.Runner
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types.Accounts

import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.Wasm
import qualified Concordium.Cost as Cost

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.TestUtils
import Concordium.Types.UpdateQueues (currentParameters)
import qualified Concordium.Wasm as Wasm

thomasBalance :: Types.Amount
thomasBalance = 100000000

initialBlockState :: BlockState PV5
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount thomasBalance) Acc.emptyAccounts)

initialBlockStateWithStakeAndSchedule :: BlockState PV5
initialBlockStateWithStakeAndSchedule = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds extra Acc.emptyAccounts)
    where extra = (mkAccount @'AccountV2 thomasVK thomasAccount thomasBalance) {
            _accountReleaseSchedule = addReleases ([(Types.Timestamp maxBound, 123)], Types.TransactionHashV0 (Hash.hash "")) emptyAccountReleaseSchedule,
            _accountStaking = AccountStakeDelegate AccountDelegationV1 {
                _delegationIdentity = 1,
                _delegationStakedAmount = 234,
                _delegationStakeEarnings = False,
                _delegationTarget = Types.DelegatePassive,
                _delegationPendingChange = NoChange
                }
            }

blockEnergyRate :: Types.EnergyRate
blockEnergyRate = initialBlockState ^. blockUpdates . currentParameters . Types.energyRate

accountBalanceSourceFile :: FilePath
accountBalanceSourceFile = "./testdata/contracts/v1/queries-account-balance.wasm"

accountBalanceTestCase :: TestCase PV5
accountBalanceTestCase =
  TestCase { tcName = "Simple account balance query"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockStateWithStakeAndSchedule}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 accountBalanceSourceFile
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck accountBalanceSourceFile), emptySpec))
             , ( TJSON { payload = InitContract 0 V1 accountBalanceSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck accountBalanceSourceFile "init_contract"), emptySpec))
             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
             ]
           }
  where
    parameters = BSS.toShort $ runPut $ do
      put thomasAccount
      putWord64le (fromIntegral thomasBalance) -- expected public balance
      putWord64le 234 -- expected staked balance
      putWord64le 123 -- expected locked balance

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        eventsLengthCheck 1 events

accountBalanceInvokerTestCase :: TestCase PV5
accountBalanceInvokerTestCase =
  TestCase { tcName = "Query the account balance of the invoker"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 accountBalanceSourceFile
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck accountBalanceSourceFile), emptySpec))
             , ( TJSON { payload = InitContract 0 V1 accountBalanceSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck accountBalanceSourceFile "init_contract"), emptySpec))
             , ( TJSON { payload = Update updateAmount (Types.ContractAddress 0 0) "contract.query" parameters
                       , metadata = makeDummyHeader thomasAccount 1 energyLimit
                       , keys = [(0,[(0, thomasKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
             ]
           }
  where
    energyLimit = 100000
    updateAmount = 123

    parameters = BSS.toShort $ runPut $ do
      put thomasAccount
      Wasm.putAmountLE $ thomasBalance - costUpperBound - updateAmount -- expected public balance
      Wasm.putAmountLE 0 -- expected staked amount
      Wasm.putAmountLE 0 -- expected locked amount

    costUpperBound = Types.computeCost blockEnergyRate energyLimit

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        eventsLengthCheck 1 events

accountBalanceTransferSourceFile :: FilePath
accountBalanceTransferSourceFile = "./testdata/contracts/v1/queries-account-balance-transfer.wasm"

accountBalanceTransferTestCase :: TestCase PV5
accountBalanceTransferTestCase =
  TestCase { tcName = "Contracts transfers to an account and then queries"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 accountBalanceTransferSourceFile
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck accountBalanceTransferSourceFile), emptySpec))
             , ( TJSON { payload = InitContract 123 V1 accountBalanceTransferSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck accountBalanceTransferSourceFile "init_contract"), emptySpec))
             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
             ]
           }
  where
    parameters = BSS.toShort $ runPut $ do
      put thomasAccount
      Wasm.putAmountLE 123
      Wasm.putAmountLE $ thomasBalance + 123 -- expected public balance
      Wasm.putAmountLE 0 -- expected staked amount
      Wasm.putAmountLE 0 -- expected locked amount

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 3 events for the transfer.
        -- - 1 event for a succesful update to the contract.
        eventsLengthCheck 4 events

accountBalanceMissingAccountSourceFile :: FilePath
accountBalanceMissingAccountSourceFile = "./testdata/contracts/v1/queries-account-balance-missing-account.wasm"

accountBalanceMissingAccountTestCase :: TestCase PV5
accountBalanceMissingAccountTestCase =
  TestCase { tcName = "Query the balance of a missing account"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 accountBalanceMissingAccountSourceFile
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck accountBalanceMissingAccountSourceFile), emptySpec))
             , ( TJSON { payload = InitContract 123 V1 accountBalanceMissingAccountSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck accountBalanceMissingAccountSourceFile "init_contract"), emptySpec))
             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
             ]
           }
  where
    parameters = BSS.toShort $ runPut $ do
      put $ accountAddressFrom 3

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract.
        eventsLengthCheck 1 events

contractBalanceSourceFile :: FilePath
contractBalanceSourceFile = "./testdata/contracts/v1/queries-contract-balance.wasm"

contractBalanceTestCase :: TestCase PV5
contractBalanceTestCase =
  TestCase { tcName = "Query the balance a contract"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 contractBalanceSourceFile
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck contractBalanceSourceFile), emptySpec))
             , ( TJSON { payload = InitContract 0 V1 contractBalanceSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck contractBalanceSourceFile "init_contract"), emptySpec))
             , ( TJSON { payload = InitContract initAmount V1 contractBalanceSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck contractBalanceSourceFile "init_contract"), emptySpec))
             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters
                       , metadata = makeDummyHeader alesAccount 4 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
             ]
           }
  where
    initAmount = 123

    parameters = BSS.toShort $ runPut $ do
      putWord64le 1 -- Index of the contract address.
      putWord64le 0 -- Subindex of the contract address.
      Wasm.putAmountLE initAmount -- Expected balance.

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract.
        eventsLengthCheck 1 events

contractBalanceSelfTestCase :: TestCase PV5
contractBalanceSelfTestCase =
  TestCase { tcName = "Query the balance of the contract itself"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 contractBalanceSourceFile
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck contractBalanceSourceFile), emptySpec))
             , ( TJSON { payload = InitContract initAmount V1 contractBalanceSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck contractBalanceSourceFile "init_contract"), emptySpec))
             , ( TJSON { payload = Update updateAmount (Types.ContractAddress 0 0) "contract.query" parameters
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
             ]
           }
  where
    initAmount = 123
    updateAmount = 456

    parameters = BSS.toShort $ runPut $ do
      putWord64le 0 -- Index of the contract address.
      putWord64le 0 -- Subindex of the contract address.
      Wasm.putAmountLE $ initAmount + updateAmount -- Expected balance.

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract.
        eventsLengthCheck 1 events

contractBalanceTransferSourceFile :: FilePath
contractBalanceTransferSourceFile = "./testdata/contracts/v1/queries-contract-balance-transfer.wasm"

contractBalanceTransferTestCase :: TestCase PV5
contractBalanceTransferTestCase =
  TestCase { tcName = "Query the balance a contract"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 contractBalanceTransferSourceFile
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck contractBalanceTransferSourceFile), emptySpec))
             , ( TJSON { payload = InitContract initAmount V1 contractBalanceTransferSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck contractBalanceTransferSourceFile "init_contract"), emptySpec))
             , ( TJSON { payload = Update updateAmount (Types.ContractAddress 0 0) "contract.query" parameters
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
             ]
           }
  where
    initAmount = 123
    updateAmount = 456
    transferAmount = 78

    parameters = BSS.toShort $ runPut $ do
      put thomasAccount
      Wasm.putAmountLE transferAmount
      putWord64le 0 -- Index of the contract address.
      putWord64le 0 -- Subindex of the contract address.
      Wasm.putAmountLE $ initAmount + updateAmount - transferAmount -- Expected balance.

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 3 events for transfering
        -- - 1 event for a succesful update to the contract.
        eventsLengthCheck 4 events

contractBalanceMissingContractSourceFile :: FilePath
contractBalanceMissingContractSourceFile = "./testdata/contracts/v1/queries-contract-balance-missing-contract.wasm"

contractBalanceMissingContractTestCase :: TestCase PV5
contractBalanceMissingContractTestCase =
  TestCase { tcName = "Query the balance of a missing contract"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 contractBalanceMissingContractSourceFile
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck contractBalanceMissingContractSourceFile), emptySpec))
             , ( TJSON { payload = InitContract 123 V1 contractBalanceMissingContractSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck contractBalanceMissingContractSourceFile "init_contract"), emptySpec))
             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
             ]
           }
  where
    parameters = BSS.toShort $ runPut $ do
      putWord64le 1 -- Contract address index
      putWord64le 0 -- Contract address subindex

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract.
        eventsLengthCheck 1 events

exchangeRatesSourceFile :: FilePath
exchangeRatesSourceFile = "./testdata/contracts/v1/queries-exchange-rates.wasm"

exchangeRatesTestCase :: TestCase PV5
exchangeRatesTestCase =
  TestCase { tcName = "Query the exchange rates"
           , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 exchangeRatesSourceFile
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck exchangeRatesSourceFile), emptySpec))
             , ( TJSON { payload = InitContract 0 V1 exchangeRatesSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck exchangeRatesSourceFile "init_contract"), emptySpec))
             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.query" parameters
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (successWithEventsCheck eventsCheck), emptySpec))
             ]
           }
  where
    parameters = BSS.toShort $ runPut $ do
      Wasm.putExchangeRateLE currentEuroPerEnergy
      Wasm.putExchangeRateLE currentAmountPerEnergy

    currentEuroPerEnergy = initialBlockState ^. blockUpdates . currentParameters . Types.euroPerEnergy
    currentAmountPerEnergy = initialBlockState ^. blockUpdates . currentParameters . Types.microGTUPerEuro

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract.
        eventsLengthCheck 1 events

allSourceFile :: FilePath
allSourceFile = "./testdata/contracts/v1/queries-all.wasm"

allTestCase :: TestCase PV4
allTestCase =
  TestCase { tcName = "Ensure all of the queries fail prior to PV5"
           , tcParameters = (defaultParams @PV4) {tpInitialBlockState=initialBlockStateP4}
           , tcTransactions =
             [ ( TJSON { payload = DeployModule V1 allSourceFile
                       , metadata = makeDummyHeader alesAccount 1 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (deploymentCostCheck allSourceFile), emptySpec))
             , ( TJSON { payload = InitContract 0 V1 allSourceFile "init_contract" ""
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (initializationCostCheck allSourceFile "init_contract"), emptySpec))

             , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.account_balance" ""
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (rejectWithReasonCheck rejectReasonCheck), emptySpec))

               , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.contract_balance" ""
                       , metadata = makeDummyHeader alesAccount 4 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (rejectWithReasonCheck rejectReasonCheck), emptySpec))

               , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.exchange_rates" ""
                       , metadata = makeDummyHeader alesAccount 5 100000
                       , keys = [(0,[(0, alesKP)])]
                       }
               , (SuccessWithSummary (rejectWithReasonCheck rejectReasonCheck), emptySpec))
             ]
           }
  where
    initialBlockStateP4 :: BlockState PV4
    initialBlockStateP4 = blockStateWithAlesAccount
      100000000
      (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount thomasBalance) Acc.emptyAccounts)

    rejectReasonCheck :: Types.RejectReason -> Expectation
    rejectReasonCheck reason =
      case reason of
          Types.RuntimeFailure -> return ()
          other -> assertFailure $ "Unexpected reject reason " ++ show other

-- This only checks that the cost of initialization is correct.
-- If the state was not set up correctly the latter tests in the suite will fail.
initializationCostCheck :: FilePath -> T.Text -> (TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation)
initializationCostCheck sourceFile initName _ Types.TransactionSummary{..} = do
  checkSuccess "Contract initialization failed: " tsResult
  moduleSource <- BS.readFile sourceFile
  let modLen = fromIntegral $ BS.length moduleSource
      modRef = Types.ModuleRef (Hash.hash moduleSource)
      payloadSize = Types.payloadSize (Types.encodePayload (Types.InitContract 0 modRef (InitName initName) (Parameter "")))
      -- size of the transaction minus the signatures.
      txSize = Types.transactionHeaderSize + fromIntegral payloadSize
      -- transaction is signed with 1 signature
      baseTxCost = Cost.baseCost txSize 1
      -- lower bound on the cost of the transaction, assuming no interpreter energy
      -- The state size of A is 0 and larger for B. We put the lower bound at A's size.
      costLowerBound = baseTxCost + Cost.initializeContractInstanceCost 0 modLen (Just 0)

  unless (tsEnergyCost >= costLowerBound) $
    assertFailure $ "Actual initialization cost " ++ show tsEnergyCost ++ " not more than lower bound " ++ show costLowerBound
  where
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess _ _ = return ()

deploymentCostCheck :: FilePath -> (TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation)
deploymentCostCheck sourceFile _ Types.TransactionSummary{..} = do
  checkSuccess "Module deployment failed: " tsResult
  moduleSource <- BS.readFile sourceFile
  let len = fromIntegral $ BS.length moduleSource
      -- size of the module deploy payload
      payloadSize = Types.payloadSize (Types.encodePayload (Types.DeployModule (WasmModuleV1 (WasmModuleV ModuleSource{..}))))
      -- size of the transaction minus the signatures.
      txSize = Types.transactionHeaderSize + fromIntegral payloadSize
      -- transaction is signed with 1 signature
  assertEqual "Deployment has correct cost " (Cost.baseCost txSize 1 + Cost.deployModuleCost len) tsEnergyCost
  where
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess _ _ = return ()

-- | Check the transaction succeeded, taking a function to check the events.
successWithEventsCheck :: ([Types.Event] -> Expectation) -> TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
successWithEventsCheck checkEvents _ summary = case Types.tsResult summary of
  Types.TxReject {..} -> assertFailure $ "Transaction rejected unexpectedly with " ++ show vrRejectReason
  Types.TxSuccess {..} -> checkEvents vrEvents

-- | Check the number of events is as expected.
eventsLengthCheck :: Int -> [Types.Event] -> Expectation
eventsLengthCheck expected events = unless (length events == expected) $
  assertFailure $ "Unexpected number of events produced: " ++ show (length events) ++ " where the expected was " ++ show expected

-- | Check the transaction rejected, taking a function to check the reason.
rejectWithReasonCheck :: (Types.RejectReason -> Expectation) -> TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
rejectWithReasonCheck checkReason _ summary = case Types.tsResult summary of
  Types.TxReject {..} -> checkReason vrRejectReason
  Types.TxSuccess {} -> assertFailure "Transaction succeeded unexpectedly"

tests :: Spec
tests = describe "V1: Queries" $ do
  mkSpecs [
    accountBalanceTestCase,
    accountBalanceInvokerTestCase,
    accountBalanceTransferTestCase,
    accountBalanceMissingAccountTestCase,
    contractBalanceTestCase,
    contractBalanceSelfTestCase,
    contractBalanceTransferTestCase,
    contractBalanceMissingContractTestCase,
    exchangeRatesTestCase]
  mkSpecs [allTestCase]
