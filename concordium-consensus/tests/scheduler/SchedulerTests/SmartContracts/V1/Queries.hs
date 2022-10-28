{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-| This module tests the chain queries which was implemented as part of P5. -}
module SchedulerTests.SmartContracts.V1.Queries (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual, assertBool)

import Lens.Micro.Platform
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Control.Monad
import qualified Data.Set as Set
import Data.Serialize(runPut, Serialize (put), putWord32le, putWord64le)
import qualified Data.Text as T
import qualified Data.List as List

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Scheduler.Runner
import qualified Concordium.TransactionVerification as TVer

import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.Wasm
import qualified Concordium.Cost as Cost

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.TestUtils
import Concordium.Types.UpdateQueues (currentParameters)

thomasBalance :: Types.Amount
thomasBalance = 100000000

initialBlockState :: BlockState PV5
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount thomasBalance) Acc.emptyAccounts)

blockEnergyRate :: Types.EnergyRate
blockEnergyRate = initialBlockState ^. blockUpdates . currentParameters . Types.energyRate

accountBalanceSourceFile :: FilePath
accountBalanceSourceFile = "./testdata/contracts/v1/queries-account-balance.wasm"

accountBalanceTestCase :: TestCase PV5
accountBalanceTestCase =
  TestCase { tcName = "Simple account balance query"
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
      putWord64le 0
      putWord64le 0

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
      putWord64le . fromIntegral $ thomasBalance - costUpperBound - updateAmount -- expected public balance
      putWord64le 0 -- expected staked amount
      putWord64le 0 -- expected locked amount

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
      putWord64le 123
      putWord64le (fromIntegral (thomasBalance + 123)) -- expected public balance
      put (0 :: Types.Amount) -- expected staked amount
      put (0 :: Types.Amount) -- expected locked amount

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
      putWord64le $ fromIntegral $ initAmount + updateAmount -- Expected balance.

    eventsCheck :: [Types.Event] -> Expectation
    eventsCheck events = do
        -- Check the number of events:
        -- - 1 event for a succesful update to the contract.
        eventsLengthCheck 1 events

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

-- | Check the transaction rejected, taking a function to check the reason.
rejectWithReasonCheck :: (Types.RejectReason -> Expectation) -> TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
rejectWithReasonCheck checkReason _ summary = case Types.tsResult summary of
  Types.TxReject {..} -> checkReason vrRejectReason
  Types.TxSuccess {} -> assertFailure "Transaction succeeded unexpectedly"

-- | Check the number of events is as expected.
eventsLengthCheck :: Int -> [Types.Event] -> Expectation
eventsLengthCheck expected events = unless (length events == expected) $
  assertFailure $ "Unexpected number of events produced: " ++ show (length events) ++ " where the expected was " ++ show expected


tests :: Spec
tests = describe "V1: Queries" $ mkSpecs [
    accountBalanceTestCase,
    accountBalanceInvokerTestCase,
    accountBalanceTransferTestCase,
    accountBalanceMissingAccountTestCase,
    contractBalanceTestCase
  ]
