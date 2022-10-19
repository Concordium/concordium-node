{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-| This module tests the upgrading feature which was implemented as part of P5.
    In particular it tests that an initialized instance which has been deployed
    in P5 can upgrade it's underlying module i.e. the artifact via the host function
    'upgrade'

    * Test case 1
        The scenario checks that a contract A which is initialized with some state can 
        be upgraded to a new module. The new module contains a view function that returns 
        the state stored in the prior version is still available.

    * Test case 2.. todo.

-}
module SchedulerTests.SmartContracts.V1.UpgradingSelfInvoke (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Control.Monad
import Data.Serialize(runPut, putWord64le, putByteString, putWord16le, encode, Serialize (put))
import qualified Data.Text as T
import qualified Data.List as List

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Scheduler.Runner
import qualified Concordium.TransactionVerification as TVer

import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.Wasm
import qualified Concordium.Cost as Cost

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.TestUtils
import Concordium.Scheduler.Types (Event(..))


initialBlockState :: BlockState PV5
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

selfInvokeSourceFile0 :: FilePath
selfInvokeSourceFile0 = "./testdata/contracts/v1/upgrading-self-invoke0.wasm"

selfInvokeSourceFile1 :: FilePath
selfInvokeSourceFile1 = "./testdata/contracts/v1/upgrading-self-invoke1.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

selfInvokeTestCaseIO :: IO (TestCase PV5)
selfInvokeTestCaseIO = do
  moduleRef <- getModuleRef @V1 . WasmModuleV . ModuleSource <$> BS.readFile selfInvokeSourceFile0
  let upgradeParameters = BSS.toShort $ runPut $ put moduleRef
  return $ TestCase { tcName = "Upgrading self invoke"
  , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
  , tcTransactions =
    [ ( TJSON { payload = DeployModule wasmModVersion selfInvokeSourceFile0
              , metadata = makeDummyHeader alesAccount 1 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck selfInvokeSourceFile0), emptySpec))
    , ( TJSON { payload = InitContract 0 wasmModVersion selfInvokeSourceFile0 "init_contract" ""
              , metadata = makeDummyHeader alesAccount 2 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (initializationCostCheck selfInvokeSourceFile0 "init_contract"), emptySpec))
    , ( TJSON { payload = DeployModule wasmModVersion selfInvokeSourceFile1
              , metadata = makeDummyHeader alesAccount 3 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck selfInvokeSourceFile1), emptySpec))

    , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters
              , metadata = makeDummyHeader alesAccount 4 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary ensureSuccess, emptySpec))
    ]
  }
  where
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ summary = case Types.tsResult summary of
      Types.TxReject {..} -> assertFailure $ "Update failed with " ++ show vrRejectReason
      Types.TxSuccess {..} -> do
        -- Check the number of events:
        -- - 3 events from invoking another contract ('contract.name').
        -- - 3 events from upgrading.
        -- - 3 events from invoking again  ('contract.name').
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        unless (length vrEvents == 10) $ assertFailure $ "Update succeeded but with unexpected number of events: " ++ show (length vrEvents)
        -- Find and check the upgrade event
        case List.find isUpgradeEvent vrEvents of
          Nothing -> assertFailure "Missing event: Upgraded"
          Just Upgraded{..} | euFrom /= euTo -> assertFailure "Event Upgraded is incorrect"
          Just _ -> return ()
        -- Ensure only one upgrade event
        unless (List.length (filter isUpgradeEvent vrEvents) == 1) $ assertFailure "Multiple events: Upgraded"

missingModuleSourceFile :: FilePath
missingModuleSourceFile = "./testdata/contracts/v1/upgrading-missing-module.wasm"

missingModuleTestCase :: TestCase PV5
missingModuleTestCase = TestCase { tcName = "Upgrading to a missing module fails"
  , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
  , tcTransactions =
    [ ( TJSON { payload = DeployModule wasmModVersion missingModuleSourceFile
              , metadata = makeDummyHeader alesAccount 1 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck missingModuleSourceFile), emptySpec))
    , ( TJSON { payload = InitContract 0 wasmModVersion missingModuleSourceFile "init_contract" ""
              , metadata = makeDummyHeader alesAccount 2 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (initializationCostCheck missingModuleSourceFile "init_contract"), emptySpec))

    , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" ""
              , metadata = makeDummyHeader alesAccount 3 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary ensureSuccess, emptySpec))
    ]
  }
  where
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ summary = case Types.tsResult summary of
      Types.TxReject {..} -> assertFailure $ "Update failed with " ++ show vrRejectReason
      Types.TxSuccess {..} -> do
        -- Check the number of events:
        -- - 2 events from upgrading (interrupt and resume).
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        unless (length vrEvents == 3) $ assertFailure $ "Update succeeded but with unexpected number of events: " ++ show (length vrEvents)
        -- Find and check for upgrade events
        unless (List.any isUpgradeEvent vrEvents) $ assertFailure "Found unexpected event: Upgraded"

missingContractSourceFile0 :: FilePath
missingContractSourceFile0 = "./testdata/contracts/v1/upgrading-missing-contract0.wasm"

missingContractSourceFile1 :: FilePath
missingContractSourceFile1 = "./testdata/contracts/v1/upgrading-missing-contract1.wasm"


missingContractTestCaseIO :: IO (TestCase PV5)
missingContractTestCaseIO = do
  moduleRef <- getModuleRef @V1 . WasmModuleV . ModuleSource <$> BS.readFile missingContractSourceFile1
  let upgradeParameters = BSS.toShort $ runPut $ put moduleRef
  return $ TestCase { tcName = "Upgrading to a module without matching contract fails"
  , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
  , tcTransactions =
    [ ( TJSON { payload = DeployModule wasmModVersion missingContractSourceFile0
              , metadata = makeDummyHeader alesAccount 1 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck missingContractSourceFile0), emptySpec))
    , ( TJSON { payload = InitContract 0 wasmModVersion missingContractSourceFile0 "init_contract" ""
              , metadata = makeDummyHeader alesAccount 2 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (initializationCostCheck missingContractSourceFile0 "init_contract"), emptySpec))
    , ( TJSON { payload = DeployModule wasmModVersion missingContractSourceFile1
              , metadata = makeDummyHeader alesAccount 3 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck missingContractSourceFile1), emptySpec))

    , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters
              , metadata = makeDummyHeader alesAccount 4 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary ensureSuccess, emptySpec))
    ]
  }
  where
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ summary = case Types.tsResult summary of
      Types.TxReject {..} -> assertFailure $ "Update failed with " ++ show vrRejectReason
      Types.TxSuccess {..} -> do
        -- Check the number of events:
        -- - 2 events from upgrading (interrupt and resume).
        -- - 1 event for a succesful update to the contract ('contract.upgrade').
        unless (length vrEvents == 3) $ assertFailure $ "Update succeeded but with unexpected number of events: " ++ show (length vrEvents)
        -- Find and check for upgrade events
        unless (List.any isUpgradeEvent vrEvents) $ assertFailure "Found unexpected event: Upgraded"

-- | Check if some event is the Upgraded event.
isUpgradeEvent :: Event -> Bool
isUpgradeEvent event = case event of
  Upgraded{} -> True
  _ -> False

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
      payloadSize = Types.payloadSize (Types.encodePayload (Types.DeployModule (WasmModuleV0 (WasmModuleV ModuleSource{..}))))
      -- size of the transaction minus the signatures.
      txSize = Types.transactionHeaderSize + fromIntegral payloadSize
      -- transaction is signed with 1 signature
  assertEqual "Deployment has correct cost " (Cost.baseCost txSize 1 + Cost.deployModuleCost len) tsEnergyCost
  where
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess _ _ = return ()


tests :: IO Spec
tests = do
  selfInvokeTestCase <- selfInvokeTestCaseIO
  missingContractTestCase <- missingContractTestCaseIO
  return $ describe "V1: Upgrade" $ do
    mkSpecs [
       selfInvokeTestCase
      , missingModuleTestCase
      , missingContractTestCase
      ]
