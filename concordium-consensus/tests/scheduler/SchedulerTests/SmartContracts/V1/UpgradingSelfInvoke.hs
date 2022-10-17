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


initialBlockState :: BlockState PV5
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

sourceFile0 :: FilePath
sourceFile0 = "./testdata/contracts/v1/upgrading-self-invoke0.wasm"

sourceFile1 :: FilePath
sourceFile1 = "./testdata/contracts/v1/upgrading-self-invoke1.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

testCase1IO :: IO (TestCase PV5)
testCase1IO = do
  moduleRef <- Types.ModuleRef . Hash.hash <$> BS.readFile sourceFile1
  let upgradeParameters = BSS.toShort $ runPut $ put moduleRef
  return $ TestCase { tcName = "Upgrading self invoke"
  , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
  , tcTransactions =
    [ ( TJSON { payload = DeployModule wasmModVersion sourceFile0
              , metadata = makeDummyHeader alesAccount 1 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck sourceFile0), emptySpec))
    , ( TJSON { payload = InitContract 0 wasmModVersion sourceFile0 "init_contract" ""
              , metadata = makeDummyHeader alesAccount 2 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (initializationCostCheck sourceFile0 "init_contract"), emptySpec))
    , ( TJSON { payload = DeployModule wasmModVersion sourceFile1
              , metadata = makeDummyHeader alesAccount 3 100000
              , keys = [(0,[(0, alesKP)])]
              }
      , (SuccessWithSummary (deploymentCostCheck sourceFile1), emptySpec))

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
      Types.TxSuccess {..} -> if length vrEvents == 7
        then return ()
        else assertFailure $ "Update succeeded but with unexpected number of events: " ++ show (length vrEvents)

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
  testCase1 <- testCase1IO
  return $ describe "V1: Upgrade" $ do
    mkSpecs [testCase1]
