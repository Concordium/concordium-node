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
module SchedulerTests.SmartContracts.V1.Upgrading (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Control.Monad
import Data.Serialize(runPut, Serialize (put))
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
import System.IO.Unsafe


initialBlockState :: BlockState PV5
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

-- |A module that is used as a base for upgrading.
upgrading0SourceFile :: FilePath
upgrading0SourceFile = "./testdata/contracts/v1/upgrading_0.wasm"

-- |A v1 module with a matching contract name of 'upgrading0SourceFile' 
-- so it should always be possible to upgrade to this from a contract based on 
-- the former mentioned module.
upgrading1SourceFile :: FilePath
upgrading1SourceFile = "./testdata/contracts/v1/upgrading_1.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion1 :: WasmVersion
wasmModVersion1 = V1

testCases :: TestCase PV5
testCases = 
    TestCase
    { tcName = "Checkpointing 1"
    , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ -- Deploy `upgrading_0.wasm
        ( TJSON { payload = DeployModule wasmModVersion1 upgrading0SourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck upgrading0SourceFile), emptySpec)
        )
      , -- Deploy upgrading_1.wasm
        ( TJSON { payload = DeployModule wasmModVersion1 upgrading1SourceFile
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck upgrading1SourceFile), emptySpec)
        )
        -- Initialize upgrading_0.wasm
      , ( TJSON { payload = InitContract 0 wasmModVersion1 upgrading0SourceFile "init_a" ""
                , metadata = makeDummyHeader alesAccount 3 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck upgrading0SourceFile "init_a"), emptySpec)
        )
      ,      
        -- Invoke the `upgrade` by calling 'a.upgrade' with the resulting 'ModuleRef' of 
        -- deploying upgrade_1.wasm.
        ( TJSON { payload = Update 1 (Types.ContractAddress 0 0) "a.upgrade" parameters
                , metadata = makeDummyHeader alesAccount 4 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess, emptySpec)
        )
      ,
        -- Invoke `new` which is only accessible after the module upgrade
        ( TJSON { payload = Update 1 (Types.ContractAddress 0 0) "a.new" BSS.empty
                , metadata = makeDummyHeader alesAccount 5 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess, emptySpec)
        )
      ]
    }  
  where
    parameters = BSS.toShort $ runPut $ do        
        -- The 'ModuleRef' to the desired module to upgrade to.
        put $! getModRefFromFile upgrading0SourceFile
    -- ensure the test case is successful        
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess msg Types.TxSuccess{..} = if length vrEvents == 3
      then return ()
      else assertFailure $ msg ++ " unexepcted no. of events " ++ show (length vrEvents) ++ " expected 3."

-- |Get a 'ModuleRef' from a given 'Module' specified via the 
-- 'FilePath'.
getModRefFromFile :: FilePath -> Types.ModuleRef
getModRefFromFile f = unsafePerformIO $ do
        moduleSource <- BS.readFile f
        return $! Types.ModuleRef $! Hash.hash moduleSource

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


tests :: Spec
tests = describe "V1: Upgrade" $ do
    mkSpec testCases

