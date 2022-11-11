{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests calling a contract which makes use of an iterator.
--    The checks are being performed in the contract itself so if invoking the
--    contract completes successfully then this implies that the tests have done so as well.
--    Note. as per above no checks are being performed in this file wrt. the state etc. after execution etc.
module SchedulerTests.SmartContracts.V1.Iterator (tests) where

import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.TransactionVerification as TVer

import qualified Concordium.Cost as Cost
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.Wasm

import Concordium.Crypto.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Scheduler.DummyData
import Concordium.Types.DummyData

import SchedulerTests.TestUtils

initialBlockState :: BlockState PV4
initialBlockState =
    blockStateWithAlesAccount
        100000000
        (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

iteratorSourceFile :: FilePath
iteratorSourceFile = "./testdata/contracts/v1/iterator.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

testCases :: [TestCase PV4]
testCases =
    [ TestCase
        { tcName = "Iterator",
          tcParameters = (defaultParams @PV4){tpInitialBlockState = initialBlockState},
          tcTransactions =
            [   ( TJSON
                    { payload = DeployModule wasmModVersion iteratorSourceFile,
                      metadata = makeDummyHeader alesAccount 1 100000,
                      keys = [(0, [(0, alesKP)])]
                    },
                  (SuccessWithSummary deploymentCostCheck, emptySpec)
                ),
                ( TJSON
                    { payload = InitContract 0 wasmModVersion iteratorSourceFile "init_iterator" "",
                      metadata = makeDummyHeader alesAccount 2 100000,
                      keys = [(0, [(0, alesKP)])]
                    },
                  (SuccessWithSummary initializationCostCheck, iteratorSpec)
                ),
                ( TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "iterator.iteratetest" BSS.empty,
                      metadata = makeDummyHeader alesAccount 3 100000,
                      keys = [(0, [(0, alesKP)])]
                    },
                  (SuccessWithSummary ensureSuccess, iteratorSpec)
                ),
                ( TJSON
                    { payload = Update 0 (Types.ContractAddress 0 0) "iterator.lockingtest" BSS.empty,
                      metadata = makeDummyHeader alesAccount 4 100000,
                      keys = [(0, [(0, alesKP)])]
                    },
                  (SuccessWithSummary ensureSuccess, iteratorSpec)
                )
            ]
        }
    ]
  where
    iteratorSpec _ = return ()
    deploymentCostCheck :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    deploymentCostCheck _ Types.TransactionSummary{..} = do
        checkSuccess "Module deployment failed: " tsResult
        moduleSource <- BS.readFile iteratorSourceFile
        let len = fromIntegral $ BS.length moduleSource
            -- size of the module deploy payload
            payloadSize = Types.payloadSize (Types.encodePayload (Types.DeployModule (WasmModuleV0 (WasmModuleV ModuleSource{..}))))
            -- size of the transaction minus the signatures.
            txSize = Types.transactionHeaderSize + fromIntegral payloadSize
        -- transaction is signed with 1 signature
        assertEqual "Deployment has correct cost " (Cost.baseCost txSize 1 + Cost.deployModuleCost len) tsEnergyCost
    -- This only checks that the cost of initialization is correct.
    -- If the state was not set up correctly the latter tests in the suite will fail.
    initializationCostCheck :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    initializationCostCheck _ Types.TransactionSummary{..} = do
        checkSuccess "Contract initialization failed: " tsResult
        moduleSource <- BS.readFile iteratorSourceFile
        let modLen = fromIntegral $ BS.length moduleSource
            modRef = Types.ModuleRef (Hash.hash moduleSource)
            payloadSize = Types.payloadSize (Types.encodePayload (Types.InitContract 0 modRef (InitName "init_iterator") (Parameter "")))
            -- size of the transaction minus the signatures.
            txSize = Types.transactionHeaderSize + fromIntegral payloadSize
            -- transaction is signed with 1 signature
            baseTxCost = Cost.baseCost txSize 1
            -- lower bound on the cost of the transaction, assuming no interpreter energy
            -- we know the size of the state should be 8 bytes
            costLowerBound = baseTxCost + Cost.initializeContractInstanceCost 0 modLen (Just 8)
        unless (tsEnergyCost >= costLowerBound) $
            assertFailure $
                "Actual initialization cost " ++ show tsEnergyCost ++ " not more than lower bound " ++ show costLowerBound
    -- ensure the transaction is successful
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess _ _ = return ()

tests :: Spec
tests =
    describe "V1: Iterator." $
        mkSpecs testCases
