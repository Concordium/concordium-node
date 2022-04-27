{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-| This module tests basic V1 state operations with the recorder contract.
-}
module SchedulerTests.SmartContracts.V1.Recorder (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Data.Serialize(runPut, putWord64le)
import Lens.Micro.Platform
import Control.Monad

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Scheduler.Runner
import qualified Concordium.TransactionVerification as TVer

import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState.Instances
import Concordium.GlobalState.Basic.BlockState
import Concordium.Wasm
import qualified Concordium.Cost as Cost

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.TestUtils


initialBlockState :: BlockState PV4
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

recorderSourceFile :: FilePath
recorderSourceFile = "./testdata/contracts/v1/record-parameters.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

testCases :: [TestCase PV4]
testCases =
  [ TestCase
    { tcName = "Record data in a contract."
    , tcParameters = (defaultParams @PV4) {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion recorderSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary deploymentCostCheck, emptySpec)
        )
      , ( TJSON { payload = InitContract 0 wasmModVersion recorderSourceFile "init_recorder" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary initializationCostCheck, recorderSpec 0)
        )
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "recorder.record_u64" (BSS.toShort (runPut (putWord64le 20)))
                , metadata = makeDummyHeader alesAccount 3 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess , recorderSpec 20)
        )
        -- and then again
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "recorder.record_u64" (BSS.toShort (runPut (putWord64le 40)))
                , metadata = makeDummyHeader alesAccount 4 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess , recorderSpec 60)
        )
      ]
     }
  ]

  where
        deploymentCostCheck :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
        deploymentCostCheck _ Types.TransactionSummary{..} = do
          checkSuccess "Module deployment failed: " tsResult
          moduleSource <- BS.readFile recorderSourceFile
          let len = fromIntegral $ BS.length moduleSource
              -- size of the module deploy payload
              payloadSize = Types.payloadSize (Types.encodePayload (Types.DeployModule (WasmModuleV1 (WasmModuleV ModuleSource{..}))))
              -- size of the transaction minus the signatures.
              txSize = Types.transactionHeaderSize + fromIntegral payloadSize
              -- transaction is signed with 1 signature
          assertEqual "Deployment has correct cost " (Cost.baseCost txSize 1 + Cost.deployModuleCost len) tsEnergyCost

        -- check that the initialization cost was at least the administrative cost.
        -- It is not practical to check the exact cost because the execution cost of the init function is hard to
        -- have an independent number for, other than executing.
        initializationCostCheck :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
        initializationCostCheck _ Types.TransactionSummary{..} = do
          checkSuccess "Contract initialization failed: " tsResult
          moduleSource <- BS.readFile recorderSourceFile
          let modLen = fromIntegral $ BS.length moduleSource
              modRef = Types.ModuleRef (Hash.hash moduleSource)
              payloadSize = Types.payloadSize (Types.encodePayload (Types.InitContract 0 modRef (InitName "init_recorder") (Parameter "")))
              -- size of the transaction minus the signatures.
              txSize = Types.transactionHeaderSize + fromIntegral payloadSize
              -- transaction is signed with 1 signature
              baseTxCost = Cost.baseCost txSize 1
              -- lower bound on the cost of the transaction, assuming no interpreter energy
              -- we know the size of the state should be 8 bytes
              costLowerBound = baseTxCost + Cost.initializeContractInstanceCost 0 modLen (Just 0)
          unless (tsEnergyCost >= costLowerBound) $
            assertFailure $ "Actual initialization cost " ++ show tsEnergyCost ++ " not more than lower bound " ++ show costLowerBound

        -- ensure the transaction is successful
        ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
        ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed: " tsResult

        checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
        checkSuccess _ _ = return ()

        recorderSpec n bs = specify "Contract state" $
          case getInstance (Types.ContractAddress 0 0) (bs ^. blockInstances) of
            Nothing -> assertFailure "Instance at <0,0> does not exist."
            Just istance -> do
              case istance of
                InstanceV0 _ -> assertFailure "Expected V1 instance since a V1 module is deployed, but V0 encountered."
                InstanceV1 InstanceV{_instanceVModel=InstanceStateV1 s} -> do
                  -- since we inserted 60 values we expect to find keys on all those indices
                  forM_ [1..n] $ \idx ->
                    StateV1.lookupKey s (runPut (putWord64le (idx-1))) >>= \case
                      Nothing -> assertFailure $ "Failed to find key " ++ show (idx-1)
                      Just _ -> return ()
                  StateV1.lookupKey s (runPut (putWord64le n)) >>= \case
                    Nothing -> return ()
                    Just _ -> assertFailure $ "Found key " ++ show n ++ ", but did not expect to."
              assertEqual "Contract has 0 CCD." (Types.Amount 0) (instanceAmount istance)

tests :: Spec
tests = describe "V1: Record 20 + 40 strings." $
  mkSpecs testCases
