{-# LANGUAGE OverloadedStrings #-}
{-| todo doc
-}
module SchedulerTests.SmartContracts.V1.Checkpointing (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Control.Monad
import Data.Serialize(runPut, putWord64le, putByteString, putWord16le)

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
import Debug.Trace

initialBlockState :: BlockState PV4
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

checkpointingSourceFile :: FilePath
checkpointingSourceFile = "./testdata/contracts/v1/checkpointing.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

testCases :: [TestCase PV4]
testCases =
  [ TestCase
    { tcName = "Checkpointing"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion checkpointingSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary deploymentCostCheck, emptySpec)
        )       
      , ( TJSON { payload = InitContract 0 wasmModVersion checkpointingSourceFile "init_a" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary initializationCostCheck, checkpointingSpec 0)
        )
      ,
        ( TJSON { payload = InitContract 0 wasmModVersion checkpointingSourceFile "init_b" ""
                , metadata = makeDummyHeader alesAccount 3 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary initializationCostCheck, checkpointingSpec 0)
        )
      ,
        ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "a.a_test_one" testOneArgs
                , metadata = makeDummyHeader alesAccount 4 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess, checkpointingSpec 0)
        )
      ]
    }
  ]
  where
    testOneArgs = BSS.toShort $ runPut $ do
          putWord64le 1 -- contract index of contract B
          putWord64le 0 -- contract subindex
          putWord16le (fromIntegral (BS.length forwardParameter)) -- length of parameter
          putByteString forwardParameter
          putWord16le (fromIntegral (BSS.length "b_test_one")) -- contract b's receive function.
          putByteString "b_test_one" -- entrypoint name
          putWord64le 0 -- amount
    forwardParameter = runPut $ do
          putWord64le 0 -- index of contract A
          putWord64le 0 -- subindex of the counter contract
          putWord16le 0 -- length of the empty parameter
          putWord16le (fromIntegral (BSS.length "a_test_one_modify"))
          putByteString "a_test_one_modify" -- entrypoint name
          putWord64le 0 -- amount
    checkpointingSpec _ _ = return ()
    deploymentCostCheck :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    deploymentCostCheck _ Types.TransactionSummary{..} = do
      checkSuccess "Module deployment failed: " tsResult
      moduleSource <- BS.readFile checkpointingSourceFile
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
          moduleSource <- BS.readFile checkpointingSourceFile
          let modLen = fromIntegral $ BS.length moduleSource
              modRef = Types.ModuleRef (Hash.hash moduleSource)
              payloadSize = Types.payloadSize (Types.encodePayload (Types.InitContract 0 modRef (InitName "init_a") (Parameter "")))
              -- size of the transaction minus the signatures.
              txSize = Types.transactionHeaderSize + fromIntegral payloadSize
              -- transaction is signed with 1 signature
              baseTxCost = Cost.baseCost txSize 1
              -- lower bound on the cost of the transaction, assuming no interpreter energy
              -- The state size of A is 0 and larger for B. We put the lower bound at A's size.
              costLowerBound = baseTxCost + Cost.initializeContractInstanceCost 0 modLen (Just 0)
          unless (tsEnergyCost >= costLowerBound) $
            assertFailure $ "Actual initialization cost " ++ show tsEnergyCost ++ " not more than lower bound " ++ show costLowerBound
    -- ensure the transaction is successful
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess msg Types.TxSuccess{..} = if (length vrEvents) == 3
      then return ()
      else assertFailure $ "Unexepcted no. of events " ++ show (length vrEvents) ++ " expected 3."

tests :: Spec
tests = describe "V1: Checkpointing." $
  mkSpecs testCases
