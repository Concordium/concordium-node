{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-| This module tests calling a contract from a contract and inspecting the return
    message. Concretely it invokes a counter contract that maintains a 64-bit
    counter in its state.
-}
module SchedulerTests.SmartContracts.V1.Counter (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Data.Serialize(runPut, putWord64le, putByteString, putWord16le)
import Lens.Micro.Platform
import Control.Monad

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Scheduler.Runner
import qualified Concordium.TransactionVerification as TVer

import Concordium.GlobalState.Instance
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState.Instances
import Concordium.GlobalState.Basic.BlockState
import Concordium.Wasm
import qualified Concordium.Cost as Cost
import qualified Concordium.GlobalState.ContractStateV1 as StateV1

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.TestUtils


initialBlockState :: BlockState PV4
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

counterSourceFile :: FilePath
counterSourceFile = "./testdata/contracts/v1/call-counter.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

testCases :: [TestCase PV4]
testCases =
  [ TestCase
    { tcName = "Counter updates and returns."
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion counterSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary deploymentCostCheck, emptySpec)
        )
      , ( TJSON { payload = InitContract 0 wasmModVersion counterSourceFile "init_counter" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary initializationCostCheck, counterSpec 0)
        )
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "counter.inc" BSS.empty
                , metadata = makeDummyHeader alesAccount 3 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess , counterSpec 1)
        )
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "counter.inc" BSS.empty
                , metadata = makeDummyHeader alesAccount 4 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess , counterSpec 2)
        )
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "counter.inc10" callArgs
                , metadata = makeDummyHeader alesAccount 5 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess , counterSpec 12)
        )
      ]
     }
  ]

  where
        callArgs = BSS.toShort $ runPut $ do
          putWord64le 0 -- contract index
          putWord64le 0 -- contract subindex
          putWord16le 0 -- length of parameter
          putWord16le (fromIntegral (BSS.length "inc"))
          putByteString "inc" -- entrypoint name
          putWord64le 0 -- amount
        deploymentCostCheck :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
        deploymentCostCheck _ Types.TransactionSummary{..} = do
          checkSuccess "Module deployment failed: " tsResult
          moduleSource <- BS.readFile counterSourceFile
          let len = fromIntegral $ BS.length moduleSource
              -- size of the module deploy payload
              payloadSize = Types.payloadSize (Types.encodePayload (Types.DeployModule (WasmModuleV0 (WasmModuleV ModuleSource{..}))))
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
          moduleSource <- BS.readFile counterSourceFile
          let modLen = fromIntegral $ BS.length moduleSource
              modRef = Types.ModuleRef (Hash.hash moduleSource)
              payloadSize = Types.payloadSize (Types.encodePayload (Types.InitContract 0 modRef (InitName "init_counter") (Parameter "")))
              -- size of the transaction minus the signatures.
              txSize = Types.transactionHeaderSize + fromIntegral payloadSize
              -- transaction is signed with 1 signature
              baseTxCost = Cost.baseCost txSize 1
              -- lower bound on the cost of the transaction, assuming no interpreter energy
              -- we know the size of the state should be 8 bytes
              costLowerBound = baseTxCost + Cost.initializeContractInstanceCost 0 modLen (Just 8)
          return ()
          unless (tsEnergyCost >= costLowerBound) $
            assertFailure $ "Actual initialization cost " ++ show tsEnergyCost ++ " not more than lower bound " ++ show costLowerBound

        -- ensure the transaction is successful
        ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
        ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult

        checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
        checkSuccess _ _ = return ()

        counterSpec n bs = specify "Contract state" $
          case getInstance (Types.ContractAddress 0 0) (bs ^. blockInstances) of
            Nothing -> assertFailure "Instance at <0,0> does not exist."
            Just istance -> do
              case istance of
                InstanceV0 _ -> assertFailure "Expecte V1 instance since a V1 module is deployed, but V0 encountered."
                InstanceV1 InstanceV{_instanceVModel=InstanceStateV1 s} -> do
                  -- the contract stores the state at key = [0u8; 8]
                  StateV1.lookupKey s (runPut (putWord64le 0)) >>= \case
                    Nothing -> assertFailure "Failed to find key [0,0,0,0,0,0,0,0]"
                    Just stateContents -> assertEqual ("State contains " ++ show n ++ ".") (runPut (putWord64le n)) stateContents

tests :: Spec
tests = describe "V1: Counter counts." $
  mkSpecs testCases
