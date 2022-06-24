{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-| This module tests making a transfer from a contract to an account.
-}
module SchedulerTests.SmartContracts.V1.Transfer (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Data.Serialize(encode, runPut, putWord64le)
import Lens.Micro.Platform
import Control.Monad

import Concordium.Types.HashableTo (getHash)
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

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.TestUtils


initialBlockState :: BlockState PV4
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

transferSourceFile :: FilePath
transferSourceFile = "./testdata/contracts/v1/transfer.wasm"

-- Tests in this module use version 1, creating V1 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V1

testCases :: [TestCase PV4]
testCases =
  [ TestCase
    { tcName = "Transfer from V1 contract to account."
    , tcParameters = (defaultParams @PV4) {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( (TJSON { payload = DeployModule wasmModVersion transferSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                },
            okVerRes 1)
        , (SuccessWithSummary deploymentCostCheck, emptySpec)
        )
      , ( (TJSON { payload = InitContract 0 wasmModVersion transferSourceFile "init_transfer" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                },
            okVerRes 2)
        , (SuccessWithSummary initializationCostCheck, transferSpec)
        )
      , ( (TJSON { payload = Update 123 (Types.ContractAddress 0 0) "transfer.forward" (BSS.toShort (encode alesAccount))
                , metadata = makeDummyHeader alesAccount 3 700000
                , keys = [(0,[(0, alesKP)])]
                },
            okVerRes 3)
        , (SuccessWithSummary ensureSuccess , transferSpec)
        )
      , ( (TJSON { payload = Update 1000 (Types.ContractAddress 0 0) "transfer.deposit" ""
                , metadata = makeDummyHeader alesAccount 4 700000
                , keys = [(0,[(0, alesKP)])]
                },
            okVerRes 4)
        , (SuccessWithSummary ensureSuccess , const (return ()))
        )
      , ( (TJSON { payload = Update 0 (Types.ContractAddress 0 0) "transfer.send" sendParameter
                , metadata = makeDummyHeader alesAccount 5 700000
                , keys = [(0,[(0, alesKP)])]
                },
            okVerRes 5)
        , (Success (assertEqual "Transfer events" [
                       Types.Interrupted (Types.ContractAddress 0 0) [],
                       Types.Transferred (Types.AddressContract (Types.ContractAddress 0 0)) 17 (Types.AddressAccount alesAccount),
                       Types.Resumed (Types.ContractAddress 0 0) True,
                       Types.Updated (Types.ContractAddress 0 0) (Types.AddressAccount alesAccount) 0 (Parameter sendParameter) (ReceiveName "transfer.send") V1 []
                       ]) , sendSpec)
        )
      ]
     }
  ]

  where
        sendParameter = BSS.toShort (encode alesAccount <> runPut (putWord64le 17))
        deploymentCostCheck :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
        deploymentCostCheck _ Types.TransactionSummary{..} = do
          checkSuccess "Module deployment failed: " tsResult
          moduleSource <- BS.readFile transferSourceFile
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
          moduleSource <- BS.readFile transferSourceFile
          let modLen = fromIntegral $ BS.length moduleSource
              modRef = Types.ModuleRef (Hash.hash moduleSource)
              payloadSize = Types.payloadSize (Types.encodePayload (Types.InitContract 0 modRef (InitName "init_transfer") (Parameter "")))
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

        -- Check that the contract has the initial amount 0 microCCD on its account.
        transferSpec bs = specify "Contract state" $
          case getInstance (Types.ContractAddress 0 0) (bs ^. blockInstances) of
            Nothing -> assertFailure "Instance at <0,0> does not exist."
            Just istance ->
              assertEqual ("Contract has 0 CCD.") (Types.Amount 0) (instanceAmount istance)


        -- Check that the contract has the deposited amount (1000) minus 17 microCCD on its account.
        sendSpec bs = specify "Contract state" $
          case getInstance (Types.ContractAddress 0 0) (bs ^. blockInstances) of
            Nothing -> assertFailure "Instance at <0,0> does not exist."
            Just istance ->
              assertEqual ("Contract has 983 CCD.") (Types.Amount (1000 - 17)) (instanceAmount istance)
        -- Create a `Ok NormalTransactionSuccess` verification result.
        okVerRes nonce =
          let
            account = getAccount PV4 alesAccount (initialBlockState .^ blockAccounts)
            accountInformation = undefined
          in TVer.Ok $ TVer.NormalTransactionSuccess (getHash accountInformation) nonce

tests :: Spec
tests = describe "V1: Transfer from contract to account." $
  mkSpecs testCases
