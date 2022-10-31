{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-| This module tests the relaxed smart contract restrictions introduced in P5.
    The old and new limits are checked, in P4 and P5, respectively.
    The limit changes in P5 are:
      - Parameter size limit: 1kb -> 65kb
      - Return value size limit: 16kb -> no limit (apart from energy) *Only relevant for V1 contracts*.
      - Number of logs: 64 -> no limit (apart from energy)
      - Cost of parameters:
        - Of size <= 1kb: base cost + 1NRG / 1 *kilobyte* (same as before P5)
        - Of size > 1 kb: base cost + 1NRG / 1 *byte*
-}
module SchedulerTests.SmartContracts.V0.RelaxedRestrictions (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Data.Serialize(runPut, putWord32le, putByteString, putWord16le)
import Data.Word (Word16, Word32)
import Control.Monad

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


initialBlockStatePV4 :: BlockState PV4
initialBlockStatePV4 = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

initialBlockStatePV5 :: BlockState PV5
initialBlockStatePV5 = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

sourceFile :: FilePath
sourceFile = "./testdata/contracts/relaxed-restrictions.wasm"

-- Tests in this module use version 0, creating V0 instances.
wasmModVersion :: WasmVersion
wasmModVersion = V0


testCasesPV4 :: [TestCase PV4]
testCasesPV4 =
  [ TestCase
    { tcName = "Correct parameter size limits in PV4."
    , tcParameters = (defaultParams @PV4) {tpInitialBlockState=initialBlockStatePV4}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion sourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary deploymentCostCheck, emptySpec)
        )
      , ( TJSON { payload = InitContract 0 wasmModVersion sourceFile "init_relax" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary initializationCostCheck, emptySpec)
        )
        -- Check that the max size parameter is allowed.
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "relax.param" (callArgsParam 1024 1024)
                , metadata = makeDummyHeader alesAccount 3 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess, emptySpec)
        )
        -- Check that if the top-level parameter is too big, we get a serialization failure.
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "relax.param" (callArgsParam 1024 1025)
                , metadata = makeDummyHeader alesAccount 4 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (Reject Types.SerializationFailure, emptySpec)
        )
        -- Check that if the inter-contract parameter is too big, we get a runtime failure.
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "relax.param" (callArgsParam 1025 1024)
                , metadata = makeDummyHeader alesAccount 5 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (Reject Types.RuntimeFailure, emptySpec)
        )
      ]
     }
  , TestCase
    { tcName = "Correct number of logs limits in PV4."
    , tcParameters = (defaultParams @PV4) {tpInitialBlockState=initialBlockStatePV4}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion sourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary deploymentCostCheck, emptySpec)
        )
      , ( TJSON { payload = InitContract 0 wasmModVersion sourceFile "init_relax" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary initializationCostCheck, emptySpec)
        )
        -- Check that the max number of logs is allowed.
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "relax.logs" (callArgsWord32 64)
                , metadata = makeDummyHeader alesAccount 3 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess , emptySpec)
        )
        -- Check that one above the max number of logs is _not_ allowed.
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "relax.logs" (callArgsWord32 65)
                , metadata = makeDummyHeader alesAccount 4 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (Reject Types.RuntimeFailure, emptySpec)
        )
      ]
     }
  ]

testCasesPV5 :: [TestCase PV5]
testCasesPV5 =
  [ TestCase
    { tcName = "Correct parameter size limits in PV5."
    , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockStatePV5}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion sourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary deploymentCostCheck, emptySpec)
        )
      , ( TJSON { payload = InitContract 0 wasmModVersion sourceFile "init_relax" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary initializationCostCheck, emptySpec)
        )
        -- Check that the max size parameter is allowed. We cannot check above it easily, because it is Word16::MAX.
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "relax.param" (callArgsParam 65535 65535)
                , metadata = makeDummyHeader alesAccount 3 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess , emptySpec)
        )
      ]
     }
  , TestCase
    { tcName = "Correct number of logs limits in PV5."
    , tcParameters = (defaultParams @PV5) {tpInitialBlockState=initialBlockStatePV5}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion sourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary deploymentCostCheck, emptySpec)
        )
      , ( TJSON { payload = InitContract 0 wasmModVersion sourceFile "init_relax" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary initializationCostCheck, emptySpec)
        )
        -- Check that a large number of logs is allowed (more than allowed in P4).
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "relax.logs" (callArgsWord32 64)
                , metadata = makeDummyHeader alesAccount 3 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess , emptySpec)
        )
      ]
     }
  ]


-- |Creates a parameter for "relax.param".
--
-- The first input is the size of the internal parameter to be passed to "param-aux".
-- This is used to test the parameter limit checked inside the wasm interpreter.
--
-- The second input is the desired total length of the bytestring produced by this function.
-- Once the necessary data is written, extra 1s are written until the desired length is reached.
-- This is used to test the parameter limit checked in the scheduler.
callArgsParam
  :: Word16 -- ^Size of the internal parameter to be used by the contract when invoking "param-aux".
  -> Int -- ^The (desired) length of bytestring returned by this function.
  -> BSS.ShortByteString
callArgsParam internalParamSize desiredLen = BSS.toShort $ runPut $ do
  putWord16le internalParamSize
  putWord16le auxNameLen -- entrypoint name len
  putByteString auxName -- entrypoint name
  putByteString (BS.pack $ replicate numBytes 1) -- arbitrary bytes to fill the parameter
  where
    auxName = "relax.param-aux"
    auxNameLen :: Word16
    auxNameLen = fromIntegral $ BS.length auxName
    -- Calculate the number of arbitrary bytes to put in the end, so that the whole parameter gets the desired length.
    numBytes = desiredLen
      - 2 -- internalParamSize
      - 2 -- auxNameLen
      - fromIntegral auxNameLen

-- |Create a Word32 parameter.
callArgsWord32 :: Word32 -> BSS.ShortByteString
callArgsWord32 = BSS.toShort . runPut . putWord32le

deploymentCostCheck :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
deploymentCostCheck _ Types.TransactionSummary{..} = do
  checkSuccess "Module deployment failed: " tsResult
  moduleSource <- BS.readFile sourceFile
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
  moduleSource <- BS.readFile sourceFile
  let modLen = fromIntegral $ BS.length moduleSource
      modRef = Types.ModuleRef (Hash.hash moduleSource)
      payloadSize = Types.payloadSize (Types.encodePayload (Types.InitContract 0 modRef (InitName "init_relax") (Parameter "")))
      -- size of the transaction minus the signatures.
      txSize = Types.transactionHeaderSize + fromIntegral payloadSize
      -- transaction is signed with 1 signature
      baseTxCost = Cost.baseCost txSize 1
      -- lower bound on the cost of the transaction, assuming no interpreter energy
      -- we know that the state is not used, thus the 'Nothing'.
      costLowerBound = baseTxCost + Cost.initializeContractInstanceCost 0 modLen Nothing
  unless (tsEnergyCost >= costLowerBound) $
    assertFailure $ "Actual initialization cost " ++ show tsEnergyCost ++ " not more than lower bound " ++ show costLowerBound

-- ensure the transaction is successful
ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult

checkSuccess :: [Char] -> Types.ValidResult -> IO ()
checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
checkSuccess _ _ = return ()

tests :: Spec
tests = do
  describe "V1: Relax restrictions. Test in PV4." $
    mkSpecs testCasesPV4
  describe "V1: Relax restrictions. Test in PV5." $
    mkSpecs testCasesPV5
