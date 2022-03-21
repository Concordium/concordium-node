{-# LANGUAGE OverloadedStrings #-}
{-| This file contains three test cases.
    The details are outlined in 'checkpointing.wat'

    Most checks are being carried out in the smart contracts.
    However some checks are also being carried out here in this file.
    The latter checks are geared towards outcome checking while the
    former checks are checking that host functions behaves as expected and that
    the integrity of the smart contracts during rollback are upheld.
-}
module SchedulerTests.SmartContracts.V1.Checkpointing (tests) where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Control.Monad
import Data.Serialize(runPut, putWord64le, putByteString, putWord16le, encode)
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

initialBlockState :: BlockState PV4
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 0) Acc.emptyAccounts)

checkpointingSourceFile :: FilePath
checkpointingSourceFile = "./testdata/contracts/v1/checkpointing.wasm"

v0ProxySourceFile :: FilePath
v0ProxySourceFile = "./testdata/contracts/v1/send-message-v1.wasm"

wasmModVersion0 :: WasmVersion
wasmModVersion0 = V0

wasmModVersion1 :: WasmVersion
wasmModVersion1 = V1

-- | This test has the following call pattern:
-- A 
--   -->  B
--          --> A 
--          <-- 
--        B(trap)
-- A <--
--
-- The state at A should be left unchanged by the changes of the 'inner' invocation on contract A.
-- A correctly perceives B's trapping signal.
-- Only V1 contracts are being used.
testCase1 :: [TestCase PV4]
testCase1 =
  [ TestCase
    { tcName = "Checkpointing 1"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion1 checkpointingSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck checkpointingSourceFile), emptySpec)
        )       
      , ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_a" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_a"), emptySpec)
        )
      ,
        ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_b" ""
                , metadata = makeDummyHeader alesAccount 3 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_b"), emptySpec)
        )
      ,
        ( TJSON { payload = Update 10000000 (Types.ContractAddress 0 0) "a.a_modify_proxy" parameters
                , metadata = makeDummyHeader alesAccount 4 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess, emptySpec)
        )
      ]
    }
  ]
  where
    parameters = BSS.toShort $ runPut $ do
          putWord64le 1 -- contract index of contract B
          putWord64le 0 -- contract subindex
          putWord16le (fromIntegral (BS.length forwardParameter)) -- length of parameter
          putByteString forwardParameter
          putWord16le (fromIntegral (BSS.length "b_forward_crash")) -- contract b's receive function.
          putByteString "b_forward_crash" -- entrypoint name
          putWord64le 0 -- amount
    forwardParameter = runPut $ do
          putWord64le 0 -- index of contract A
          putWord64le 0 -- subindex of the counter contract
          putWord16le 0 -- length of the empty parameter
          putWord16le (fromIntegral (BSS.length "a_modify"))
          putByteString "a_modify" -- entrypoint name
          putWord64le 0 -- amount
    -- ensure the test case is successful
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess msg Types.TxSuccess{..} = if length vrEvents == 3
      then return ()
      else assertFailure $ msg ++ " unexepcted no. of events " ++ show (length vrEvents) ++ " expected 3."

-- | This test has the following call pattern:
-- A 
--   -->  B
--          --> A (no modification, but bump iterator)
--          <-- 
--        B
-- A <--
--
-- The state at A should be left unchanged.
-- The iterator initialized at A should point to the same entry at the point of initialization.
-- Only V1 contracts are being used.
testCase2 :: [TestCase PV4]
testCase2 =
  [ TestCase
    { tcName = "Checkpointing 2"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [
        ( TJSON { payload = DeployModule wasmModVersion1 checkpointingSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck checkpointingSourceFile), emptySpec)
        )       
      , ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_a" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_a"), emptySpec)
        )
      ,
        ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_b" ""
                , metadata = makeDummyHeader alesAccount 3 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_b"), emptySpec)
        )
      ,
        (TJSON { payload = Update 0 (Types.ContractAddress 0 0) "a.a_modify_proxy" parameters
                , metadata = makeDummyHeader alesAccount 4 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess, emptySpec)
        )
      ]
    }
  ]
  where
    parameters = BSS.toShort $ runPut $ do
          putWord64le 1 -- contract index of contract B
          putWord64le 0 -- contract subindex
          putWord16le (fromIntegral (BS.length forwardParameter)) -- length of parameter
          putByteString forwardParameter
          putWord16le (fromIntegral (BSS.length "b_forward")) -- contract b's receive function.
          putByteString "b_forward" -- entrypoint name
          putWord64le 0 -- amount
    forwardParameter = runPut $ do
          putWord64le 0 -- index of contract A
          putWord64le 0 -- subindex of the counter contract
          putWord16le 0 -- length of the empty parameter
          putWord16le (fromIntegral (BSS.length "a_no_modify"))
          putByteString "a_no_modify" -- entrypoint name
          putWord64le 0 -- amount
    -- ensure the test case is successful
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess msg Types.TxSuccess{..} = if length vrEvents == 6
      then return ()
      else assertFailure $ msg ++  " unexepcted no. of events " ++ show (length vrEvents) ++ " expected 6."

-- | This test has the following call pattern:
-- A 
--   -->  Transfer 
-- A <--
--
-- The state at A should be left unchanged.
-- The iterator initialized at A should point to the same entry at the point of initialization.
-- Only V1 contracts are being used.
testCase3 :: [TestCase PV4]
testCase3 =
  [ TestCase
    { tcName = "Checkpointing 3"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [
        ( TJSON { payload = DeployModule wasmModVersion1 checkpointingSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck checkpointingSourceFile), emptySpec)
        )       
      , ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_a" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_a"), emptySpec)
        )
      ,
        ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_b" ""
                , metadata = makeDummyHeader alesAccount 3 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_b"), emptySpec)
        )
      ,
        (TJSON { payload = Update 1234 (Types.ContractAddress 0 0) "a.a_modify_proxy" (BSS.toShort (encode thomasAccount))
                , metadata = makeDummyHeader alesAccount 4 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess, emptySpec)
        )
      ]
    }
  ]
  where
    -- ensure the test case is successful
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess msg Types.TxSuccess{..} = if length vrEvents == 4
      then return ()
      else assertFailure $ msg ++  " unexepcted no. of events " ++ show (length vrEvents) ++ " expected 4."

-- | This test has the following call pattern:
-- A 
--   -->  B
--          --> A modify
--          <-- 
--        B
-- A <--
--
-- The state at A should have changed according to the 'inner' invocation on contract A.
-- Only V1 contracts are being used.
testCase4 :: [TestCase PV4]
testCase4 =
  [ TestCase
    { tcName = "Checkpointing 4"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion1 checkpointingSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck checkpointingSourceFile), emptySpec)
        )       
      , ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_a" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_a"), emptySpec)
        )
      ,
        ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_b" ""
                , metadata = makeDummyHeader alesAccount 3 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_b"), emptySpec)
        )
      ,
        ( TJSON { payload = Update 4242 (Types.ContractAddress 0 0) "a.a_modify_proxy" parameters
                , metadata = makeDummyHeader alesAccount 4 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess, emptySpec)
        )
      ]
    }
  ]
  where
    parameters = BSS.toShort $ runPut $ do
          putWord64le 1 -- contract index of contract B
          putWord64le 0 -- contract subindex
          putWord16le (fromIntegral (BS.length forwardParameter)) -- length of parameter
          putByteString forwardParameter
          putWord16le (fromIntegral (BSS.length "b_forward")) -- contract b's receive function.
          putByteString "b_forward" -- entrypoint name
          putWord64le 0 -- amount
    forwardParameter = runPut $ do
          putWord64le 0 -- index of contract A
          putWord64le 0 -- subindex of the counter contract
          putWord16le 0 -- length of the empty parameter
          putWord16le (fromIntegral (BSS.length "a_modify"))
          putByteString "a_modify" -- entrypoint name
          putWord64le 0 -- amount
    -- ensure the test case is successful
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess msg Types.TxSuccess{..} = if length vrEvents == 7
      then return ()
      else assertFailure $ msg ++ " unexepcted no. of events " ++ show (length vrEvents) ++ " expected 3."

-- | This test has the following call pattern:
-- A 
--      -->  V0Proxy
--                --> B
--                      --> A 
--                      <--
--                    B(trap)
--                <--
--          
-- A    <--
--
-- The state at A should have changed according to the 'inner' invocation on contract A.
-- A mix of V0 and V1 contracts are used.
testCase5 :: [TestCase PV4]
testCase5 =
  [ TestCase
    { tcName = "Cross Checkpointing 1"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion1 checkpointingSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck checkpointingSourceFile), emptySpec)
        )
      ,
        ( TJSON { payload = DeployModule wasmModVersion0 v0ProxySourceFile
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck v0ProxySourceFile), emptySpec)
        )       
      , ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_a" ""
                , metadata = makeDummyHeader alesAccount 3 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_a"), emptySpec)
        )
      ,
        ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_b" ""
                , metadata = makeDummyHeader alesAccount 4 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_b"), emptySpec)
        )
      ,
        ( TJSON { payload = InitContract 0 wasmModVersion0 v0ProxySourceFile "init_proxy" ""
                , metadata = makeDummyHeader alesAccount 5 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck v0ProxySourceFile "init_proxy"), emptySpec)
        )
      ,
        ( TJSON { payload = Update 10000001 (Types.ContractAddress 0 0) "a.a_modify_proxy" parameters
                , metadata = makeDummyHeader alesAccount 6 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess, emptySpec)
        )
      ]
    }
  ]
  where
    parameters = BSS.toShort $ runPut $ do
          putWord64le 2 -- contract index of proxy contract
          putWord64le 0 -- contract subindex
          putWord16le (fromIntegral (BS.length forwardParameter0)) -- length of parameter
          putByteString forwardParameter0
          putWord16le (fromIntegral (BSS.length "forward")) -- contract b's receive function.
          putByteString "forward" -- entrypoint name
          putWord64le 0 -- amount
    -- This is invoked by a v0 contract so the parameter and receive adddress + method are swapped.
    -- Also V0 contracts invoke others by their fully qualified name i.e. contract-name.receive-name
    forwardParameter0 = runPut $ do
          putWord64le 1 -- contract index of contract B
          putWord64le 0 -- contract subindex
          putWord16le (fromIntegral (BSS.length "b.b_forward_crash")) -- contract b's receive function. 
          putByteString "b.b_forward_crash" -- entrypoint name
          putWord16le (fromIntegral (BS.length forwardParameter1)) -- length of parameter
          putByteString forwardParameter1
          putWord64le 0 -- amount
    forwardParameter1 = runPut $ do
          putWord64le 0 -- index of contract A
          putWord64le 0 -- subindex of the counter contract
          putWord16le 0 -- length of the empty parameter
          putWord16le (fromIntegral (BSS.length "a_modify"))
          putByteString "a_modify" -- entrypoint name
          putWord64le 0 -- amount
    -- ensure the test case is successful
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess msg Types.TxSuccess{..} = if length vrEvents == 3
      then return ()
      else assertFailure $ msg ++ " unexepcted no. of events " ++ show (length vrEvents) ++ " expected 3."


-- | This test has the following call pattern:
-- A 
--      -->  V0Proxy
--                --> B
--                      --> A 
--                      <--
--                    B
--                <--
--           
-- A    <--
--
-- The state at A should have changed according to the 'inner' invocation on contract A.
-- A mix of V0 and V1 contracts are used.
testCase6 :: [TestCase PV4]
testCase6 =
  [ TestCase
    { tcName = "Cross Checkpointing 2"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule wasmModVersion1 checkpointingSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck checkpointingSourceFile), emptySpec)
        )
      ,
        ( TJSON { payload = DeployModule wasmModVersion0 v0ProxySourceFile
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (deploymentCostCheck v0ProxySourceFile), emptySpec)
        )       
      , ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_a" ""
                , metadata = makeDummyHeader alesAccount 3 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_a"), emptySpec)
        )
      ,
        ( TJSON { payload = InitContract 0 wasmModVersion1 checkpointingSourceFile "init_b" ""
                , metadata = makeDummyHeader alesAccount 4 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck checkpointingSourceFile "init_b"), emptySpec)
        )
      ,
        ( TJSON { payload = InitContract 0 wasmModVersion0 v0ProxySourceFile "init_proxy" ""
                , metadata = makeDummyHeader alesAccount 5 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary (initializationCostCheck v0ProxySourceFile "init_proxy"), emptySpec)
        )
      ,
        ( TJSON { payload = Update 4242 (Types.ContractAddress 0 0) "a.a_modify_proxy" parameters
                , metadata = makeDummyHeader alesAccount 6 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureSuccess, emptySpec)
        )
      ]
    }
  ]
  where
    parameters = BSS.toShort $ runPut $ do
          putWord64le 2 -- contract index of proxy contract
          putWord64le 0 -- contract subindex
          putWord16le (fromIntegral (BS.length forwardParameter0)) -- length of parameter
          putByteString forwardParameter0
          putWord16le (fromIntegral (BSS.length "forward")) -- contract b's receive function.
          putByteString "forward" -- entrypoint name
          putWord64le 0 -- amount
    -- This is invoked by a v0 contract so the parameter and receive adddress + method are swapped.
    -- Also V0 contracts invoke others by their fully qualified name i.e. contract-name.receive-name
    forwardParameter0 = runPut $ do
          putWord64le 1 -- contract index of contract B
          putWord64le 0 -- contract subindex
          putWord16le (fromIntegral (BSS.length "b.b_forward")) -- contract b's receive function. 
          putByteString "b.b_forward" -- entrypoint name
          putWord16le (fromIntegral (BS.length forwardParameter1)) -- length of parameter
          putByteString forwardParameter1
          putWord64le 0 -- amount
    forwardParameter1 = runPut $ do
          putWord64le 0 -- index of contract A
          putWord64le 0 -- subindex of the counter contract
          putWord16le 0 -- length of the empty parameter
          putWord16le (fromIntegral (BSS.length "a_modify"))
          putByteString "a_modify" -- entrypoint name
          putWord64le 0 -- amount
    -- ensure the test case is successful
    ensureSuccess :: TVer.BlockItemWithStatus -> Types.TransactionSummary -> Expectation
    ensureSuccess _ Types.TransactionSummary{..} = checkSuccess "Update failed" tsResult
    checkSuccess msg Types.TxReject{..} = assertFailure $ msg ++ show vrRejectReason
    checkSuccess msg Types.TxSuccess{..} = if length vrEvents == 8
      then return ()
      else assertFailure $ msg ++ " unexepcted no. of events " ++ show (length vrEvents) ++ " expected 8."

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

-- todo add the other tests back
tests :: Spec
tests = describe "V1: Checkpointing." $
  mkSpecs $ testCase1 ++ testCase2 ++ testCase3 ++ testCase4 ++ testCase5 ++ testCase6
