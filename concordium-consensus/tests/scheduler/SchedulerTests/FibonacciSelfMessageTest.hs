{-# LANGUAGE OverloadedStrings #-}
{-| This tests sending messages to contracts. Concretely it invokes
    A fibonacci contract that adds the n-th Fibonacci number to its state,
    by repeatedly sending itself messages.

    See ../smart-contracts/rust-contracts/example-contracts/fib for the source code.
-}
module SchedulerTests.FibonacciSelfMessageTest where

import Test.Hspec
import Test.HUnit(assertFailure, assertEqual)

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Data.Serialize(runPut, putWord64le)
import Data.Word
import Lens.Micro.Platform
import Control.Monad

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Scheduler.Runner

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


initialBlockState :: BlockState PV1
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

fibParamBytes :: Word64 -> BSS.ShortByteString
fibParamBytes n = BSS.toShort $ runPut (putWord64le n)

fibSourceFile :: FilePath
fibSourceFile = "./testdata/contracts/fib.wasm"

testCases :: [TestCase PV1]
testCases =
  [ -- NOTE: Could also check resulting balances on each affected account or contract, but
    -- the block state invariant at least tests that the total amount is preserved.
    TestCase
    { tcName = "Error handling in contracts."
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule 0 fibSourceFile
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary deploymentCostCheck, emptySpec)
        )
      , ( TJSON { payload = InitContract 0 0 "./testdata/contracts/fib.wasm" "init_fib" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary initializationCostCheck, emptySpec)
        )
        -- compute F(10)
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "fib.receive" (fibParamBytes 10)
                , metadata = makeDummyHeader alesAccount 3 700000
                , keys = [(0,[(0, alesKP)])]
                }
        , (SuccessWithSummary ensureAllUpdates , fibSpec 10)
        )
      ]
     }
  ]

  where
        deploymentCostCheck :: Types.BlockItem -> Types.TransactionSummary -> Expectation
        deploymentCostCheck _ Types.TransactionSummary{..} = do
          moduleSource <- BS.readFile fibSourceFile
          let len = fromIntegral $ BS.length moduleSource
              -- size of the module deploy payload
              payloadSize = Types.payloadSize (Types.encodePayload (Types.DeployModule (WasmModule 0 ModuleSource{..})))
              -- size of the transaction minus the signatures.
              txSize = Types.transactionHeaderSize + fromIntegral payloadSize
              -- transaction is signed with 1 signature
          assertEqual "Deployment has correct cost " (Cost.baseCost txSize 1 + Cost.deployModuleCost len) tsEnergyCost

        -- check that the initialization cost was at least the administrative cost.
        -- It is not practical to check the exact cost because the execution cost of the init function is hard to
        -- have an independent number for, other than executing.
        initializationCostCheck :: Types.BlockItem -> Types.TransactionSummary -> Expectation
        initializationCostCheck _ Types.TransactionSummary{..} = do
          moduleSource <- BS.readFile fibSourceFile
          let modLen = fromIntegral $ BS.length moduleSource
              modRef = Types.ModuleRef (Hash.hash moduleSource)
              payloadSize = Types.payloadSize (Types.encodePayload (Types.InitContract 0 modRef (InitName "init_fib") (Parameter "")))
              -- size of the transaction minus the signatures.
              txSize = Types.transactionHeaderSize + fromIntegral payloadSize
              -- transaction is signed with 1 signature
              baseTxCost = Cost.baseCost txSize 1
              -- lower bound on the cost of the transaction, assuming no interpreter energy
              -- we know the size of the state should be 8 bytes
              costLowerBound = baseTxCost + Cost.initializeContractInstanceCost 0 modLen (Just 8)
          unless (tsEnergyCost >= costLowerBound) $
            assertFailure $ "Actual initialization cost " ++ show tsEnergyCost ++ " not more than lower bound " ++ show costLowerBound

        ensureAllUpdates :: Types.BlockItem -> Types.TransactionSummary -> Expectation
        ensureAllUpdates _ Types.TransactionSummary{..} =
          case tsResult of
            Types.TxSuccess evs -> do
              mapM_ p evs -- check that all updates are the right ones
              -- and check that the cost was adequate (we again only check the lower bound only)
              moduleSource <- BS.readFile fibSourceFile
              let modLen = fromIntegral $ BS.length moduleSource
                  payloadSize = Types.payloadSize (Types.encodePayload (Types.Update 0 (Types.ContractAddress 0 0) (ReceiveName "fib.receive") (Parameter (fibParamBytes 10))))
                  -- size of the transaction minus the signatures.
                  txSize = Types.transactionHeaderSize + fromIntegral payloadSize
                  -- transaction is signed with 1 signature
                  baseTxCost = Cost.baseCost txSize 1
                  -- lower bound on the cost of the transaction, assuming no interpreter energy and the instance is not created.
                  -- the number of invocations of the smart contract is fibOne 10, see below for the definition of fibOne
                  -- the size of the contract state is 8 bytes (one u64)
                  costLowerBound = baseTxCost + (fibOne 10) * Cost.updateContractInstanceCost 0 modLen 8 (Just 8)
              unless (tsEnergyCost >= costLowerBound) $
                assertFailure $ "Actual update cost " ++ show tsEnergyCost ++ " not more than lower bound " ++ show costLowerBound

            Types.TxReject rr -> assertFailure $ "Fibonacci update should succeed, but it failed with " ++ show rr
        p Types.Updated { euAmount = 0
                        , euEvents = []
                        , ..
                        } = case euInstigator of
                              Types.AddressAccount addr -> assertEqual "Only the initial account can be in events." addr alesAccount
                              Types.AddressContract addr -> assertEqual "Contract address is self-address" addr euAddress
        p event = assertFailure ("Unexpected event: " ++ show event)


        -- Check that the contract state contains the n-th Fib number.
        fibSpec n bs = specify "Contract state" $
          case getInstance (Types.ContractAddress 0 0) (bs ^. blockInstances) of
            Nothing -> assertFailure "Instnace at <0,0> does not exist."
            Just istance -> assertEqual "State contains the n-th Fibonacci number." (fibNBytes n) (instanceModel istance)

        fib n = let go = 1:1:zipWith (+) go (tail go)
                in go !! n
        fibNBytes n = ContractState (runPut (putWord64le (fib n)))

        -- the number of invocations of the contract follows https://oeis.org/A001595
        fibOne n = 2 * fib n - 1

tests :: Spec
tests = describe "Self-referential Fibonacci." $
  mkSpecs testCases
