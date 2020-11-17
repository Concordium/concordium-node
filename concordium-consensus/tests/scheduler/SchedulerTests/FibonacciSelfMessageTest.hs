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
import Data.Serialize(runPut, putWord64le)
import Data.Word
import Lens.Micro.Platform

import qualified Concordium.Scheduler.Types as Types
import Concordium.Scheduler.Runner

import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState.Instances
import Concordium.GlobalState.Basic.BlockState
import Concordium.Wasm

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.TestUtils


initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    100000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000000) Acc.emptyAccounts)

fibParamBytes :: Word64 -> BSS.ShortByteString
fibParamBytes n = BSS.toShort $ runPut (putWord64le n)

testCases :: [TestCase]
testCases =
  [ -- NOTE: Could also check resulting balances on each affected account or contract, but
    -- the block state invariant at least tests that the total amount is preserved.
    TestCase
    { tcName = "Error handling in contracts."
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule 0 "./testdata/contracts/fib.wasm"
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0, alesKP)]
                }
        , (Success emptyExpect, emptySpec)
        )
      , ( TJSON { payload = InitContract 0 0 "./testdata/contracts/fib.wasm" "init_fib" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0, alesKP)]
                }
        , (Success emptyExpect, emptySpec)
        )
        -- compute F(10)
      , ( TJSON { payload = Update 0 (Types.ContractAddress 0 0) "fib.receive" (fibParamBytes 10)
                , metadata = makeDummyHeader alesAccount 3 70000
                , keys = [(0, alesKP)]
                }
        , (Success ensureAllUpdates , fibSpec 10)
        )
      ]
     }
  ]

  where ensureAllUpdates = mapM_ p
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

tests :: Spec
tests = describe "Self-referential Fibonacci." $
  mkSpecs testCases
