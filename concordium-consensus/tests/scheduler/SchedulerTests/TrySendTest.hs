{-# LANGUAGE OverloadedStrings #-}
{-| This tests a minimal example of error handling in returns of smart contracts.
  We do two invocations of the receive method, one with a valid account to send
  to, and one with invalid.

  In the first case we expect a transfer to happen, in the second case we expect no events
  since the transfer did not happen. However we still expect the transaction to succeed.
-}
module SchedulerTests.TrySendTest where

import Test.Hspec
import qualified Data.ByteString.Short as BSS
import Data.Serialize(encode)

import qualified Concordium.Scheduler.Types as Types
import Concordium.Scheduler.Runner

import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
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

toAddr :: BSS.ShortByteString
toAddr = BSS.toShort (encode alesAccount)

testCases :: [TestCase]
testCases =
  [ -- NOTE: Could also check resulting balances on each affected account or contract, but
    -- the block state invariant at least tests that the total amount is preserved.
    TestCase
    { tcName = "Error handling in contracts."
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcTransactions =
      [ ( TJSON { payload = DeployModule 0 "./testdata/contracts/try-send-test.wasm"
                , metadata = makeDummyHeader alesAccount 1 100000
                , keys = [(0, alesKP)]
                }
        , (Success emptyExpect, emptySpec)
        )
      , ( TJSON { payload = InitContract 0 0 "./testdata/contracts/try-send-test.wasm" "init_try" ""
                , metadata = makeDummyHeader alesAccount 2 100000
                , keys = [(0, alesKP)]
                }
        , (Success emptyExpect, emptySpec)
        )
        -- valid account, should succeed in transferring
      , ( TJSON { payload = Update 11 (Types.ContractAddress 0 0) "try.receive" toAddr
                , metadata = makeDummyHeader alesAccount 3 70000
                , keys = [(0, alesKP)]
                }
        , (SuccessE [Types.Updated { euAddress = Types.ContractAddress 0 0
                                   , euInstigator = Types.AddressAccount alesAccount
                                   , euAmount = 11
                                   , euMessage = Parameter toAddr
                                   , euReceiveName = ReceiveName "try.receive"
                                   , euEvents = []
                                   },
                    Types.Transferred {
                        etFrom = Types.AddressContract (Types.ContractAddress 0 0),
                        etAmount = 11,
                        etTo = Types.AddressAccount alesAccount
                    }] , emptySpec)
        )
        -- transfer did not happen
      , ( TJSON { payload = Update 11 (Types.ContractAddress 0 0) "try.receive" (BSS.pack (replicate 32 0) )
                , metadata = makeDummyHeader alesAccount 4 70000
                , keys = [(0, alesKP)]
                }
        , (SuccessE [Types.Updated { euAddress = Types.ContractAddress 0 0
                                   , euInstigator = Types.AddressAccount alesAccount
                                   , euAmount = 11
                                   , euMessage = Parameter (BSS.pack (replicate 32 0))
                                   , euReceiveName = ReceiveName "try.receive"
                                   , euEvents = []
                                   }], emptySpec)
        )
      ]
     }
  ]

tests :: Spec
tests = describe "SimpleTransfer from contract to account." $
  mkSpecs testCases
