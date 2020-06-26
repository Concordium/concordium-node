{-# LANGUAGE OverloadedStrings #-}

{- Testing the SimpleTransfer transaction.
NOTE: See also 'SchedulerTests.SimpleTransfersTest'.
-}
module SchedulerTests.SimpleTransferSpec where

import Test.Hspec

import qualified Concordium.Scheduler.Types as Types
import Concordium.Scheduler.Runner

import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import qualified Acorn.Interpreter as I

import SchedulerTests.TestUtils


initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    10000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 10000000) Acc.emptyAccounts)

testCases :: [TestCase]
testCases =
  [ -- NOTE: Could also check resulting balances on each affected account or contract, but
    -- the block state invariant at least tests that the total amount is preserved.
    TestCase
    { tcName = "Transfers from an account to contracts"
    , tcParameters = defaultParams {tpInitialBlockState=initialBlockState}
    , tcModules = ["contracts/SimpleCounter.acorn"]
    , tcTransactions =
      [ ( TJSON { payload = DeployModule "SimpleCounter"
                , metadata = makeDummyHeader alesAccount 1 100000
                , keypair = alesKP
                }
        , (Success emptyExpect, emptySpec)
        )
      , ( TJSON { payload = InitContract { amount = 100
                                         , contractName = "Counter"
                                         , moduleName = "SimpleCounter"
                                         , parameter = "0"
                                         }
                , metadata = makeDummyHeader alesAccount 2 100000
                , keypair = alesKP
                }
        , (Success emptyExpect, emptySpec)
        )
      , ( TJSON { payload = Transfer { toaddress = Types.AddressContract (Types.ContractAddress 0 0)
                                     , amount = 10 }
                , metadata = makeDummyHeader alesAccount 3 70000
                , keypair = alesKP
                }
        , (SuccessE [Types.Updated { euAddress = Types.ContractAddress 0 0
                                   , euInstigator = Types.AddressAccount alesAccount
                                   , euAmount = 10
                                   , euMessage = Types.ExprMessage I.mkNothingE
                                   }] , emptySpec)
        )
      -- SimpleTransfer to non-existing address
      , ( TJSON { payload = Transfer { toaddress = Types.AddressContract (Types.ContractAddress 1 0), amount = 10 }
                , metadata = makeDummyHeader alesAccount 4 70000
                , keypair = alesKP
                }
        , (Reject $ Types.InvalidContractAddress (Types.ContractAddress 1 0), emptySpec)
        )
      ]
     }
  ]


tests :: Spec
tests = describe "SimpleTransfer" $
  mkSpecs testCases
