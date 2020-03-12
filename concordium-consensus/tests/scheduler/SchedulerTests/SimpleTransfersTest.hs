{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.SimpleTransfersTest where

import Test.Hspec
import Test.HUnit

import Lens.Micro.Platform
import Control.Monad.IO.Class

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.Cost as Cost

import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants

import qualified Acorn.Core as Core

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    100000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000) Acc.emptyAccounts)
    200000

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = Transfer {toaddress = Types.AddressAccount alesAccount, amount = 100 }
         , metadata = makeDummyHeader alesAccount 1 1000
         , keypair = alesKP
         }
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 88 }
         , metadata = makeDummyHeader alesAccount 2 1000
         , keypair = alesKP
         }
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 98700 }
         , metadata = makeDummyHeader alesAccount 3 1000
         , keypair = alesKP
         }
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount alesAccount, amount = 100 }
         , metadata = makeDummyHeader thomasAccount 1 500
         , keypair = thomasKP
         }
    -- the next transaction should fail because the balance on alesAccount is now 1282, which is
    -- less than 600 + 700
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 600 }
         , metadata = makeDummyHeader alesAccount 4 700
         , keypair = alesKP
         }
  ]


testSimpleTransfer
  :: PR.Context Core.UA
       IO
       ([(Types.BareTransaction, Types.ValidResult)],
        [(Types.BareTransaction, Types.FailureKind)], Types.Amount, Types.Amount)
testSimpleTransfer = do
    transactions <- processUngroupedTransactions transactionsInput
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
            dummySpecialBetaAccounts
            Types.dummyChainMeta
            maxBound
            initialBlockState
    let gstate = finState ^. Types.ssBlockState
    case invariantBlockState gstate of
        Left f -> liftIO $ assertFailure f
        Right _ -> return ()
    return (getResults ftAdded,
            ftFailed,
            gstate ^. blockAccounts . singular (ix alesAccount) . Types.accountAmount,
            gstate ^. blockAccounts . singular (ix thomasAccount) . Types.accountAmount)

checkSimpleTransferResult :: ([(a, Types.ValidResult)], [b], Types.Amount, Types.Amount) -> Bool
checkSimpleTransferResult (suc, fails, alesamount, thomasamount) =
  null fails && -- should be no failed transactions
  reject &&  -- the last transaction is rejected
  nonreject && -- all initial transactions are successful
  alesamount == (100000 - 4 * fromIntegral simpleTransferCost - 88 - 98700 + 100) &&
  thomasamount == (100000 - fromIntegral simpleTransferCost + 88 + 98700 - 100)
  where
    nonreject = all (\case (_, Types.TxSuccess{}) -> True
                           (_, Types.TxReject{}) -> False)
                    (init suc)
    reject = case last suc of
               (_, Types.TxReject (Types.AmountTooLarge _ _)) -> True
               _ -> False

tests :: SpecWith ()
tests =
  describe "Simple transfers test:" $
    specify "3 successful and 1 failed transaction" $
      PR.evalContext Init.initialContextData testSimpleTransfer `shouldReturnP` checkSimpleTransferResult
