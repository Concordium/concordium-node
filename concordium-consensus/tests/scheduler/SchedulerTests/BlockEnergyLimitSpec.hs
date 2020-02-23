{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module SchedulerTests.BlockEnergyLimitSpec where

import Test.Hspec
import Test.HUnit

import Control.Monad.IO.Class

import Acorn.Core
import qualified Acorn.Utils.Init as Init
import qualified Acorn.Parser.Runner as PR
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Cost as Cost 
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 200000 Acc.emptyAccounts 200000

-- We will use simple transfer transactions whose energy cost is equal to checking the header
usedTransactionEnergy :: Types.Energy
usedTransactionEnergy = Cost.checkHeader

maxBlockEnergy :: Types.Energy
maxBlockEnergy = usedTransactionEnergy * 2

transactions :: [TransactionJSON]
transactions = [-- valid transaction: energy not over max limit
                TJSON { payload = Transfer { toaddress = Types.AddressAccount alesAccount, amount = 100 }
                      , metadata = makeDummyHeader alesAccount 1 usedTransactionEnergy
                      , keypair = alesKP
                      },
                -- invalid transaction: its used and stated energy of 10000 exceeds the maximum
                -- block energy limit
                TJSON { payload = DeployCredential cdi1
                      , metadata = makeDummyHeader alesAccount 2 10000
                      , keypair = alesKP
                      },
                -- invalid transaction: although its used energy amount (plus the energy of the
                -- previously valid transaction) is under the energy limit,
                -- the stated energy is above the limit, so this transaction cannot be added to the block
                TJSON { payload = Transfer { toaddress = Types.AddressAccount alesAccount, amount = 100 }
                      , metadata = makeDummyHeader alesAccount 3 $ maxBlockEnergy + Types.Energy 1
                      , keypair = alesKP
                      },
                -- will be an unprocessed transaction because together with the first valid transaction,
                -- its energy exceeds the limit
                TJSON { payload = Transfer { toaddress = Types.AddressAccount alesAccount, amount = 100 }
                      , metadata = makeDummyHeader alesAccount 4 $ usedTransactionEnergy + 1
                      , keypair = alesKP
                      }
               ]

testMaxBlockEnergy ::
    PR.Context UA
       IO
       ([(Types.BareTransaction, Types.ValidResult)],
        [(Types.BareTransaction, Types.FailureKind)],
        [Types.BareTransaction],
        [Types.BareTransaction])
testMaxBlockEnergy = do
    ts <- processUngroupedTransactions transactions
    let ((Sch.FilteredTransactions{..}, _), gstate) =
          Types.runSI (Sch.filterTransactions dummyBlockSize maxBlockEnergy ts)
            dummySpecialBetaAccounts
            Types.dummyChainMeta
            initialBlockState
    case invariantBlockState gstate of
        Left f -> liftIO $ assertFailure f
        Right _ -> return (getResults ftAdded, ftFailed, ftUnprocessed, concat ts)

checkResult :: ([(Types.BareTransaction, Types.ValidResult)],
                [(Types.BareTransaction, Types.FailureKind)],
                [Types.BareTransaction],
                [Types.BareTransaction]) ->
               Expectation
checkResult (valid, invalid, unproc, [t1, t2, t3, t4]) =
    validCheck >> invalidCheck >> unprocessedCheck
    where
        validCheck = case valid of
            [(t, Types.TxSuccess{})] ->
                 assertEqual "The first transaction should be valid:" t1 t
            _ -> assertFailure "There should be one valid transaction with a TxSuccess result."
        invalidCheck = do
            let (invalidTs, failures) = unzip invalid
            assertEqual "The second and third transactions are invalid:" [t3, t2] invalidTs
            assertEqual "There are two transactions whose energy exceeds the block energy limit:"
                (replicate 2 Types.ExceedsMaxBlockEnergy) failures
        unprocessedCheck =
            assertEqual "The last transaction does not fit into the block since the block has reached the energy limit"
                [t4] unproc
checkResult _ = assertFailure "There should be four filtered transactions."


tests :: Spec
tests =
  describe "Maximum block energy limit test:" $
    specify "One valid, two invalid, one unprocessed transaction" $
        PR.evalContext Init.initialContextData testMaxBlockEnergy >>= checkResult
