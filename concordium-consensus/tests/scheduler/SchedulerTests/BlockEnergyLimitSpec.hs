{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module SchedulerTests.BlockEnergyLimitSpec where

import Test.Hspec
import Test.HUnit

import Control.Monad.IO.Class

import Lens.Micro.Platform

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers
import SchedulerTests.TestUtils

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState PV1
initialBlockState = blockStateWithAlesAccount 200000 Acc.emptyAccounts

-- We will use simple transfer transactions.
usedTransactionEnergy :: Types.Energy
usedTransactionEnergy = simpleTransferCost

maxBlockEnergy :: Types.Energy
maxBlockEnergy = usedTransactionEnergy * 2

transactions :: [TransactionJSON]
transactions = [-- valid transaction: energy not over max limit
                TJSON { payload = Transfer { toaddress = alesAccount, amount = 100 }
                      , metadata = makeDummyHeader alesAccount 1 usedTransactionEnergy
                      , keys = [(0,[(0, alesKP)])]
                      },
                -- invalid transaction: although its used energy amount (plus the energy of the
                -- previously valid transaction) is under the energy limit,
                -- the stated energy is above the limit, so this transaction cannot be added to the block
                TJSON { payload = Transfer { toaddress = alesAccount, amount = 100 }
                      , metadata = makeDummyHeader alesAccount 3 $ maxBlockEnergy + 1
                      , keys = [(0,[(0, alesKP)])]
                      },
                -- will be an unprocessed transaction because together with the first valid transaction,
                -- its energy exceeds the limit
                TJSON { payload = Transfer { toaddress = alesAccount, amount = 100 }
                      , metadata = makeDummyHeader alesAccount 4 $ usedTransactionEnergy + 1
                      , keys = [(0,[(0, alesKP)])]
                      }
               ]

type TestResult = ([(Types.BlockItem, Types.TransactionSummary)],
                   [(Types.Transaction, Types.FailureKind)],
                   [(Types.CredentialDeploymentWithMeta, Types.FailureKind)],
                   [Types.Transaction],
                   [Types.Transaction])

testMaxBlockEnergy :: IO TestResult
testMaxBlockEnergy = do
    ts' <- processUngroupedTransactions transactions
    -- invalid transaction: its used and stated energy of 10000 exceeds the maximum
    -- block energy limit
    let ts = Types.TGCredentialDeployment (Types.addMetadata Types.CredentialDeployment 0 cdi1) : ts' -- dummy arrival time of 0

    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize dummyBlockTimeout ts)
            dummyChainMeta
            maxBlockEnergy
            maxBound
            initialBlockState
    let gstate = finState ^. Types.ssBlockState
    case invariantBlockState gstate (finState ^. Types.schedulerExecutionCosts) of
        Left f -> liftIO $ assertFailure f
        Right _ -> return (ftAdded, ftFailed, ftFailedCredentials, ftUnprocessed, concat (Types.perAccountTransactions ts))

checkResult :: TestResult -> Expectation
checkResult (valid, invalid, invalidCred, unproc, [t1, t3, t4]) =
    validCheck >> invalidCheck >> unprocessedCheck
    where
        validCheck = case valid of
            [(t, Types.TransactionSummary{tsResult = Types.TxSuccess{vrEvents = [Types.Transferred{}]}, tsEnergyCost = energyCost})] -> do
                 assertEqual "The first transaction should be valid:" (Types.normalTransaction t1) t
                 assertEqual "Correct energy cost: " simpleTransferCost energyCost
            _ -> assertFailure "There should be one valid transaction with a TxSuccess result."
        invalidCheck = do
            let (invalidTs, failures) = unzip invalid
            let (invalidCreds, credFailures) = unzip invalidCred
            assertEqual "The second transaction is invalid because it would exceed max block energy:" [t3] invalidTs
            assertEqual "The credential deployment is invalid." [Types.addMetadata Types.CredentialDeployment 0 cdi1] invalidCreds
            assertEqual "There is one normal transaction whose energy exceeds the block energy limit, and one with non-sequential nonce:"
                [Types.ExceedsMaxBlockEnergy] failures
            assertEqual "There is one credential deployment whose energy exceeds the block energy limit:"
                [Types.ExceedsMaxBlockEnergy] credFailures
        unprocessedCheck =
            assertEqual "The last transaction does not fit into the block since the block has reached the energy limit"
                [t4] unproc
checkResult _ = assertFailure "There should be three filtered transactions."


tests :: Spec
tests = do
  describe "Maximum block energy limit test:" $
    specify "One valid, two invalid, one unprocessed transaction" $
        testMaxBlockEnergy >>= checkResult
