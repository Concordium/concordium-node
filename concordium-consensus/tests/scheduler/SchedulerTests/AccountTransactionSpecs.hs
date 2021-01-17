{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.AccountTransactionSpecs where

import Test.Hspec
import Test.HUnit

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import Lens.Micro.Platform
import Control.Monad.IO.Class
import Concordium.Types

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData

import SchedulerTests.Helpers

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialAmount :: Types.Amount
initialAmount = 0

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount initialAmount Acc.emptyAccounts

transactionsInput :: [Types.CredentialDeploymentWithMeta]
transactionsInput = map (Types.fromCDI 0) $ [
  cdi1,
  cdi2,
  cdi3,
  cdi4, -- should fail because repeated credential ID
  cdi5,
  -- cdi6, -- deploy just a new predicate
  cdi7  -- should run out of gas (see initial amount on the sender account)
  ]

testAccountCreation ::
    IO
    ([(Types.BlockItem, Types.ValidResult)],
     [(Types.CredentialDeploymentWithMeta, Types.FailureKind)],
     [Maybe Account],
     Account,
     Amount)
testAccountCreation = do
    let transactions = Types.TGCredentialDeployment <$> transactionsInput
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
            dummyChainMeta
            maxBound
            maxBound
            initialBlockState
    let state = finState ^. Types.ssBlockState
    let accounts = state ^. blockAccounts
    let accAddrs = map accountAddressFromCred [cdi1,cdi2,cdi3,cdi5,cdi7]
    case invariantBlockState state (finState ^. Types.schedulerExecutionCosts) of
        Left f -> liftIO $ assertFailure $ f ++ "\n" ++ show state
        _ -> return ()
    return (getResults ftAdded, ftFailedCredentials,
            map (\addr -> accounts ^? ix addr) accAddrs, accounts ^. singular (ix alesAccount),
            finState ^. Types.schedulerExecutionCosts)

checkAccountCreationResult ::
  ([(Types.BlockItem, Types.ValidResult)],
     [(Types.CredentialDeploymentWithMeta, Types.FailureKind)],
     [Maybe Account],
     Account,
     Amount)
  -> Assertion
checkAccountCreationResult (suc, fails, stateAccs, stateAles, executionCost) = do
  assertEqual "All but the 4th transaction should fail." 1 (length fails)
  assertEqual "Account should keep the initial amount." initialAmount (stateAles ^. accountAmount)
  assertEqual "Execution cost should be 0." 0 executionCost
  assertEqual "Total amount of tokens is maintained." initialAmount (stateAles ^. accountAmount + executionCost)

  -- FIXME: Make these more fine-grained so that failures are understandable.
  assertBool "Successful transaction results." txsuc
  assertBool "Newly created accounts." txstateAccs

  where txsuc = case suc of
          [(_, a11), (_, a12),(_, a13),(_, a15),(_, a17)] |
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a11,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a12,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a13,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a15,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a17 -> True
          _ -> False
        txstateAccs = case stateAccs of
                        [Just _, Just _, Just _, Just _, Just _] -> True
                        _ -> False

tests :: Spec
tests =
  describe "Account creation" $
    specify "4 accounts created, fifth rejected, credential deployed, and one more account created." $
      testAccountCreation >>= checkAccountCreationResult
