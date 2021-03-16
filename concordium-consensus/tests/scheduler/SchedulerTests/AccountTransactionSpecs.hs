{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.AccountTransactionSpecs where

import Test.Hspec
import Test.HUnit

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.ID.Types as Types
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

initialBlockState :: BlockState PV
initialBlockState = blockStateWithAlesAccount initialAmount Acc.emptyAccounts

-- cdi7, but with lowest possible expiry
cdi7' :: Types.AccountCreation
cdi7' = Types.AccountCreation{
  messageExpiry = 0,
  credential = Types.credential cdi7
  }

transactionsInput :: [Types.CredentialDeploymentWithMeta]
transactionsInput = map (Types.addMetadata Types.CredentialDeployment 0) $ [
  cdi1,
  cdi2,
  cdi3,
  cdi4, -- should fail because repeated credential ID
  cdi5,
  -- cdi6, -- deploy just a new predicate
  cdi7,
  cdi7'
  ]

testAccountCreation ::
    IO
    ([(Types.BlockItem, Types.ValidResult)],
     [(Types.CredentialDeploymentWithMeta, Types.FailureKind)],
     [Maybe (Account PV)],
     Account PV,
     Amount)
testAccountCreation = do
    let transactions = Types.TGCredentialDeployment <$> transactionsInput
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
            dummyChainMeta { slotTime = 250 } -- we set slot time to non-zero so that the deployment of the last credential fails due to message expiry
            maxBound
            maxBound
            initialBlockState
    let state = finState ^. Types.ssBlockState
    let accounts = state ^. blockAccounts
    let accAddrs = map (accountAddressFromCredential . Types.credential) [cdi1,cdi2,cdi3,cdi5,cdi7]
    case invariantBlockState state (finState ^. Types.schedulerExecutionCosts) of
        Left f -> liftIO $ assertFailure $ f ++ "\n" ++ show state
        _ -> return ()
    return (getResults ftAdded, ftFailedCredentials,
            map (\addr -> accounts ^? ix addr) accAddrs, accounts ^. singular (ix alesAccount),
            finState ^. Types.schedulerExecutionCosts)

checkAccountCreationResult ::
  ([(Types.BlockItem, Types.ValidResult)],
     [(Types.CredentialDeploymentWithMeta, Types.FailureKind)],
     [Maybe (Account PV)],
     Account PV,
     Amount)
  -> Assertion
checkAccountCreationResult (suc, fails, stateAccs, stateAles, executionCost) = do
  -- this is in reverse order since failed transactions are returned in reverse order of the order tried.
  assertEqual "Fourth and eighth credential deployment should fail." [Types.ExpiredTransaction, (Types.DuplicateAccountRegistrationID (Types.credId . Types.credential $ cdi3))] (map snd fails)
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
