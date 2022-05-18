{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.InitialAccountCreationSpec where

import Test.Hspec
import Test.HUnit

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch
import Concordium.TransactionVerification

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
import SchedulerTests.TestUtils

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialAmount :: Types.Amount
initialAmount = 0

initialBlockState :: BlockState PV1
initialBlockState = blockStateWithAlesAccount initialAmount Acc.emptyAccounts

-- We just put a dummy verification result such that it will be verified by the scheduler.
transactionsInput :: [CredentialDeploymentWithStatus]
transactionsInput = map ((\x -> (x, NotOk Expired)) . Types.addMetadata Types.CredentialDeployment 0) [
  icdi1,
  icdi2,
  icdi3 -- should fail because reuse of prf key
  ]

testAccountCreation ::
    IO
    ([(BlockItemWithStatus, Types.ValidResult)],
     [(CredentialDeploymentWithStatus, Types.FailureKind)],
     [Maybe (Account (AccountVersionFor PV1))],
     Amount)
testAccountCreation = do
    let transactions = Types.TGCredentialDeployment <$> transactionsInput
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize dummyBlockTimeout transactions)
            dummyChainMeta
            maxBound
            maxBound
            initialBlockState
    let state = finState ^. Types.ssBlockState
    let accounts = state ^. blockAccounts
    let accAddrs = map (accountAddressFromCredential . Types.credential) [icdi1,icdi2,icdi4] -- cdi3 has the same address as cdi2
    case invariantBlockState state (finState ^. Types.schedulerExecutionCosts) of
        Left f -> liftIO $ assertFailure $ f ++ "\n" ++ show state
        _ -> return ()
    return (getResults ftAdded, ftFailedCredentials,
            map (\addr -> accounts ^? ix addr) accAddrs,
            finState ^. Types.schedulerExecutionCosts)

checkAccountCreationResult ::
  ([(BlockItemWithStatus, Types.ValidResult)],
     [(CredentialDeploymentWithStatus, Types.FailureKind)],
     [Maybe (Account (AccountVersionFor PV1))],
     Amount)
  -> Assertion
checkAccountCreationResult (suc, fails, stateAccs, executionCost) = do
  assertEqual "The third transaction should fail." 1 (length fails)
  assertEqual "Execution cost should be 0." 0 executionCost

  -- FIXME: Make these more fine-grained so that failures are understandable.
  assertBool "Successful transaction results." txsuc
  let addr1 = accountAddressFromCredential . Types.credential $ icdi1
  let addr2 = accountAddressFromCredential . Types.credential $ icdi2
  assertEqual "Accounts created" [Just addr1, Just addr2, Nothing] (map (fmap (^. accountAddress)) stateAccs)


  where txsuc = case suc of
          [(_, a11), (_, a12)] |
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a11,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a12 -> True
          _ -> False

tests :: Spec
tests =
  describe "Account creation" $
    specify "2 accounts created, next two rejected." $
      testAccountCreation >>= checkAccountCreationResult
