{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.AccountTransactionSpecs where

import Test.Hspec
import Test.HUnit

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import Lens.Micro.Platform
import Control.Monad.IO.Class

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData

import SchedulerTests.Helpers

import qualified Acorn.Core as Core

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialAmount :: Types.Amount
initialAmount = 0

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount initialAmount Acc.emptyAccounts initialAmount

deployAccountCost :: Types.Energy
deployAccountCost = Cost.deployCredential + Cost.checkHeader

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployCredential cdi1
         , metadata = makeDummyHeader alesAccount 1 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi2
         , metadata = makeDummyHeader alesAccount 2 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi3
         , metadata = makeDummyHeader alesAccount 3 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi4 -- should fail because repeated credential ID
         , metadata = makeDummyHeader alesAccount 4 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi5
         , metadata = makeDummyHeader alesAccount 5 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi6 -- deploy just a new predicate
         , metadata = makeDummyHeader alesAccount 6 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi7  -- should run out of gas (see initial amount on the sender account)
         , metadata = makeDummyHeader alesAccount 7 Cost.checkHeader
         , keypair = alesKP
         }
  ]

testAccountCreation ::
  PR.Context Core.UA
    IO
    ([(Types.BlockItem, Types.ValidResult)],
     [(Types.CredentialDeploymentWithMeta, Types.FailureKind)],
     [Maybe Types.Account],
     Types.Account,
     Types.BankStatus)
testAccountCreation = do
    let transactions = Types.emptyGroupedTransactions {
          Types.credentialDeployments = transactionsInput
          }
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
            dummySpecialBetaAccounts
            Types.dummyChainMeta
            maxBound
            initialBlockState
    let state = finState ^. Types.ssBlockState
    let accounts = state ^. blockAccounts
    let accAddrs = map accountAddressFromCred [cdi1,cdi2,cdi3,cdi5,cdi7]
    case invariantBlockState state of
        Left f -> liftIO $ assertFailure $ f ++ "\n" ++ show state
        _ -> return ()
    return (getResults ftAdded, ftFailedCredentials,
            map (\addr -> accounts ^? ix addr) accAddrs, accounts ^. singular (ix alesAccount),
            state ^. blockBank)

checkAccountCreationResult ::
  ([(Types.BlockItem, Types.ValidResult)],
     [(Types.CredentialDeploymentWithMeta, Types.FailureKind)],
     [Maybe Types.Account],
     Types.Account,
     Types.BankStatus)
  -> Bool
checkAccountCreationResult (suc, fails, stateAccs, stateAles, bankState) =
  length fails == 1 && -- all but the 4'th transaction should fail.
  txsuc &&
  txstateAccs &&
  noCost &&
  stateInvariant
  where txsuc = case suc of
          [(_, a11), (_, a12),(_, a13),(_, a15),(_, a16),(_, a17)] |
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a11,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a12,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a13,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a15,
            Types.TxSuccess [Types.CredentialDeployed{}] <- a16,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed{}] <- a17 -> True
          _ -> False
        txstateAccs = case stateAccs of
                        [Just _, Just _, Just _, Just _, Just _] -> True
                        _ -> False
        noCost = stateAles ^. Types.accountAmount == initialAmount && bankState ^. Types.executionCost == 0
        stateInvariant = stateAles ^. Types.accountAmount + bankState ^. Types.executionCost == initialAmount

tests :: Spec
tests =
  describe "Account creation" $
    specify "4 accounts created, fifth rejected, credential deployed, and one more account created." $
      PR.evalContext Init.initialContextData testAccountCreation `shouldReturnP` checkAccountCreationResult
