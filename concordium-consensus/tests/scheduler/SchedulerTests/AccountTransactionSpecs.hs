{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.AccountTransactionSpecs where

import Test.Hspec
import Test.HUnit

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.Cost as Cost

import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import qualified Concordium.GlobalState.Rewards as Rew
import Lens.Micro.Platform
import Control.Monad.IO.Class

import SchedulerTests.DummyData

import qualified Acorn.Core as Core

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialAmount :: Types.Amount
initialAmount = fromIntegral (6 * Cost.deployCredential + 7 * Cost.checkHeader)

initialBlockState :: BlockState
initialBlockState =
  -- NB: We need 6 * deploy account since we still charge the cost even if an
  -- account already exists (case 4 in the tests).
  emptyBlockState emptyBirkParameters dummyCryptographicParameters &
    (blockAccounts .~ Acc.putAccount (mkAccount alesVK initialAmount) Acc.emptyAccounts) .
    (blockBank . Rew.totalGTU .~ initialAmount) .
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.fromModuleList (Init.moduleList gs))) .
    (blockIdentityProviders .~ dummyIdentityProviders)

deployAccountCost :: Types.Energy
deployAccountCost = Cost.deployCredential + Cost.checkHeader

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployCredential cdi1
         , metadata = makeHeader alesKP 1 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi2
         , metadata = makeHeader alesKP 2 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi3
         , metadata = makeHeader alesKP 3 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi4 -- should fail because repeated credential ID
         , metadata = makeHeader alesKP 4 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi5
         , metadata = makeHeader alesKP 5 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi6 -- deploy just a new predicate
         , metadata = makeHeader alesKP 6 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = DeployCredential cdi7  -- should run out of gas (see initial amount on the sender account)
         , metadata = makeHeader alesKP 7 Cost.checkHeader
         , keypair = alesKP
         }
  ]

testAccountCreation ::
  PR.Context Core.UA
    IO
    ([(Types.BareTransaction, Types.ValidResult)],
     [(Types.BareTransaction, Types.FailureKind)],
     [Maybe Types.Account],
     Types.Account,
     Types.BankStatus)
testAccountCreation = do
    transactions <- processTransactions transactionsInput
    let ((Sch.FilteredTransactions{..}, _), state) =
          Types.runSI (Sch.filterTransactions blockSize transactions)
            dummySpecialBetaAccounts
            Types.dummyChainMeta
            initialBlockState
    let accounts = state ^. blockAccounts
    let accAddrs = map accountAddressFromCred [cdi1,cdi2,cdi3,cdi4,cdi5,cdi7]
    case invariantBlockState state of
        Left f -> liftIO $ assertFailure $ f ++ "\n" ++ show state
        _ -> return ()
    return (ftAdded, ftFailed, map (\addr -> accounts ^? ix addr) accAddrs, accounts ^. singular (ix alesAccount), state ^. blockBank)

checkAccountCreationResult ::
  ([(Types.BareTransaction, Types.ValidResult)],
   [(Types.BareTransaction, Types.FailureKind)],
   [Maybe Types.Account],
   Types.Account,
   Types.BankStatus)
  -> Bool
checkAccountCreationResult (suc, fails, stateAccs, stateAles, bankState) =
  null fails && -- all transactions succeed, but some are rejected
  txsuc &&
  txstateAccs &&
  stateInvariant
  where txsuc = case suc of
          [(_, a11), (_, a12),(_, a13),(_, a14),(_, a15),(_, a16),(_, a17)] |
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed _] _ _ <- a11,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed _] _ _ <- a12,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed _] _ _ <- a13,
            Types.TxReject (Types.DuplicateAccountRegistrationID _) _ _ <- a14,
            Types.TxSuccess [Types.AccountCreated _, Types.CredentialDeployed _] _ _ <- a15,
            Types.TxSuccess [Types.CredentialDeployed _] _ _ <- a16,
            Types.TxReject Types.OutOfEnergy _ _ <- a17 -> True
          _ -> False
        txstateAccs = case stateAccs of
                        -- account for cdi4 was not created because of duplicate registration id
                        -- account for cdi7 was not created because of out of gas
                        [Just _, Just _, Just _, Nothing, Just _, Nothing] -> True
                        _ -> False
        stateInvariant = stateAles ^. Types.accountAmount + bankState ^. Types.executionCost == initialAmount

tests :: Spec
tests =
  describe "Account creation" $ do
    specify "3 accounts created, fourth rejected, one more created, a credential deployed, and out of gas " $ do
      PR.evalContext Init.initialContextData testAccountCreation `shouldReturnP` checkAccountCreationResult
