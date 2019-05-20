{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.AccountTransactionSpecs where

import Test.Hspec

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.Cost as Cost

import Concordium.GlobalState.TreeState.Basic
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Modules as Mod

import Lens.Micro.Platform

import SchedulerTests.DummyData

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState & -- NB: We need 6 * deploy account since we still charge the cost even if an account already exists (case 4 in the tests).
    (blockAccounts .~ Acc.putAccount (Types.Account alesAccount 1 (Types.energyToGtu (6 * Cost.deployAccount + 7 * Cost.checkHeader)) [] alesACI []) Acc.emptyAccounts) .
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.fromModuleList (Init.moduleList gs)))

deployAccountCost :: Types.Amount
deployAccountCost = Types.energyToGtu (Cost.deployAccount + Cost.checkHeader)

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = CreateAccount $ createAccountFrom 10
         , metadata = makeHeader alesKP 1 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = CreateAccount $ createAccountFrom 11
         , metadata = makeHeader alesKP 2 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = CreateAccount $ createAccountFrom 12
         , metadata = makeHeader alesKP 3 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = CreateAccount $ createAccountFrom 12
         , metadata = makeHeader alesKP 4 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = CreateAccount $ createAccountFrom 13
         , metadata = makeHeader alesKP 5 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = CreateAccount $ createAccountFrom 14
         , metadata = makeHeader alesKP 6 deployAccountCost
         , keypair = alesKP
         }
  ,TJSON { payload = CreateAccount $ createAccountFrom 14
         , metadata = makeHeader alesKP 7 100
         , keypair = alesKP
         }
  ]


testAccountCreation ::
  PR.Context
    IO
    ([(Types.Transaction, Types.ValidResult)],
     [(Types.Transaction, Types.FailureKind)],
     [Maybe Types.Account])
testAccountCreation = do
    transactions <- processTransactions transactionsInput
    let ((suc, fails), state) = Types.runSI (Sch.filterTransactions transactions)
                                            Types.dummyChainMeta
                                            initialBlockState
    let accounts = state ^. blockAccounts
    let accAddrs = map accountAddressFrom [10,11,12,13,14]
    
    return (suc, fails, map (\addr -> accounts ^? ix addr) accAddrs)

checkAccountCreationResult :: ([(Types.Transaction, Types.ValidResult)], [(Types.Transaction, Types.FailureKind)], [Maybe Types.Account]) -> Bool
checkAccountCreationResult (suc, fails, state) =
  null fails && -- all transactions succedd, but some are rejected
  txsuc &&
  txstate
  where txsuc = case suc of
          (_, a11) : (_, a12) : (_, a13) : (_, a14) : (_, a15) : (_, a16) : (_, a17) : [] |
            Types.TxSuccess [Types.AccountCreated _] <- a11,
            Types.TxSuccess [Types.AccountCreated _] <- a12,
            Types.TxSuccess [Types.AccountCreated _] <- a13,
            Types.TxReject (Types.AccountAlreadyExists _) <- a14,
            Types.TxSuccess [Types.AccountCreated _] <- a15,
            Types.TxSuccess [Types.AccountCreated _] <- a16,
            Types.TxReject Types.OutOfEnergy <- a17 -> True
          _ -> False
        txstate = case state of
                    [Just _, Just _, Just _, Just _, Just _] -> True
                    _ -> False


tests :: SpecWith ()
tests = 
  describe "Account creation" $ do
    specify "3 accounts created, fourth rejected, two more created, and out of gas " $ do
      PR.evalContext Init.initialContextData testAccountCreation `shouldReturnP` checkAccountCreationResult
