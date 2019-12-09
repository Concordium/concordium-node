{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.CredentialTest where

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

import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Rewards as Rew
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants

import qualified Acorn.Core as Core

import SchedulerTests.DummyData

-- Test that sending to and from an account without credentials fails.
-- Also test that the same holds for smart contracts.

-- Create initial state where alesAccount has a credential, but thomasAccount does not.
initialBlockState :: BlockState
initialBlockState =
  emptyBlockState emptyBirkParameters dummyCryptographicParameters &
    (blockAccounts .~ Acc.putAccountWithRegIds (mkAccount alesVK 100000)
                      (Acc.putAccountWithRegIds (mkAccountNoCredentials thomasVK 100000) Acc.emptyAccounts)) .
    (blockBank . Rew.totalGTU .~ 200000) .
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.fromModuleList (Init.moduleList gs)))

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = Transfer {toaddress = Types.AddressAccount alesAccount, amount = 0 }
         , metadata = makeHeader alesKP 1 1000
         , keypair = alesKP
         }
   -- The next one should fail because the recepient account is not valid.
   -- The transaction should be in a block, but rejected.
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 0 }
         , metadata = makeHeader alesKP 2 1000
         , keypair = alesKP
         }
   -- The next transaction should not be part of a block since it is being sent by an account
   -- without a credential
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 0 }
         , metadata = makeHeader thomasKP 1 1000
         , keypair = alesKP
         }
  ]


testCredentialCheck
  :: PR.Context Core.UA
       IO
       ([(Types.BareTransaction, Types.ValidResult)],
        [(Types.BareTransaction, Types.FailureKind)],
        [Types.BareTransaction])
testCredentialCheck = do
    transactions <- processTransactions transactionsInput
    let ((Sch.FilteredTransactions{..}, _), gstate) =
          Types.runSI (Sch.filterTransactions blockSize transactions)
            dummySpecialBetaAccounts
            Types.dummyChainMeta
            initialBlockState
    case invariantBlockState gstate of
        Left f -> liftIO $ assertFailure f
        Right _ -> return ()
    return (ftAdded, ftFailed, transactions)

checkCredentialCheckResult :: ([(Types.BareTransaction, Types.ValidResult)],
                               [(Types.BareTransaction, Types.FailureKind)],
                               [Types.BareTransaction]) -> Expectation
checkCredentialCheckResult (suc, fails, transactions) =
  failsCheck >> rejectCheck >> nonrejectCheck
  where
    -- last transaction failed with the right message
    failsCheck =
      case fails of
        [(tx, fr)] -> do
          assertEqual "Last transaction fails:" tx (transactions !! 2)
          assertEqual "With failure NoValidCredential:" fr Types.NoValidCredential
        xs -> assertFailure $ "List should be a singleton:" ++ show xs
    rejectCheck =
      case last suc of
        (tx, Types.TxReject (Types.ReceiverAccountNoCredential _) _ _) ->
          assertEqual "The second transaction should be rejected." tx (transactions !! 1)
        other -> assertFailure $ "Last recorded transaction should fail with no account credential: " ++ show other
    nonrejectCheck =
      case head suc of
        (tx, Types.TxSuccess{}) ->
          assertEqual "The first transaction should be successful." tx (transactions !! 0)
        other -> assertFailure $ "First recorded transaction should be successful: " ++ show other
    

tests :: Spec
tests =
  describe "Credential check test:" $
    specify "one successful, one rejected, one failed transaction" $
      PR.evalContext Init.initialContextData testCredentialCheck >>= checkCredentialCheckResult
