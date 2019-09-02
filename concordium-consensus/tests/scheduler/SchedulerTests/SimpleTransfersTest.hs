{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
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

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Rewards as Rew
import Concordium.GlobalState.Basic.Invariants

import qualified Acorn.Core as Core

import SchedulerTests.DummyData

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState emptyBirkParameters Types.dummyCryptographicParameters &
    (blockAccounts .~ Acc.putAccount (mkAccount alesVK 100000)
                      (Acc.putAccount (mkAccount thomasVK 100000) Acc.emptyAccounts)) .
    (blockBank . Rew.totalGTU .~ 200000) .
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.fromModuleList (Init.moduleList gs)))

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = Transfer {toaddress = Types.AddressAccount alesAccount, amount = 100 }
         , metadata = makeHeader alesKP 1 1000
         , keypair = alesKP
         }
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 88 }
         , metadata = makeHeader alesKP 2 1000
         , keypair = alesKP
         }
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 98700 }
         , metadata = makeHeader alesKP 3 1000
         , keypair = alesKP
         }
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount alesAccount, amount = 100 }
         , metadata = makeHeader thomasKP 1 500
         , keypair = thomasKP
         }
    -- the next transaction should fail because the balance on alesAccount is now 1012, which is
    -- less than 600 + 500
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 600 }
         , metadata = makeHeader alesKP 4 500
         , keypair = alesKP
         }    
  ]


testSimpleTransfer
  :: PR.Context Core.UA
       IO
       ([(Types.Transaction, Types.ValidResult)],
        [(Types.Transaction, Types.FailureKind)], Types.Amount, Types.Amount)
testSimpleTransfer = do
    transactions <- processTransactions transactionsInput
    let ((suc, fails), gstate) = Types.runSI (Sch.filterTransactions transactions)
                                             Types.dummyChainMeta
                                             initialBlockState
    case invariantBlockState gstate of
        Left f -> liftIO $ assertFailure f
        Right _ -> return ()
    return (suc,
            fails,
            gstate ^. blockAccounts . singular (ix alesAccount) . Types.accountAmount,
            gstate ^. blockAccounts . singular (ix thomasAccount) . Types.accountAmount)

checkSimpleTransferResult :: ([(a, Types.ValidResult)], [b], Types.Amount, Types.Amount) -> Bool
checkSimpleTransferResult (suc, fails, alesamount, thomasamount) =
  null fails && -- should be no failed transactions
  reject &&  -- the last transaction is rejected
  nonreject && -- all initial transactions are successful
  alesamount == (100000 - 4 * fromIntegral Cost.checkHeader - 88 - 98700 + 100) &&
  thomasamount == (100000 - fromIntegral Cost.checkHeader + 88 + 98700 - 100)
  where 
    nonreject = all (\case (_, Types.TxSuccess _) -> True
                           (_, Types.TxReject _ _) -> False)
                    (init suc)
    reject = case last suc of
               (_, Types.TxReject (Types.AmountTooLarge _ _) _) -> True
               _ -> False

tests :: SpecWith ()
tests = 
  describe "Simple transfers test:" $ do
    specify "3 successful and 1 failed transaction" $ do
      PR.evalContext Init.initialContextData testSimpleTransfer `shouldReturnP` checkSimpleTransferResult
