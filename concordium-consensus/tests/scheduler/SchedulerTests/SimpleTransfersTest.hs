{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.SimpleTransfersTest where

import Test.Hspec

import Lens.Micro.Platform

import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as AH
import qualified Concordium.Crypto.BlockSignature as S
import System.Random

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Modules as Mod

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

alesKP :: S.KeyPair
alesKP = fst (S.randomKeyPair (mkStdGen 1))

alesACI :: AH.AccountCreationInformation
alesACI = AH.createAccount (S.verifyKey alesKP)

alesAccount :: Types.AccountAddress
alesAccount = AH.accountAddress alesACI

thomasKP :: S.KeyPair
thomasKP = fst (S.randomKeyPair (mkStdGen 2))

thomasACI :: AH.AccountCreationInformation
thomasACI = AH.createAccount (S.verifyKey thomasKP)

thomasAccount :: Types.AccountAddress
thomasAccount = AH.accountAddress thomasACI

initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState &
    (blockAccounts .~ Acc.putAccount (Types.Account alesAccount 1 100000 alesACI)
                      (Acc.putAccount (Types.Account thomasAccount 1 100000 thomasACI) Acc.emptyAccounts)) .
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.Modules gs))

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = Transfer {toaddress = Types.AddressAccount alesAccount, amount = 100 }
         , metadata = Types.TransactionHeader alesAccount 1 123
         , keypair = alesKP
         }
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 88 }
         , metadata = Types.TransactionHeader alesAccount 2 123
         , keypair = alesKP
         }
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 99812 }
         , metadata = Types.TransactionHeader alesAccount 3 100
         , keypair = alesKP
         }    
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount alesAccount, amount = 100 }
         , metadata = Types.TransactionHeader thomasAccount 1 100
         , keypair = thomasKP
         }    
  ,TJSON { payload = Transfer {toaddress = Types.AddressAccount thomasAccount, amount = 101 }
         , metadata = Types.TransactionHeader alesAccount 4 100
         , keypair = alesKP
         }    
  ]


testSimpleTransfer
  :: PR.Context
       IO
       ([(Types.Transaction, Types.ValidResult)],
        [(Types.Transaction, Types.FailureKind)], Types.Amount, Types.Amount)
testSimpleTransfer = do
    transactions <- processTransactions transactionsInput
    let ((suc, fails), gstate) = Types.runSI (Sch.filterTransactions transactions)
                                             Types.dummyChainMeta
                                             initialBlockState
    
    return (suc,
            fails,
            gstate ^. blockAccounts . singular (ix alesAccount) . Types.accountAmount,
            gstate ^. blockAccounts . singular (ix thomasAccount) . Types.accountAmount)

checkSimpleTransferResult :: ([(a, Types.ValidResult)], [b], Types.Amount, Types.Amount) -> Bool
checkSimpleTransferResult (suc, fails, alesamount, thomasamount) =
  null fails && -- should be no failed transactions
  reject &&  -- the last transaction is rejected
  nonreject && -- all initial transactions are successful
  alesamount == 200 &&
  thomasamount == 199800
  where 
    nonreject = all (\case (_, Types.TxSuccess _) -> True
                           (_, Types.TxReject _) -> False)
                    (init suc)
    reject = case last suc of
               (_, Types.TxReject (Types.AmountTooLarge _ _)) -> True
               _ -> False


tests :: SpecWith ()
tests = 
  describe "Simple transfers test:" $ do
    specify "3 successful and 1 failed transaction" $ do
      PR.evalContext Init.initialContextData testSimpleTransfer `shouldReturnP` checkSimpleTransferResult
