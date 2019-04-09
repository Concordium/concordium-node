{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module AcornTests.SimpleTransfersTest where

import Test.Hspec

import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as AH
import qualified Concordium.Crypto.Signature as S
import System.Random

import qualified Acorn.Types as Types
import qualified Acorn.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import qualified Acorn.Parser.Runner as PR
import qualified Acorn.Scheduler as Sch

import qualified Data.HashMap.Strict as Map

import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Monad.IO.Class

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

alesACI :: AH.AccountCreationInformation
alesACI = AH.createAccount (S.verifyKey (fst (S.randomKeyPair (mkStdGen 1))))

alesAccount :: Types.AccountAddress
alesAccount = AH.accountAddress alesACI

thomasACI :: AH.AccountCreationInformation
thomasACI = AH.createAccount (S.verifyKey (fst (S.randomKeyPair (mkStdGen 2))))

thomasAccount :: Types.AccountAddress
thomasAccount = AH.accountAddress thomasACI


initialGlobalState :: Types.GlobalState
initialGlobalState = Types.GlobalState Map.empty (Map.fromList [(alesAccount, Types.Account alesAccount 1 100000 alesACI),
                                                                 (thomasAccount, Types.Account thomasAccount 1 100000 thomasACI)]) (let (_,_, gs) = Init.baseState in gs)

testSimpleTransfer
  :: PR.Context
       IO
       ([(Types.MessageTy, Types.ValidResult)],
        [(Types.MessageTy, Types.FailureKind)], Types.Amount, Types.Amount)
testSimpleTransfer = do
    transactionsText <- liftIO $ BSL.readFile "test/transactions/simpletransfers.json"
    transactions <- PR.processTransactions transactionsText
    let ((suc, fails), gstate) = Types.runSI (Sch.makeValidBlock transactions)
                                             Types.dummyChainMeta
                                             initialGlobalState
    
    return (suc, fails, Types.accountAmount $ (Types.accounts gstate) Map.! alesAccount, Types.accountAmount $ (Types.accounts gstate) Map.! thomasAccount)

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
