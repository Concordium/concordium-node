{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module AcornTests.ContractCommSpec where

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

import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Monad.IO.Class

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

thomasAccount :: Types.AccountAddress
thomasAccount = "1mm27ZCNW8i2H2iu88YsDGgAQn4T"

alesACI :: AH.AccountCreationInformation
alesACI = AH.createAccount (S.verifyKey (fst (S.randomKeyPair (mkStdGen 1))))

alesAccount :: Types.AccountAddress
alesAccount = AH.accountAddress alesACI

initialGlobalState :: Types.GlobalState
initialGlobalState = (Types.GlobalState Map.empty (Map.fromList [(alesAccount, Types.Account alesAccount 1 100000 alesACI)]) (let (_, _, gs) = Init.baseState in gs))

testCommCounter ::
  PR.Context
    IO
    ([(Types.MessageTy, Types.ValidResult)],
     [(Types.MessageTy, Types.FailureKind)])
testCommCounter = do
    source <- liftIO $ TIO.readFile "test/contracts/CommCounter.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state
    transactionsText <- liftIO $ BSL.readFile "test/transactions/commcounter.json"
    transactions <- PR.processTransactions transactionsText
    let (suc, fails) = Types.evalSI (Sch.makeValidBlock transactions)
                                    Types.dummyChainMeta
                                    initialGlobalState
    return (suc, fails)

checkCommCounterResult :: ([(a, Types.ValidResult)], [b]) -> Bool
checkCommCounterResult (suc, fails) =
  null fails && -- should be no failed transactions
  length reject == 1 &&  -- one rejected (which is also the last one)
  length nonreject == 6  -- and 6 successful ones
  where 
    nonreject = filter (\case (_, Types.TxSuccess _) -> True
                              (_, Types.TxReject _) -> False)
                        suc
    reject = filter (\case (_, Types.TxSuccess _) -> False
                           (_, Types.TxReject _) -> True
                    )
                        suc

tests :: SpecWith ()
tests = 
  describe "Communicating counter." $ do
    specify "6 successful and 1 failed transaction" $ do
      PR.evalContext Init.initialContextData testCommCounter `shouldReturnP` checkCommCounterResult
