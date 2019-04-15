{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.ContractCommSpec where

import Test.Hspec

import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as AH
import qualified Concordium.Crypto.Signature as S
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

import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

alesACI :: AH.AccountCreationInformation
alesACI = AH.createAccount (S.verifyKey alesKP)

alesAccount :: Types.AccountAddress
alesAccount = AH.accountAddress alesACI

alesKP :: S.KeyPair
alesKP = fst (S.randomKeyPair (mkStdGen 1))

initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState
    { blockAccounts = Acc.putAccount (Types.Account alesAccount 1 1000000 alesACI) Acc.emptyAccounts
    , blockModules = (let (_, _, gs) = Init.baseState in Mod.Modules gs) }

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployModule "CommCounter"
         , metadata = Types.TransactionHeader alesAccount 1 100000
         , keypair = alesKP
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Recorder"
                                  ,moduleName = "CommCounter"
                                  ,parameter = "Unit.Unit"
                                  }
         , metadata = Types.TransactionHeader alesAccount 2 100000
         , keypair = alesKP
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Counter"
                                  ,moduleName = "CommCounter"
                                  ,parameter = "Prod.Pair [Int64] [<address>] 0 <0, 0>"
                                  }
         , metadata = Types.TransactionHeader alesAccount 3 100000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractVersion = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Inc 100"
                            }
         , metadata = Types.TransactionHeader alesAccount 4 100000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractVersion = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 50"
                            }
         , metadata = Types.TransactionHeader alesAccount 5 100000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractVersion = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 50"
                            }
         , metadata = Types.TransactionHeader alesAccount 6 120000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractVersion = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 1"
                            }
         , metadata = Types.TransactionHeader alesAccount 7 120000
         , keypair = alesKP
         }
  ]

testCommCounter ::
  PR.Context
    IO
    ([(Types.Transaction, Types.ValidResult)],
     [(Types.Transaction, Types.FailureKind)])
testCommCounter = do
    source <- liftIO $ TIO.readFile "test/contracts/CommCounter.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state
    transactions <- processTransactions transactionsInput
    let (suc, fails) = Types.evalSI (Sch.makeValidBlock transactions)
                                    Types.dummyChainMeta
                                    initialBlockState
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
