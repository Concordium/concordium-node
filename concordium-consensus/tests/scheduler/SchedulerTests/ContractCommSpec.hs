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

import qualified Data.HashMap.Strict as Map

import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

alesACI :: AH.AccountCreationInformation
alesACI = AH.createAccount (S.verifyKey (fst (S.randomKeyPair (mkStdGen 1))))

alesAccount :: Types.AccountAddress
alesAccount = AH.accountAddress alesACI

initialGlobalState :: Types.GlobalState
initialGlobalState = (Types.GlobalState Map.empty (Map.fromList [(alesAccount, Types.Account alesAccount 1 1000000 alesACI)])
                      (let (_, _, gs) = Init.baseState in gs))

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployModule "CommCounter"
         , metadata = Types.Header {sender = alesAccount
                                   ,nonce = 1
                                   ,gasAmount = 100000
                                   }
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Recorder"
                                  ,moduleName = "CommCounter"
                                  ,parameter = "Unit.Unit"
                                  }
         , metadata = Types.Header {sender = alesAccount
                                   ,nonce = 2
                                   ,gasAmount = 100000
                                   }
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Counter"
                                  ,moduleName = "CommCounter"
                                  ,parameter = "Prod.Pair [Int64] [<address>] 0 <0, 0>"
                                  }
         , metadata = Types.Header {sender = alesAccount
                                   ,nonce = 3
                                   ,gasAmount = 100000
                                   }
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractVersion = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Inc 100"
                            }
         , metadata = Types.Header {sender = alesAccount
                                   ,nonce = 4
                                   ,gasAmount = 100000
                                   }
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractVersion = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 50"
                            }
         , metadata = Types.Header {sender = alesAccount
                                   ,nonce = 5
                                   ,gasAmount = 100000
                                   }
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractVersion = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 50"
                            }
         , metadata = Types.Header {sender = alesAccount
                                   ,nonce = 6
                                   ,gasAmount = 120000
                                   }
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractVersion = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 1"
                            }
         , metadata = Types.Header {sender = alesAccount
                                   ,nonce = 7
                                   ,gasAmount = 120000
                                   }
         }
  ]

testCommCounter ::
  PR.Context
    IO
    ([(Types.MessageTy, Types.ValidResult)],
     [(Types.MessageTy, Types.FailureKind)])
testCommCounter = do
    source <- liftIO $ TIO.readFile "test/contracts/CommCounter.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state
    transactions <- processTransactions transactionsInput
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
