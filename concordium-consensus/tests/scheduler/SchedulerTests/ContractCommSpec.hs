{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.ContractCommSpec where

-- import Test.Hspec
-- import Test.HUnit

-- import Lens.Micro.Platform

-- import qualified Concordium.Scheduler.Types as Types
-- import qualified Concordium.Scheduler.EnvironmentImplementation as Types
-- import qualified Acorn.Utils.Init as Init
-- import Concordium.Scheduler.Runner
-- import qualified Acorn.Parser.Runner as PR
-- import qualified Concordium.Scheduler as Sch

-- import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
-- import Concordium.GlobalState.Basic.BlockState
-- import Concordium.GlobalState.Basic.BlockState.Invariants

-- import qualified Data.Text.IO as TIO

-- import Control.Monad.IO.Class

-- import qualified Acorn.Core as Core
-- import Concordium.Scheduler.DummyData
-- import Concordium.GlobalState.DummyData
-- import Concordium.Types.DummyData
-- import Concordium.Crypto.DummyData

-- import SchedulerTests.Helpers

-- shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
-- shouldReturnP action f = action >>= (`shouldSatisfy` f)

-- initialBlockState :: BlockState
-- initialBlockState = blockStateWithAlesAccount 100000000 Acc.emptyAccounts

-- transactionsInput :: [TransactionJSON]
-- transactionsInput =
--   [TJSON { payload = DeployModule "CommCounter"
--          , metadata = makeDummyHeader alesAccount 1 10000000
--          , keys = [(0, alesKP)]
--          }
--   ,TJSON { payload = InitContract {amount = 100
--                                   ,contractName = "Recorder"
--                                   ,moduleName = "CommCounter"
--                                   ,parameter = "Unit.Unit"
--                                   }
--          , metadata = makeDummyHeader alesAccount 2 10000000
--          , keys = [(0, alesKP)]
--          }
--   ,TJSON { payload = InitContract {amount = 100
--                                   ,contractName = "Counter"
--                                   ,moduleName = "CommCounter"
--                                   ,parameter = "let pair :: Int64 -> <address> -> Prod.Pair Int64 <address> = Prod.Pair [Int64, <address>] in pair 0 <0, 0>"
--                                   }
--          , metadata = makeDummyHeader alesAccount 3 10000000
--          , keys = [(0, alesKP)]
--          }
--   ,TJSON { payload = Update {amount = 101
--                             ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
--                             ,moduleName = "CommCounter"
--                             ,message = "Inc 100"
--                             }
--          , metadata = makeDummyHeader alesAccount 4 10000000
--          , keys = [(0, alesKP)]
--          }
--   ,TJSON { payload = Update {amount = 100
--                             ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
--                             ,moduleName = "CommCounter"
--                             ,message = "Dec 50"
--                             }
--          , metadata = makeDummyHeader alesAccount 5 10000000
--          , keys = [(0, alesKP)]
--          }
--   ,TJSON { payload = Update {amount = 100
--                             ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
--                             ,moduleName = "CommCounter"
--                             ,message = "Dec 50"
--                             }
--          , metadata = makeDummyHeader alesAccount 6 12000000
--          , keys = [(0, alesKP)]
--          }
--   ,TJSON { payload = Update {amount = 100
--                             ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
--                             ,moduleName = "CommCounter"
--                             ,message = "Dec 1"
--                             }
--          , metadata = makeDummyHeader alesAccount 7 12000000
--          , keys = [(0, alesKP)]
--          }
--   ]

-- type TestResult = ([(Types.BlockItem, Types.ValidResult)],
--                    [(Types.Transaction, Types.FailureKind)])

-- testCommCounter :: PR.Context Core.UA IO TestResult
-- testCommCounter = do
--     let file = "test/contracts/CommCounter.acorn"
--     source <- liftIO $ TIO.readFile file
--     (_, _) <- PR.processModule file source -- execute only for effect on global state
--     transactions <- processUngroupedTransactions transactionsInput
--     let (Sch.FilteredTransactions{..}, finState) =
--             Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
--               dummySpecialBetaAccounts
--               Types.dummyChainMeta
--               maxBound
--               initialBlockState
--     let endState = finState ^. Types.ssBlockState
--     case invariantBlockState endState of
--         Left f -> liftIO $ assertFailure $ f ++ "\n" ++ show endState
--         _ -> return ()
--     return (getResults ftAdded, ftFailed)

-- checkCommCounterResult :: TestResult -> Bool
-- checkCommCounterResult (suc, fails) =
--   null fails && -- should be no failed transactions
--   length reject == 1 &&  -- one rejected (which is also the last one)
--   length nonreject == 6  -- and 6 successful ones
--   where
--     nonreject = filter (\case (_, Types.TxSuccess{}) -> True
--                               (_, Types.TxReject{}) -> False)
--                         suc
--     reject = filter (\case (_, Types.TxSuccess{}) -> False
--                            (_, Types.TxReject{}) -> True
--                     )
--                         suc

-- tests :: SpecWith ()
-- tests =
--   describe "Communicating counter." $
--     specify "6 successful and 1 failed transaction" $
--       PR.evalContext Init.initialContextData testCommCounter `shouldReturnP` checkCommCounterResult
