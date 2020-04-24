{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{-|
Testing of 'Concordium.Scheduler.filterTransactions'.
See also 'SchedulerTests.TransactionGroupingSpec2'.

TODO These tests need to be updated to the newer specification of
'Concordium.Scheduler.filterTransactions' or should be removed.
-}
module SchedulerTests.TransactionGroupingSpec where

import Test.Hspec
import Test.HUnit

import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import Lens.Micro.Platform

import Acorn.Core
import qualified Acorn.Utils.Init as Init
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 200000 Acc.emptyAccounts

baker :: (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker = mkFullBaker 1 alesAccount

transactions :: [[TransactionJSON]]
transactions = [[-- t1: first transaction in group valid
                TJSON { payload = Transfer { toaddress = Types.AddressAccount alesAccount, amount = 100 }
                      , metadata = makeDummyHeader alesAccount 1 1000
                      , keypair = alesKP
                      }
                -- t2: second transaction in group invalid: non-sequential nonce
                ,TJSON { payload = Transfer { toaddress = Types.AddressAccount alesAccount, amount = 100 }
                       , metadata = makeDummyHeader alesAccount 3 1000
                       , keypair = alesKP
                       }
                -- t3: remaining transactions in group valid again
               ,TJSON { payload = AddBaker (baker ^. _1 . bakerElectionVerifyKey)
                                           (baker ^. _2)
                                           (baker ^. _1 . bakerSignatureVerifyKey)
                                           (baker ^. _1 . bakerAggregationVerifyKey)
                                           (baker ^. _4)
                                           (baker ^. _3)
                                           alesAccount
                                           alesKP
                      , metadata = makeDummyHeader alesAccount 2 10000
                      , keypair = alesKP
                      }
                -- TODO update following
                -- t4: invalid (SuccessorOfInvalidTransaction)
               ,TJSON { payload = UpdateBakerAccount 0 alesAccount alesKP
                      , metadata = makeDummyHeader alesAccount 2 10000
                      , keypair = alesKP
                      }
               ],
               [-- t5: next group can have valid transactions again
                TJSON { payload = AddBaker (baker ^. _1 . bakerElectionVerifyKey)
                                           (baker ^. _2)
                                           (baker ^. _1 . bakerSignatureVerifyKey)
                                           (baker ^. _1 . bakerAggregationVerifyKey)
                                           (baker ^. _4)
                                           (baker ^. _3)
                                           alesAccount
                                           alesKP
                      , metadata = makeDummyHeader alesAccount 2 10000
                      , keypair = alesKP
                      }
                -- t6: second valid transaction
                ,TJSON { payload = UpdateBakerSignKey 0 (BlockSig.verifyKey (bakerSignKey 3)) (BlockSig.signKey (bakerSignKey 3))
                      , metadata = makeDummyHeader alesAccount 3 10000
                      , keypair = alesKP
                      }
                -- t7: invalid transaction: DepositInsufficient
               ,TJSON { payload = DelegateStake 0
                      , metadata = makeDummyHeader alesAccount 4 1
                      , keypair = alesKP
                      }
                -- t8: remaining transaction invalid with SuccessorOfInvalidTransaction failure
                -- even if it would have a different failure reason (NonSequentialNonce) if it were processed
               ,TJSON { payload = UndelegateStake
                      , metadata = makeDummyHeader alesAccount 5 100000
                      , keypair = alesKP
                      }
               ],
               -- empty group should be OK
               [],
               [
               -- t9: first transaction in new group valid again
                TJSON { payload = RemoveBaker 0
                      , metadata = makeDummyHeader alesAccount 4 10000
                      , keypair = alesKP
                      }
               -- t10: unprocessed due to block energy limit ('maxEnergy')
               ,TJSON { payload = DeployModule "FibContract"
                      , metadata = makeDummyHeader alesAccount 5 20000
                      , keypair = alesKP
                      }
               -- t11: transaction succeeding an unprocessed transaction can still be valid
               ,TJSON { payload = Transfer { toaddress = Types.AddressAccount alesAccount, amount = 100 }
                     , metadata = makeDummyHeader alesAccount 5 1000
                     , keypair = alesKP
                     }
               ]]

maxEnergy :: Types.Energy
maxEnergy = Types.Energy 20000

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [Types.Transaction],
                   [Types.Transaction])

testGrouping :: PR.Context UA IO TestResult
testGrouping = do
    source <- liftIO $ TIO.readFile "test/contracts/FibContract.acorn"
    (_, _) <- PR.processModule source
    ts <- processGroupedTransactions transactions
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize ts)
            dummySpecialBetaAccounts
            Types.dummyChainMeta
            maxEnergy
            initialBlockState
    let gstate = finState ^. Types.ssBlockState
    case invariantBlockState gstate of
        Left f -> liftIO $ assertFailure f
        Right _ -> return (getResults ftAdded, ftFailed, ftUnprocessed, concat (Types.perAccountTransactions ts))

checkResult :: TestResult -> Expectation
checkResult (valid, invalid, unproc, [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11]) =
    validCheck >> invalidCheck >> unprocessedCheck
    where
        validCheck = do
          let (validTs, validResults) = unzip valid
          assertEqual "1st, 3rd, 5th, 6th, 9th, 11th transactions are valid:" (map (Types.NormalTransaction <$>) [t1, t3, t5, t6, t9, t11]) validTs
          assertEqual "1st, 3rd, 5th, 6th, 9th, 11th transactions are valid with TxSuccess result:"
            True $ all (\case Types.TxSuccess{} -> True
                              _ -> False) validResults
        invalidCheck = do
          assertEqual "4 invalid transactions" 4 (length invalid)
          assertEqual "2nd transaction fails with NonSequentialNonce reason:" True $
            (t2, Types.NonSequentialNonce 2) `elem` invalid
          assertEqual "7th transaction fails with DepositInsufficient reason:" True $
            (t7, Types.DepositInsufficient) `elem` invalid
          assertEqual "4th, 8th transactions fail with SuccessorOfInvalidTransaction reason" True $
            all (`elem` invalid) $ zip [t4, t8] (repeat Types.SuccessorOfInvalidTransaction)
        unprocessedCheck =
            assertEqual "10th transaction is unprocessed because the block runs out of energy:" [t10] unproc
checkResult _ = assertFailure "There should be 11 filtered transactions."

tests :: Spec
tests =
  xdescribe "Transaction grouping test:" $
    specify "6 valid, 4 invalid, 1 unprocessed transaction" $
        PR.evalContext Init.initialContextData testGrouping >>= checkResult
