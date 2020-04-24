{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.UpdateElectionDifficultySpec where

import Test.Hspec

import qualified Data.HashSet as Set
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants

import qualified Acorn.Core as Core

import Lens.Micro.Platform

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    100000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000) Acc.emptyAccounts)

-- | The initial election difficulty (NOTE: This value is used for verification,
-- the difficulty is not set from this value).
initialElectionDifficulty :: Types.ElectionDifficulty
initialElectionDifficulty = 0.5

specialBetaAccounts :: Set.HashSet Types.AccountAddress
specialBetaAccounts = Set.fromList [alesAccount]

-- | Transactions that should run successful, with the resulting
-- election difficulty birk parameter and event list.
testCasesSuccess :: [(TransactionJSON, (Double, [Types.Event]))]
testCasesSuccess =
    let nonces = [1..]
        legalElectionDifficulties = [0, 0.0001, 0.5, 0.99999]
    in
      zip nonces legalElectionDifficulties <&> \(n, d) ->
      ( TJSON { payload = UpdateElectionDifficulty d
              , metadata = makeDummyHeader alesAccount n 10000
              , keypair = alesKP
              }
      , (d, [Types.ElectionDifficultyUpdated d])
      )

-- | Transactions that should be rejected with the given reason.
testCasesReject :: [(TransactionJSON, Types.RejectReason)]
testCasesReject =
    let nonces = [1..]
        legalElectionDifficulty = 0.527583
        illegalElectionDifficulties = [-0.00001, -0.5, -0.99999, -1, -2, -1000,
                                        1, 1.000001, 1.5, 2.0, 3000]
    in zipWith (\n tc -> tc n) nonces
      (illegalElectionDifficulties <&> \d n ->
          ( TJSON { payload = UpdateElectionDifficulty d
                  , metadata = makeDummyHeader alesAccount n 10000
                  , keypair = alesKP
                  }
          , Types.SerializationFailure
          )
      )
      ++
      [ ( TJSON { payload = UpdateElectionDifficulty legalElectionDifficulty
                , metadata = makeDummyHeader thomasAccount 1 10000
                , keypair = thomasKP
                }
        , Types.NotFromSpecialAccount
        )
      ]

type TestResult = ([([(Types.BlockItem, Types.ValidResult)],
                      [(Types.Transaction, Types.FailureKind)], Types.BirkParameters)],
                    BlockState)

runWithIntermediateStates
  :: [TransactionJSON]
  -> PR.Context Core.UA IO TestResult
runWithIntermediateStates transactionsInput = do
  txs <- processUngroupedTransactions transactionsInput
  let (res, state) = foldl (\(acc, st) tx ->
                            let (Sch.FilteredTransactions{..}, st') =
                                  Types.runSI
                                    (Sch.filterTransactions dummyBlockSize (Types.fromTransactions [tx]))
                                    specialBetaAccounts
                                    Types.dummyChainMeta
                                    maxBound
                                    st
                            in (acc ++ [(getResults ftAdded, ftFailed, st' ^. Types.ssBlockState . blockBirkParameters)], st' ^. Types.schedulerBlockState))
                         ([], initialBlockState)
                         (Types.perAccountTransactions txs)
  return (res, state)


-- | Run the given transactions, testing the result with the given spec and some basic specs.
runAndTest :: String -> [TransactionJSON] -> (TestResult -> Spec) -> Spec
runAndTest name transactionsInput specs = do
  describe name $ do
    -- Basic specs
    res@(results, endState) <- runIO (PR.evalContext Init.initialContextData $ runWithIntermediateStates transactionsInput)
    specify "Result state satisfies invariant" $
      case invariantBlockState endState of
        Left f -> expectationFailure f
        Right _ -> return ()
    specify "Correct number of transactions" $
      length results `shouldBe` length transactionsInput
    -- Given specs
    specs res

-- NEXT: Testing can be enhanced by combining ok and failing transactions into one
-- run (use Either for result specification) in randomized order, checking that after a failed transaction,
-- the election difficulty is still the old. For the latter, first extend the entries in the result list
-- with the birk parameters from the previous entry.
tests :: Spec
tests =
  describe "UpdateElectionDifficulty" $ do
    runAndTest "Successful transactions" (map fst testCasesSuccess) $ \(results, _) ->
      describe "Checking results" $ do
        mapM_ ( \( ( ok
                   , failed
                   , bps)
                 , (d, expectedEvents)
                 ) -> do
                  specify "Not failed" $ failed `shouldBe` []
                  specify "Correct events" $
                    case ok of
                      [(_, Types.TxSuccess events)] -> events `shouldBe` expectedEvents
                      _ -> expectationFailure $ "Check failed on " ++ show ok
                  specify "Correct value in birk parameters" $
                    bps ^. Types.birkElectionDifficulty `shouldBe` d
              ) $
          zip results (map snd testCasesSuccess)
    runAndTest "Rejected transactions" (map fst testCasesReject) $ \(results, _) -> do
      describe "Checking results" $ do
        mapM_ ( \( ( ok
                   , failed
                   , bps)
                 , expectedRejectReason
                 ) -> do
                  specify "Not failed" $ failed `shouldBe` []
                  specify "Correct reject reason" $
                    case ok of
                      [(_, Types.TxReject rejectReason)] -> rejectReason `shouldBe` expectedRejectReason
                      _ -> expectationFailure $ "Check failed on " ++ show ok
                  specify "Election difficulty not changed" $
                    bps ^. Types.birkElectionDifficulty `shouldBe` initialElectionDifficulty
              ) $
          zip results (map snd testCasesReject)

