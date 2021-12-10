{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Small test framework for transactions.
Allows to specify test cases consisting of a list of transactions to be executed in order
together with assertions on the result after each step, i.e. on generated events for accepted
transactions, the reject reason for rejected transactions, the failure kind for failed
transactions and the updated block state.
Also checks invariants on the block state after each processed transaction.

NOTE: This processes each transaction individually - for testing grouped transactions, see
      'SchedulerTests.TransactionGroupingSpec' and 'SchedulerTests.TransactionGroupingSpec2'.
-}
module SchedulerTests.TestUtils(PV1, PV2, PV3, ResultSpec,TResultSpec(..),emptySpec,emptyExpect,TestCase(..),
                                TestParameters(..),defaultParams, mkSpec,mkSpecs, createAlias) where

import Test.Hspec

import Lens.Micro.Platform
import Control.Monad
import Data.List (foldl')

import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.DummyData
import Concordium.Scheduler.DummyData
import Data.Time

-- |Protocol version
type PV1 = 'P1
type PV2 = 'P2
type PV3 = 'P3


-- | Specification on the expected result of executing a transaction and the resulting block state.
type ResultSpec pv = (TResultSpec, BlockState pv -> Spec)

-- | Specification on the expected result of executing a transaction.
data TResultSpec
  -- | Expect the transaction to succeed, satisfying the given 'Spec's on the resulting event list.
  = Success ([Event] -> Expectation)
  -- | Like 'Success' but with exactly the given list of events.
  | SuccessE [Event]
  -- | Like Success but has access to the full transaction summary.
  | SuccessWithSummary (BlockItem -> TransactionSummary -> Expectation)
  -- | Expect the transaction to be rejected with the given reason.
  | Reject RejectReason
  -- | Expect the transaction to fail with the given reason.
  | Fail FailureKind


emptySpec :: a -> Spec
emptySpec _ = return ()

emptyExpect :: a -> Expectation
emptyExpect _ = return ()


data TestParameters pv = TestParameters
  { tpChainMeta :: ChainMetadata
    -- | The blockstate to start from.
  , tpInitialBlockState :: BlockState pv
    -- | Limit on the total energy the processed transactions can use.
  , tpEnergyLimit :: Energy
    -- | Limit on the number of credential deployments that can occur in a block.
  , tpMaxCredentials :: CredentialsPerBlockLimit
    -- | Limit on the total size of the processed transactions.
  , tpSizeLimit :: Integer
    -- | Block construction timeout
  , tpBlockTimeout :: UTCTime
  }

defaultParams :: forall pv. (IsProtocolVersion pv, ChainParametersVersionFor pv ~ 'ChainParametersV0) => TestParameters pv
defaultParams = TestParameters
  { tpChainMeta = dummyChainMeta
  , tpInitialBlockState = createBlockState @pv Acc.emptyAccounts
  , tpEnergyLimit = maxBound
  , tpMaxCredentials = maxBound
  , tpSizeLimit = fromIntegral $ (maxBound :: Int)
  , tpBlockTimeout = dummyBlockTimeout
  }

-- | A test case for executing a list of transactions, specifying 'ResultSpec's for the result
-- of each transaction's execution. The transactions are run with 'Sch.filterTransactions'
-- in sequenced and not grouped, with the given parameters.
data TestCase pv = TestCase
  { -- | A name for the test case, which is printed.
    tcName :: String
    -- | Parameters for executing the transactions.
  , tcParameters :: TestParameters pv
    -- | The transactions to run, with their respective 'ResultSpec'.
    -- NOTE: The following could be parametrized over the loaded module data like references, names etc.
    -- to be able to specify result events like specifying the TJSON.
    -- See transactionHelper in Runner to implement this.
  , tcTransactions :: [(TransactionJSON, ResultSpec pv)]
  }

-- | Result of processing a single transaction.
data ProcessResult
  = Valid (BlockItem, TransactionSummary)
  | Failed FailureKind
  | Unprocessed
  deriving (Eq, Show)


-- | Execute the given transactions in sequence (ungrouped) with 'Sch.filterTransactions',
-- with the given parameters. Returns a list of result and block state after each transaction.
runWithIntermediateStates ::
  IsProtocolVersion pv
  => TestParameters pv
  -> [TransactionJSON]
  -> IO [(ProcessResult, BlockState pv, Amount)]
runWithIntermediateStates TestParameters{..} transactions = do
  -- Create actual 'Transaction's from the 'TransactionJSON'.
  txs <- processUngroupedTransactions transactions
  return $ reverse $ (^. _1) $
    foldl' (\(acc, st, feesAccum) tx ->
                            let (ft@Sch.FilteredTransactions{..}, st') =
                                  Types.runSI
                                    (Sch.filterTransactions tpSizeLimit tpBlockTimeout (fromTransactions [tx]))
                                    tpChainMeta
                                    tpEnergyLimit
                                    tpMaxCredentials
                                    st
                            in if length ftAdded + length ftFailed + length ftUnprocessed == 1
                                  && (length ftFailedCredentials + length ftUnprocessedCredentials == 0)
                               then
                                 let res
                                       | not $ null ftAdded = Valid $ head ftAdded
                                       | not $ null ftFailed = Failed $ snd $ head ftFailed
                                       | not $ null ftUnprocessed = Unprocessed
                                       | otherwise = error "Failure in test setup."
                                 in ((res, st' ^. Types.ssBlockState, feesAccum + st' ^. Types.schedulerExecutionCosts):acc, st' ^. Types.schedulerBlockState, feesAccum + st' ^. Types.schedulerExecutionCosts)
                               else error $ "Failure in test setup: Expected one regular transaction in result, but got " ++ show ft
            )
                      ([], tpInitialBlockState, 0)
                      (perAccountTransactions txs)


-- | Make a 'Spec' from the given 'TestCase', running the specified transactions in sequence (not grouped),
-- and checking the following:
--
-- * Invariants on the block state after each transaction.
-- * The specified 'ResultSpec' for the result of each transaction.
--
-- Note: If there are errors thrown while processing a transactions, this currently results in the
-- test suite failing without showing in which test case this happens (probably because tests are
-- executed already when constructing the test suite).
mkSpec :: IsProtocolVersion pv => TestCase pv -> Spec
mkSpec TestCase{..} =
  describe tcName $ do
  let (tJsons, resultSpecs) = unzip tcTransactions
  results <- runIO (runWithIntermediateStates tcParameters tJsons)
  -- NB: This check is important to make sure that the zipped lists below have the same length and
  -- thus all 'ResultSpec's are checked.
  specify "Correct number of transactions" $
    length results `shouldBe` length tJsons
  describe "Checking results" $
    forM_ (zip3 [(1 :: Integer)..] results resultSpecs) $
      \( number
       , (res, bs, fees)
       , (resultSpec, bsSpec)
       ) -> describe ("Transaction " ++ show number) $ do
        specify "New block state satisfies invariant" $
          case invariantBlockState bs fees of
            Left f -> expectationFailure f
            Right _ -> return ()
        describe "Checks on block state" $ bsSpec bs
        let failure = expectationFailure $ "Got: " ++ show res
        case resultSpec of
          Success eventsSpec ->
            case res of
              Valid (_, ts) ->
                case tsResult ts of
                  TxSuccess events -> do
                    specify "Transaction valid and successful" emptyExpect
                    specify "Events satisfy spec" $  eventsSpec events
                  _ -> specify "Transaction successful" failure
              _ -> specify "Transaction valid" failure
          SuccessE expectedEvents ->
            case res of
              Valid (_, ts) ->
                case tsResult ts of
                  TxSuccess events -> do
                    specify "Transaction valid and successful" emptyExpect
                    specify "Correct events" $ events `shouldBe` expectedEvents
                  _ -> specify "Transaction successful" failure
              _ -> specify "Transaction valid" failure
          SuccessWithSummary ensure ->
            case res of
              Valid (bi, ts) -> specify "Ensure successful outcome " $ ensure bi ts
              _ -> specify "Transaction valid" failure
          Reject expectedReason ->
            case res of
              Valid (_, ts) ->
                case tsResult ts of
                  TxReject reason -> do
                    specify "Transaction valid and rejected" emptyExpect
                    specify "Correct reject reason" $ reason `shouldBe` expectedReason
                  _ -> specify "Transaction rejected" failure
              _ -> specify "Transaction valid" failure
          Fail expectedFk ->
            case res of
              Failed fk -> do
                specify "Transaction failed" emptyExpect
                specify "Correct failure kind" $ fk `shouldBe` expectedFk
              _ -> specify "Transaction failed" failure

-- | Make a 'Spec' for the given test cases. See 'mkSpec'.
mkSpecs :: IsProtocolVersion pv => [TestCase pv] -> Spec
mkSpecs = mapM_ mkSpec
