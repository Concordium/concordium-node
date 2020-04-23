{-|
Small test framework for transactions.
Allows to specify test cases consisting of a list of transactions to be executed in order
together with assertions on the result after each step, i.e. on generated events for accepted
transactions, on the reject reason for rejected transactions, as well as on the updated block state.
Also checks invariants on the block state after each processed transaction.

NOTE: Currently this does not support testing for failed transactions, but this can be added easily.
NOTE: This processes each transaction individually - for testing grouped transactions, see
      'SchedulerTests.TransactionGroupingSpec' and 'SchedulerTests.TransactionGroupingSpec2'.
-}
module SchedulerTests.TestUtils(ResultSpec,TResultSpec(..),emptySpec,emptyExpect,TestCase(..),
                               mkSpec,mkSpecs) where

import Test.Hspec

import Lens.Micro.Platform
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO

import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Environment as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner

import qualified Acorn.Core as Core
import qualified Acorn.Utils.Init as Init
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants

-- | Specification on the expected result of executing a transaction and the resulting block state.
type ResultSpec = (TResultSpec, BlockState -> Spec)

-- | Specification on the expected result of executing a transaction.
data TResultSpec
  -- | Expect the transaction to succeed, satisfying the given 'Spec's on the resulting event list.
  = Success ([Event] -> Expectation)
  -- | Like 'Success' but with exactly the given list of events.
  | SuccessE [Event]
  -- | Expect the transaction to be rejected with the given reason.
  | Reject RejectReason
  -- | Expect the transaction to fail with the given reason.
  | Fail FailureKind


emptySpec :: a -> Spec
emptySpec _ = return ()

emptyExpect :: a -> Expectation
emptyExpect _ = return ()

-- | A test case for executing a list of transactions, specifying 'ResultSpec's for the result
-- of each transaction's execution.
data TestCase = TestCase
  { -- | A name for the test case, which is printed.
    tcName :: String
    -- | The 'Types.SpecialBetaAccounts' to run with.
  , tcSpecialAccounts :: Types.SpecialBetaAccounts
    -- | The blockstate to start from.
  , tcInitialBlockState :: BlockState
    -- | Modules within in the test/ directory to load.
  , tcModules :: [String]
    -- | The transactions to run, with their respective 'ResultSpec'.
    -- NOTE: The following could be parametrized over the loaded module data like references, names etc.
    -- to be able to specify result events like specifying the TJSON.
    -- See transactionHelper in Runner to implement this.
  , tcTransactions :: [(TransactionJSON, ResultSpec)]
  }

-- | Result of processing a single transaction.
data ProcessResult
  = Valid (BlockItem, TransactionSummary)
  | Failed FailureKind
  | Unprocessed
  deriving (Eq, Show)


runWithIntermediateStates
  :: [String]
  -> Types.SpecialBetaAccounts
  -> BlockState
  -> [TransactionJSON]
  -> PR.Context Core.UA IO [(ProcessResult, BlockState)]
runWithIntermediateStates mods specialBetaAccounts initialBlockState transactions = do
  forM_ mods $ \m -> do
    source <- liftIO $ TIO.readFile $ "test/" ++ m
    _ <- PR.processModule source -- execute only for effect on global state
    return ()
  txs <- processUngroupedTransactions transactions
  return $ reverse $ fst $
    foldl (\(acc, st) tx ->
                            let (ft@Sch.FilteredTransactions{..}, st') =
                                  Types.runSI
                                    (Sch.filterTransactions dummyBlockSize (fromTransactions [tx]))
                                    specialBetaAccounts
                                    dummyChainMeta
                                    maxBound
                                    st
                            in if not $ (length ftAdded + length ftFailed + length ftUnprocessed == 1)
                                  && (length ftFailedCredentials + length ftUnprocessedCredentials == 0)
                               then error $ "Failure in test setup: Expected one regular transaction in result, but got " ++ show ft
                               else
                                 let res
                                       | not $ null ftAdded = Valid $ head ftAdded
                                       | not $ null ftFailed = Failed $ snd $ head ftFailed
                                       | not $ null ftUnprocessed = Unprocessed
                                       | otherwise = error "Failure in test setup."
                                 in ((res, st' ^. Types.ssBlockState):acc, st' ^. Types.schedulerBlockState))
                                 -- (acc ++ [(getResults ftAdded, ftFailed, st' ^. Types.ssBlockState)], st' ^. Types.schedulerBlockState)) -- TODO check diff block state
                      ([], initialBlockState)
                      (perAccountTransactions txs)


-- | Make a 'Spec' from the given 'TestCase', running the specified transactions in sequence,
-- and checking the following:
--
-- * Invariants on the block state after each transaction.
-- * The specified 'ResultSpec' for the result of each transaction.
--
-- Note: If there are errors thrown while processing a transactions, this currently results in the
-- test suite failing without showing in which test case this happens (probably because tests are
-- executed already when constructing the test suite).
mkSpec :: TestCase -> Spec
mkSpec TestCase{..} =
  describe tcName $ do
  let (tJsons, resultSpecs) = unzip tcTransactions
  results <- runIO (PR.evalContext Init.initialContextData $
                     runWithIntermediateStates tcModules tcSpecialAccounts tcInitialBlockState tJsons)
  -- NB: This check is important to make sure that the zipped lists below have the same length and
  -- thus all 'ResultSpec's are checked.
  specify "Correct number of transactions" $
    length results `shouldBe` length tJsons
  describe "Checking results" $
    forM_ (zip3 [(1 :: Integer)..] results resultSpecs) $
      \( number
       , ( res
         , bs)
       , (resultSpec, bsSpec)
       ) -> describe ("Transaction " ++ show number) $ do
        specify "New block state satisfies invariant" $
          case invariantBlockState bs of
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
mkSpecs :: [TestCase] -> Spec
mkSpecs = mapM_ mkSpec
