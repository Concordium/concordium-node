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
                                TestParameters(..),defaultParams, mkSpec,mkSpecs) where

import Test.Hspec

import Lens.Micro.Platform
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import System.FilePath

import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Environment as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner

import qualified Acorn.Core as Core
import qualified Acorn.Utils.Init as Init
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.DummyData

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


data TestParameters = TestParameters
  { tpChainMeta :: ChainMetadata
    -- | The blockstate to start from.
  , tpInitialBlockState :: BlockState
    -- | The 'Types.SpecialBetaAccounts' to run with.
  , tpSpecialAccounts :: Types.SpecialBetaAccounts
    -- | Limit on the total energy the processed transactions can use.
  , tpEnergyLimit :: Energy
    -- | Limit on the total size of the processed transactions.
  , tpSizeLimit :: Integer
  }

defaultParams :: TestParameters
defaultParams = TestParameters
  { tpChainMeta = dummyChainMeta
  , tpInitialBlockState = createBlockState Acc.emptyAccounts
  , tpSpecialAccounts = Types.emptySpecialBetaAccounts
  , tpEnergyLimit = maxBound
  , tpSizeLimit = fromIntegral $ (maxBound :: Int)
  }

-- | A test case for executing a list of transactions, specifying 'ResultSpec's for the result
-- of each transaction's execution. The transactions are run with 'Sch.filterTransactions'
-- in sequenced and not grouped, with the given parameters.
data TestCase = TestCase
  { -- | A name for the test case, which is printed.
    tcName :: String
    -- | Parameters for executing the transactions.
  , tcParameters :: TestParameters
    -- | Modules within in the test/ directory to be available for deployment.
  , tcModules :: [FilePath]
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


-- | Execute the given transactions in sequence (ungrouped) with 'Sch.filterTransactions',
-- with the given parameters. Returns a list of result and block state after each transaction.
runWithIntermediateStates
  :: [FilePath] -- ^ Modules from test/ directory to be available for deployment.
  -> TestParameters
  -> [TransactionJSON]
  -> PR.Context Core.UA IO [(ProcessResult, BlockState)]
runWithIntermediateStates mods TestParameters{..} transactions = do
  forM_ mods $ \m -> do
    let file = "test/" </> m
    source <- liftIO $ TIO.readFile $ file
    _ <- PR.processModule file source -- execute only for effect on global state
    return ()
  -- Create actual 'Transaction's from the 'TransactionJSON'.
  txs <- processUngroupedTransactions transactions
  return $ reverse $ fst $
    foldl (\(acc, st) tx ->
                            let (ft@Sch.FilteredTransactions{..}, st') =
                                  Types.runSI
                                    (Sch.filterTransactions tpSizeLimit (fromTransactions [tx]))
                                    tpSpecialAccounts
                                    tpChainMeta
                                    tpEnergyLimit
                                    st
                            in if length ftAdded + length ftFailed + length ftUnprocessed == 1
                                  && (length ftFailedCredentials + length ftUnprocessedCredentials == 0)
                               then
                                 let res
                                       | not $ null ftAdded = Valid $ head ftAdded
                                       | not $ null ftFailed = Failed $ snd $ head ftFailed
                                       | not $ null ftUnprocessed = Unprocessed
                                       | otherwise = error "Failure in test setup."
                                 in ((res, st' ^. Types.ssBlockState):acc, st' ^. Types.schedulerBlockState)
                               else error $ "Failure in test setup: Expected one regular transaction in result, but got " ++ show ft
            )
                      ([], tpInitialBlockState)
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
mkSpec :: TestCase -> Spec
mkSpec TestCase{..} =
  describe tcName $ do
  let (tJsons, resultSpecs) = unzip tcTransactions
  results <- runIO (PR.evalContext Init.initialContextData $
                     runWithIntermediateStates tcModules tcParameters tJsons)
  -- NB: This check is important to make sure that the zipped lists below have the same length and
  -- thus all 'ResultSpec's are checked.
  specify "Correct number of transactions" $
    length results `shouldBe` length tJsons
  describe "Checking results" $
    forM_ (zip3 [(1 :: Integer)..] results resultSpecs) $
      \( number
       , (res, bs)
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
