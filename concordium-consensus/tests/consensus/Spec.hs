module Main where

import qualified ConcordiumTests.Afgjort.ABBA (tests)
import qualified ConcordiumTests.Afgjort.CSS (tests)
import qualified ConcordiumTests.Afgjort.CSS.NominationSet (tests)
import qualified ConcordiumTests.Afgjort.Freeze (tests)
import qualified ConcordiumTests.Afgjort.Lottery (tests)
import qualified ConcordiumTests.Afgjort.Types (tests)
import qualified ConcordiumTests.Afgjort.WMVBA (tests)
import qualified ConcordiumTests.CatchUp (tests)
import qualified ConcordiumTests.FinalizationRecover (test)
import qualified ConcordiumTests.Konsensus (tests)
import qualified ConcordiumTests.KonsensusV1.Consensus (tests)
import qualified ConcordiumTests.KonsensusV1.FinalizationCommittee (tests)
import qualified ConcordiumTests.KonsensusV1.LMDB (tests)
import qualified ConcordiumTests.KonsensusV1.LeaderElectionTest (tests)
import qualified ConcordiumTests.KonsensusV1.Quorum (tests)
import qualified ConcordiumTests.KonsensusV1.TransactionProcessingTest (tests)
import qualified ConcordiumTests.KonsensusV1.TreeStateTest (tests)
import qualified ConcordiumTests.KonsensusV1.Types (tests)
import qualified ConcordiumTests.LeaderElectionTest (tests)
import qualified ConcordiumTests.PassiveFinalization (test)
import qualified ConcordiumTests.ReceiveTransactionsTest (test)
import qualified ConcordiumTests.Update (test)
import Data.List (stripPrefix)
import Data.Semigroup
import System.Environment
import Test.Hspec

atLevel :: (Word -> IO ()) -> IO ()
atLevel a = do
    args0 <- getArgs
    let (args1, mlevel) = mconcat $ map lvlArg args0
    withArgs args1 $ a $! (maybe 1 getLast mlevel)
  where
    lvlArg s = case stripPrefix "--level=" s of
        Nothing -> ([s], Nothing)
        Just r -> ([], Just $! Last $! (read r :: Word))

main :: IO ()
main = atLevel $ \lvl -> hspec $ do
    ConcordiumTests.Update.test
    ConcordiumTests.Afgjort.Types.tests lvl
    ConcordiumTests.Afgjort.CSS.tests lvl
    ConcordiumTests.Afgjort.CSS.NominationSet.tests lvl
    ConcordiumTests.Afgjort.ABBA.tests lvl
    ConcordiumTests.Afgjort.Freeze.tests lvl
    ConcordiumTests.Afgjort.WMVBA.tests lvl
    ConcordiumTests.Afgjort.Lottery.tests lvl
    ConcordiumTests.Konsensus.tests lvl
    ConcordiumTests.CatchUp.tests lvl
    ConcordiumTests.FinalizationRecover.test
    ConcordiumTests.PassiveFinalization.test
    ConcordiumTests.ReceiveTransactionsTest.test
    ConcordiumTests.LeaderElectionTest.tests
    ConcordiumTests.KonsensusV1.Types.tests
    ConcordiumTests.KonsensusV1.TreeStateTest.tests
    ConcordiumTests.KonsensusV1.LMDB.tests
    ConcordiumTests.KonsensusV1.TransactionProcessingTest.tests
    ConcordiumTests.KonsensusV1.LeaderElectionTest.tests
    ConcordiumTests.KonsensusV1.FinalizationCommittee.tests
    ConcordiumTests.KonsensusV1.Consensus.tests
    ConcordiumTests.KonsensusV1.Quorum.tests
