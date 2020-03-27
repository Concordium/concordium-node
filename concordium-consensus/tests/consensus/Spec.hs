module Main where

import System.Environment
import Data.Semigroup
import Data.List
import Test.Hspec
import qualified ConcordiumTests.Afgjort.Freeze (tests)
import qualified ConcordiumTests.Afgjort.CSS.NominationSet (tests)
import qualified ConcordiumTests.Afgjort.CSS (tests)
import qualified ConcordiumTests.Afgjort.Lottery (tests)
import qualified ConcordiumTests.Afgjort.ABBA (tests)
import qualified ConcordiumTests.Afgjort.WMVBA (tests)
import qualified ConcordiumTests.Afgjort.Types (tests)
import qualified ConcordiumTests.Konsensus (tests)
import qualified ConcordiumTests.CatchUp (tests)
import qualified ConcordiumTests.PassiveFinalization (tests)

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
    ConcordiumTests.Afgjort.Types.tests lvl
    ConcordiumTests.Afgjort.CSS.tests lvl
    ConcordiumTests.Afgjort.CSS.NominationSet.tests lvl
    ConcordiumTests.Afgjort.ABBA.tests lvl
    ConcordiumTests.Afgjort.Freeze.tests lvl
    ConcordiumTests.Afgjort.WMVBA.tests lvl
    ConcordiumTests.Afgjort.Lottery.tests lvl
    ConcordiumTests.Konsensus.tests lvl
    ConcordiumTests.CatchUp.tests lvl
    ConcordiumTests.PassiveFinalization.tests lvl
