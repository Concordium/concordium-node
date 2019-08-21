module Main where

import Test.Hspec
import qualified ConcordiumTests.Afgjort.Freeze (tests)
import qualified ConcordiumTests.Afgjort.CSS.NominationSet (tests)
import qualified ConcordiumTests.Afgjort.CSS (tests)
import qualified ConcordiumTests.Afgjort.Lottery (tests)
import qualified ConcordiumTests.Afgjort.ABBA (tests)
import qualified ConcordiumTests.Konsensus (tests)

main :: IO ()
main = hspec $ do
    ConcordiumTests.Afgjort.CSS.tests
    ConcordiumTests.Afgjort.CSS.NominationSet.tests
    ConcordiumTests.Afgjort.ABBA.tests
    ConcordiumTests.Afgjort.Freeze.tests
    ConcordiumTests.Afgjort.Lottery.tests
    ConcordiumTests.Konsensus.tests
