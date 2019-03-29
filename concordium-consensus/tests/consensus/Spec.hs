module Main where

import Test.Hspec
import qualified ConcordiumTests.Afgjort.Freeze (tests)
import qualified ConcordiumTests.Afgjort.CSS (tests)
import qualified ConcordiumTests.Afgjort.Lottery (tests)
import qualified ConcordiumTests.Afgjort.ABBA (tests)

main :: IO ()
main = hspec $ do
    ConcordiumTests.Afgjort.ABBA.tests
    ConcordiumTests.Afgjort.Freeze.tests
    ConcordiumTests.Afgjort.CSS.tests
    ConcordiumTests.Afgjort.Lottery.tests