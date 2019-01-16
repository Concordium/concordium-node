module Main where

import Test.Hspec
import qualified ConcordiumTests.Crypto.DummySignature (tests)
import qualified ConcordiumTests.Crypto.DummyVRF (tests)
import qualified ConcordiumTests.Afgjort.Freeze (tests)
import qualified ConcordiumTests.Afgjort.CSS (tests)

main :: IO ()
main = hspec $ do
    ConcordiumTests.Crypto.DummySignature.tests
    ConcordiumTests.Crypto.DummyVRF.tests
    ConcordiumTests.Afgjort.Freeze.tests
    ConcordiumTests.Afgjort.CSS.tests