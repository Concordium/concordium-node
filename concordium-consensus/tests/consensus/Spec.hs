module Main where

import Test.Hspec
import Test.QuickCheck
import qualified ConcordiumTests.Crypto.DummySignature (tests)
import qualified ConcordiumTests.Crypto.DummyVRF (tests)
import qualified ConcordiumTests.Afgjort.Freeze (tests)

main :: IO ()
main = hspec $ do
    ConcordiumTests.Crypto.DummySignature.tests
    ConcordiumTests.Crypto.DummyVRF.tests
    ConcordiumTests.Afgjort.Freeze.tests
