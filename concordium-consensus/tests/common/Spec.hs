module Main where

import Test.Hspec
import qualified CommonTests.CachingTest(tests)
import qualified CommonTests.VerifyCredentialDeploymentTest(tests)

main :: IO ()
main = hspec $ do
  CommonTests.CachingTest.tests
  CommonTests.VerifyCredentialDeploymentTest.tests
