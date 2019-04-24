module Main where

import qualified SchedulerTests.ContractCommSpec(tests)
import qualified SchedulerTests.SimpleTransfersTest(tests)
import qualified SchedulerTests.ChainMetatest(tests)
import qualified SchedulerTests.FibonacciTest(tests)

import Test.Hspec

main :: IO ()
main = do 
          hspec SchedulerTests.ContractCommSpec.tests
          hspec SchedulerTests.SimpleTransfersTest.tests
          hspec SchedulerTests.ChainMetatest.tests
          hspec SchedulerTests.FibonacciTest.tests
