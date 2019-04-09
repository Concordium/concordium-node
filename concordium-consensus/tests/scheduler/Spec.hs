module Main where

import qualified AcornTests.ContractCommSpec(tests)
import qualified AcornTests.SimpleTransfersTest(tests)
import qualified AcornTests.ChainMetatest(tests)
import qualified AcornTests.FibonacciTest(tests)

import Test.Hspec.QuickCheck

import Test.Hspec

main = do 
          hspec AcornTests.ContractCommSpec.tests
          hspec AcornTests.SimpleTransfersTest.tests
          hspec AcornTests.ChainMetatest.tests
          hspec AcornTests.FibonacciTest.tests
