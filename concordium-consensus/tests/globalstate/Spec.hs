module Main where

import Test.Hspec
import qualified GlobalStateTests.SerializationSpec(tests)
import qualified GlobalStateTests.Instances(tests)

main :: IO ()
main = hspec $ do
  GlobalStateTests.SerializationSpec.tests
  GlobalStateTests.Instances.tests

