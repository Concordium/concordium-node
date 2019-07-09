module Main where

import Test.Hspec
import qualified GlobalStateTests.SerializationSpec(tests)
import qualified GlobalStateTests.Instances(tests)
import qualified GlobalStateTests.FinalizationSerializationSpec(tests)

main :: IO ()
main = hspec $ do
  GlobalStateTests.SerializationSpec.tests
  GlobalStateTests.FinalizationSerializationSpec.tests
  GlobalStateTests.Instances.tests

