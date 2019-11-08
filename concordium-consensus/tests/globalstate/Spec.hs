module Main where

import Test.Hspec
import qualified GlobalStateTests.Instances(tests)
import qualified GlobalStateTests.FinalizationSerializationSpec(tests)
import qualified GlobalStateTests.Trie(tests)
import qualified GlobalStateTests.PersistentState(tests)

main :: IO ()
main = hspec $ do
  GlobalStateTests.Trie.tests
  GlobalStateTests.PersistentState.tests
  GlobalStateTests.FinalizationSerializationSpec.tests
  GlobalStateTests.Instances.tests

