module Main where

import Test.Hspec
import qualified GlobalStateTests.Instances(tests)
import qualified GlobalStateTests.FinalizationSerializationSpec(tests)
import qualified GlobalStateTests.Trie(tests)
import qualified GlobalStateTests.PersistentTreeState(tests)
import qualified GlobalStateTests.Accounts(tests)

main :: IO ()
main = hspec $ do
  GlobalStateTests.Accounts.tests
  GlobalStateTests.Trie.tests
  GlobalStateTests.PersistentTreeState.tests
  GlobalStateTests.FinalizationSerializationSpec.tests
  GlobalStateTests.Instances.tests
