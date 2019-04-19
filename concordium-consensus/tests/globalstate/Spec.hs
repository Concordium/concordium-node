module Main where

import Test.Hspec
import qualified GlobalStateTests.SerializationSpec(tests)

main :: IO ()
main = hspec $ do
  GlobalStateTests.SerializationSpec.tests

