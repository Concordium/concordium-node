module Main where

import Data.List (stripPrefix)
import Data.Semigroup
import qualified GlobalStateTests.AccountMap (tests)
import qualified GlobalStateTests.AccountReleaseScheduleMigration (tests)
import qualified GlobalStateTests.AccountReleaseScheduleTest (tests)
import qualified GlobalStateTests.Accounts (tests)
import qualified GlobalStateTests.AccountsMigrationP6ToP7 (tests)
import qualified GlobalStateTests.BlobStore (tests)
import qualified GlobalStateTests.BlockHash (tests)
import qualified GlobalStateTests.Cache (tests)
import qualified GlobalStateTests.DifferenceMap (tests)
import qualified GlobalStateTests.EnduringDataFlags (tests)
import qualified GlobalStateTests.FinalizationSerializationSpec (tests)
import qualified GlobalStateTests.Instances (tests)
import qualified GlobalStateTests.LFMBTree (tests)
import qualified GlobalStateTests.LMDBAccountMap (tests)
import qualified GlobalStateTests.PersistentTreeState (tests)
import qualified GlobalStateTests.Trie (tests)
import qualified GlobalStateTests.UpdateQueues (tests)
import qualified GlobalStateTests.Updates (tests)
import System.Environment
import Test.Hspec

atLevel :: (Word -> IO ()) -> IO ()
atLevel a = do
    args0 <- getArgs
    let (args1, mlevel) = mconcat $ map lvlArg args0
    withArgs args1 $ a $! maybe 1 getLast mlevel
  where
    lvlArg s = case stripPrefix "--level=" s of
        Nothing -> ([s], Nothing)
        Just r -> ([], Just $! Last $! (read r :: Word))

main :: IO ()
main = atLevel $ \lvl -> hspec $ do
    GlobalStateTests.BlockHash.tests
    GlobalStateTests.Cache.tests
    GlobalStateTests.LFMBTree.tests
    GlobalStateTests.Accounts.tests lvl
    GlobalStateTests.Trie.tests
    GlobalStateTests.PersistentTreeState.tests
    GlobalStateTests.FinalizationSerializationSpec.tests
    GlobalStateTests.Instances.tests lvl
    GlobalStateTests.AccountReleaseScheduleTest.tests
    GlobalStateTests.AccountReleaseScheduleMigration.tests
    GlobalStateTests.Updates.tests
    GlobalStateTests.AccountMap.tests lvl
    GlobalStateTests.EnduringDataFlags.tests
    GlobalStateTests.BlobStore.tests
    GlobalStateTests.UpdateQueues.tests
    GlobalStateTests.LMDBAccountMap.tests
    GlobalStateTests.DifferenceMap.tests
    GlobalStateTests.AccountsMigrationP6ToP7.tests
