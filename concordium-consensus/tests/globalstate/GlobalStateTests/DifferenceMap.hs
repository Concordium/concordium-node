{-# LANGUAGE BangPatterns #-}

-- | A module that tests the functionality of the 'DiffMap.DifferenceMap'.
--  * Insert and lookup operations.
--  * Flattening the 'DiffMap.DifferenceMap'.
module GlobalStateTests.DifferenceMap where

import Concordium.ID.Types (randomAccountAddress)
import Concordium.Types
import Data.IORef
import System.Random

import qualified Concordium.GlobalState.AccountMap.DifferenceMap as DiffMap

import Test.HUnit
import Test.Hspec

-- | Create a pair consisting of an account address and an account index based on the provided seed.
dummyPair :: Int -> (AccountAddress, AccountIndex)
dummyPair seed = (fst $ randomAccountAddress (mkStdGen seed), AccountIndex $ fromIntegral seed)

-- | Test that an account can be inserted and looked up in the 'DiffMap.DifferenceMap'.
testInsertLookupAccount :: Assertion
testInsertLookupAccount = do
    emptyParentMap <- mkParentPointer Nothing
    let diffMap = uncurry DiffMap.insert acc $ DiffMap.empty emptyParentMap
    DiffMap.lookup (fst acc) diffMap >>= \case
        Nothing -> assertFailure "account should be present in diff map"
        Just accIdx -> assertEqual "account should be there" (snd acc) accIdx
  where
    acc = dummyPair 1

-- | Create a parent pointer for the provided difference map.
mkParentPointer :: Maybe DiffMap.DifferenceMap -> IO (IORef (Maybe DiffMap.DifferenceMap))
mkParentPointer diffMap = newIORef diffMap >>= return

-- | Testing lookups in flat and nested difference maps.
testLookups :: Assertion
testLookups = do
    emptyParentMap <- mkParentPointer Nothing
    let diffMap1 = uncurry DiffMap.insert (dummyPair 1) $ DiffMap.empty emptyParentMap
    diffMap1Pointer <- mkParentPointer $ Just diffMap1
    let diffMap2 = uncurry DiffMap.insert (dummyPair 2) (DiffMap.empty diffMap1Pointer)
    diffMap2Pointer <- mkParentPointer $ Just diffMap2
    let diffMap3 = uncurry DiffMap.insert (dummyPair 3) (DiffMap.empty diffMap2Pointer)
    checkExists (dummyPair 1) diffMap1
    checkExists (dummyPair 1) diffMap2
    checkExists (dummyPair 2) diffMap2
    checkExists (dummyPair 1) diffMap3
    checkExists (dummyPair 2) diffMap3
    checkExists (dummyPair 3) diffMap3
  where
    checkExists pair diffMap =
        DiffMap.lookup (fst pair) diffMap >>= \case
            Nothing -> assertFailure "account should be present"
            Just accIdx -> assertEqual "wrong account index" (snd pair) accIdx

-- | Test flattening a difference map i.e. return all accounts as one flat map.
testFlatten :: Assertion
testFlatten = do
    emptyParentMap <- mkParentPointer Nothing
    let diffMap1 = uncurry DiffMap.insert (dummyPair 1) $ DiffMap.empty emptyParentMap
    diffMap1Pointer <- mkParentPointer $ Just diffMap1
    let diffMap2 = uncurry DiffMap.insert (dummyPair 2) (DiffMap.empty diffMap1Pointer)
    diffMap2Pointer <- mkParentPointer $ Just diffMap2
    let diffMap3 = uncurry DiffMap.insert (dummyPair 3) (DiffMap.empty diffMap2Pointer)
    assertEqual "accounts should be the same" (map dummyPair [1 .. 3]) =<< DiffMap.flatten diffMap3

tests :: Spec
tests = describe "AccountMap.DifferenceMap" $ do
    it "Test insert and lookup account" testInsertLookupAccount
    it "test lookups" testLookups
    it "Test flatten" testFlatten
