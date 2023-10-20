{-# LANGUAGE BangPatterns #-}

-- | A module that tests the functionality of the 'DiffMap.DifferenceMap'.
--  * Insert and lookup operations.
--  * Flattening the 'DiffMap.DifferenceMap'.
module GlobalStateTests.DifferenceMap where

import Concordium.ID.Types (randomAccountAddress)
import Concordium.Types
import System.Random
import Control.Monad (when)
import qualified Data.Map.Strict as Map

import qualified Concordium.GlobalState.AccountMap.DifferenceMap as DiffMap

import Test.HUnit
import Test.Hspec

-- | Create a pair consisting of an account address and an account index based on the provided seed.
dummyPair :: Int -> (AccountAddress, AccountIndex)
dummyPair seed = (fst $ randomAccountAddress (mkStdGen seed), AccountIndex $ fromIntegral seed)

-- | Test that an account can be inserted and looked up in the 'DiffMap.DifferenceMap'.
testInsertLookupAccount :: Assertion
testInsertLookupAccount = do
    let diffMap = uncurry DiffMap.insert acc $ DiffMap.empty Nothing
    case DiffMap.lookup (fst acc) diffMap of
        Nothing -> assertFailure "account should be present in diff map"
        Just accIdx -> assertEqual "account should be there" (snd acc) accIdx
  where
    acc = dummyPair 1

testLookups :: Assertion
testLookups = do
    let diffMap1 = uncurry DiffMap.insert (dummyPair 1) $ DiffMap.empty Nothing
        diffMap2 = uncurry DiffMap.insert (dummyPair 2) (DiffMap.empty $ Just diffMap1)
        diffMap3 = uncurry DiffMap.insert (dummyPair 3) (DiffMap.empty $ Just diffMap2)
    checkExists (dummyPair 1) diffMap1
    checkExists (dummyPair 1) diffMap2
    checkExists (dummyPair 2) diffMap2
    checkExists (dummyPair 1) diffMap3
    checkExists (dummyPair 2) diffMap3
    checkExists (dummyPair 3) diffMap3
  where
    checkExists pair diffMap = 
        case DiffMap.lookup (fst pair) diffMap of
            Nothing -> assertFailure "account should be present"
            Just accIdx -> assertEqual "wrong account index" (snd pair) accIdx

testFlatten :: Assertion
testFlatten = do
    let diffMap1 = uncurry DiffMap.insert (dummyPair 1) $ DiffMap.empty Nothing
        diffMap2 = uncurry DiffMap.insert (dummyPair 2) (DiffMap.empty $ Just diffMap1)
        diffMap3 = uncurry DiffMap.insert (dummyPair 3) (DiffMap.empty $ Just diffMap2)
    assertEqual "accounts should be the same" (Map.fromList (map dummyPair [1..3])) $ DiffMap.flatten diffMap3

tests :: Spec
tests = describe "AccountMap.DifferenceMap" $ do
    it "Test insert and lookup account" testInsertLookupAccount
    it "test lookups" testLookups
    it "Test flatten" testFlatten
