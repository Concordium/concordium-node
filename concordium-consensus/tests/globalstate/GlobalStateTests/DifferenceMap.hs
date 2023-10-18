{-# LANGUAGE BangPatterns #-}

-- | A module that tests the functionality of the 'DiffMap.DifferenceMap'.
--  * Insert and lookup operations.
--  * Flattening the 'DiffMap.DifferenceMap'.
module GlobalStateTests.DifferenceMap where

import Concordium.ID.Types (randomAccountAddress)
import Concordium.Types
import System.Random

import qualified Concordium.GlobalState.AccountMap.DifferenceMap as DiffMap

import Test.HUnit
import Test.Hspec
import Test.QuickCheck

-- | Create a pair consisting of an account address and an account index based on the provided seed.
dummyPair :: Int -> (AccountAddress, AccountIndex)
dummyPair seed = (fst $ randomAccountAddress (mkStdGen seed), AccountIndex $ fromIntegral seed)

-- | Test that accounts can be inserted and looked up in the 'DiffMap.DifferenceMap'.
testInsertAccount :: Assertion
testInsertAccount = do
    let diffMap = DiffMap.insert (fst acc) (snd acc) $ DiffMap.empty Nothing
    case DiffMap.lookup (fst acc) diffMap of
        Nothing -> assertFailure "account should be present in diff map"
        Just accIdx -> assertEqual "account should be there" (snd acc) accIdx
  where
    acc = dummyPair 1

-- | Test for getting all accounts in a 'DiffMap.DifferenceMap'.
testInsertAccountsAndRetrieveAll :: Assertion
testInsertAccountsAndRetrieveAll = do
    let allAccounts = DiffMap.flatten $ mkDiffMaps 42
    if length allAccounts /= 42 * 43
        then assertFailure $ "Unexpected number of accounts in difference maps: " <> show (length allAccounts)
        else pure ()
  where
    -- create a difference map with n parents and n accounts at each layer, so n+1 difference maps in total.
    mkDiffMaps n = go n n $ DiffMap.empty Nothing
      where
        go 0 _ accum = accum
        go childCount numAccounts !accum =
            let dmAccounts = [pair | pair <- dummyPair <$> [0 .. numAccounts]]
                dmParentMap = Just $ go (childCount - 1) numAccounts $ DiffMap.empty $ Just accum
            in  DiffMap.DifferenceMap{..}

tests :: Spec
tests = describe "AccountMap.DifferenceMap" $ do
    it "Test insert account" testInsertAccount
    it "Test retrieve all accounts from difference maps" testInsertAccountsAndRetrieveAll
