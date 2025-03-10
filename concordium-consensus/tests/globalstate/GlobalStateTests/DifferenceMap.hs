{-# LANGUAGE BangPatterns #-}

-- | A module that tests the functionality of the 'DiffMap.AccountDifferenceMap'.
--  * Insert and lookup operations.
--  * Flattening the 'DiffMap.AccountDifferenceMap'.
module GlobalStateTests.DifferenceMap where

import Concordium.ID.Types (accountAddressSize, randomAccountAddress)
import Concordium.Types
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.FixedByteString as FBS
import qualified Data.HashMap.Strict as HM
import Data.IORef
import System.Random
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.GlobalState.AccountMap.DifferenceMap as DiffMap
import Concordium.Types.Option

-- | Create a pair consisting of an account address and an account index based on the provided seed.
dummyPair :: Int -> (AccountAddress, AccountIndex)
dummyPair seed = (fst $ randomAccountAddress (mkStdGen seed), AccountIndex $ fromIntegral seed)

-- | Test for looking up both via equivalence class and by exactness.
--  Precondition: The provided @AccountAddress@ MUST be the canonical address,
--  and it should be present in the underlying store.
--  The equivalence lookup always looks up by an alias.
testDoLookup :: (MonadIO m) => AccountAddress -> DiffMap.AccountDifferenceMap -> m (Either Int AccountIndex)
testDoLookup accAddr diffMap = do
    res1 <- DiffMap.lookupAccountEquiv (accountAddressEmbed $ createAlias accAddr 42) diffMap
    res2 <- DiffMap.lookupAccountExact accAddr diffMap
    liftIO $ assertEqual "results should be the same" res1 res2
    return $ left fromIntegral res1

-- | Test that an account can be inserted and looked up in the 'DiffMap.AccountDifferenceMap'.
testInsertLookupAccount :: Assertion
testInsertLookupAccount = do
    emptyParentMap <- liftIO DiffMap.newEmptyReference
    let diffMap = uncurry DiffMap.insertFreshAccount acc $ DiffMap.empty emptyParentMap
    testDoLookup (fst acc) diffMap >>= \case
        Left _ -> assertFailure "account should be present in diff map"
        Right accIdx -> assertEqual "account should be there" (snd acc) accIdx
  where
    acc = dummyPair 1

-- | Create a parent pointer for the provided difference map.
mkParentPointer :: Option DiffMap.AccountDifferenceMap -> IO (IORef (Option DiffMap.AccountDifferenceMap))
mkParentPointer diffMap = newIORef diffMap >>= return

-- | Testing lookups in flat and nested difference maps.
testLookups :: Assertion
testLookups = do
    emptyParentMap <- liftIO DiffMap.newEmptyReference
    let diffMap1 = uncurry DiffMap.insertFreshAccount (dummyPair 1) $ DiffMap.empty emptyParentMap
    diffMap1Pointer <- mkParentPointer $ Present diffMap1
    let diffMap2 = uncurry DiffMap.insertFreshAccount (dummyPair 2) (DiffMap.empty diffMap1Pointer)
    diffMap2Pointer <- mkParentPointer $ Present diffMap2
    let diffMap3 = uncurry DiffMap.insertFreshAccount (dummyPair 3) (DiffMap.empty diffMap2Pointer)
    checkExists (dummyPair 1) diffMap1
    checkExists (dummyPair 1) diffMap2
    checkExists (dummyPair 2) diffMap2
    checkExists (dummyPair 1) diffMap3
    checkExists (dummyPair 2) diffMap3
    checkExists (dummyPair 3) diffMap3
  where
    checkExists pair diffMap =
        testDoLookup (fst pair) diffMap >>= \case
            Left _ -> assertFailure "account should be present"
            Right accIdx -> assertEqual "wrong account index" (snd pair) accIdx

-- | Test flattening a difference map i.e. return all accounts as one flat map.
testFlatten :: Assertion
testFlatten = do
    emptyParentMap <- liftIO DiffMap.newEmptyReference
    let diffMap1 = uncurry DiffMap.insertFreshAccount (dummyPair 1) $ DiffMap.empty emptyParentMap
    diffMap1Pointer <- mkParentPointer $ Present diffMap1
    let diffMap2 = uncurry DiffMap.insertFreshAccount (dummyPair 2) (DiffMap.empty diffMap1Pointer)
    diffMap2Pointer <- mkParentPointer $ Present diffMap2
    let diffMap3 = uncurry DiffMap.insertFreshAccount (dummyPair 3) (DiffMap.empty diffMap2Pointer)
    assertEqual "accounts should be the same" (map dummyPair [1 .. 3]) =<< DiffMap.flattenAccounts diffMap3

-- | Make the reference map for comparing lookups.
makeReference :: [(AccountAddress, AccountIndex)] -> HM.HashMap AccountAddress AccountIndex
makeReference = HM.fromList

-- | Generate an 'AccountAddress'
genAccountAddress :: Gen AccountAddress
genAccountAddress = AccountAddress . FBS.pack <$> vector accountAddressSize

-- | Generate account addresses, account indices and depth of the difference map.
genInputs :: Gen ([(AccountAddress, AccountIndex)], Int)
genInputs = sized $ \n -> do
    let maxAccs = min n 10000
    len <- choose (0, maxAccs)
    accs <- replicateM len ((,) <$> genAccountAddress <*> (AccountIndex <$> arbitrary))
    noDifferenceMaps <- choose (0, len)
    return (accs, noDifferenceMaps)

genInputs2 :: Gen [(AccountAddress, AccountIndex)]
genInputs2 = sized $ \n -> do
    let maxAccs = min n 10000
    len <- choose (0, maxAccs)
    replicateM len ((,) <$> genAccountAddress <*> (AccountIndex <$> arbitrary))

-- | Test insertions and lookups on the difference map.
insertionsAndLookups :: Spec
insertionsAndLookups = it "insertions and lookups" $
    withMaxSuccess 10000 $
        forAll genInputs $ \(inputs, noDifferenceMaps) -> do
            let reference = HM.fromList inputs
            emptyRef <- liftIO DiffMap.newEmptyReference
            diffMap <- populateDiffMap inputs noDifferenceMaps $ DiffMap.empty emptyRef
            checkAll reference diffMap
            let nonExistantAcc = fst (dummyPair (-1))
            testDoLookup nonExistantAcc diffMap >>= \case
                Right _ -> liftIO $ assertFailure "account should not be present"
                Left size -> do
                    expectedSize <- length <$> DiffMap.flatten diffMap
                    liftIO $ assertEqual "Sizes should match" expectedSize size
  where
    checkAll ref diffMap = forM_ (HM.toList ref) (check diffMap)
    check diffMap (accAddr, accIdx) = do
        testDoLookup accAddr diffMap >>= \case
            Left _ -> liftIO $ assertFailure "account address should be present"
            Right actualAccIdx -> liftIO $ assertEqual "account index should be equal" accIdx actualAccIdx
    -- return the generated difference map(s)
    populateDiffMap [] _ !accum = return accum
    -- dump any remaining accounts at the top most difference map.
    populateDiffMap ((accAddr, accIdx) : rest) 0 !accum = populateDiffMap rest 0 $ DiffMap.insertFreshAccount accAddr accIdx accum
    -- create a new layer and insert an account.
    populateDiffMap ((accAddr, accIdx) : rest) remaining !accum = do
        pRef <- mkParentPointer (Present accum)
        let accumDiffMap'' = DiffMap.insertFreshAccount accAddr accIdx $ DiffMap.empty pRef
        populateDiffMap rest (remaining - 1) accumDiffMap''

-- | A test that makes sure if multiple difference maps are
--  derivied via a common parent, then additions in one branch
--  are not propagating to other branches.
testMultipleChildrenDifferenceMaps :: Assertion
testMultipleChildrenDifferenceMaps = do
    emptyRoot <- liftIO DiffMap.newEmptyReference
    -- The common parent
    let parent = uncurry DiffMap.insertFreshAccount (dummyPair 1) $ DiffMap.empty emptyRoot
    parentReference <- mkParentPointer $ Present parent
    -- First branch
    let branch0 = uncurry DiffMap.insertFreshAccount (dummyPair 2) $ DiffMap.empty parentReference
    -- Second branch
    let branch1 = uncurry DiffMap.insertFreshAccount (dummyPair 3) $ DiffMap.empty parentReference

    -- Account from common parent should exist in both branches.
    checkExists (fst $ dummyPair 1) (snd $ dummyPair 1) branch0
    checkExists (fst $ dummyPair 1) (snd $ dummyPair 1) branch1
    -- Check that we cannot lookup elements from a different branch.
    checkNotExists (fst $ dummyPair 2) branch1
    checkNotExists (fst $ dummyPair 3) branch0
  where
    checkExists addr expectedAccIdx diffMap =
        testDoLookup addr diffMap >>= \case
            Right accIdx -> liftIO $ assertEqual "Account index should match" expectedAccIdx accIdx
            Left _ -> liftIO $ assertFailure "Expected an entry"
    checkNotExists addr diffMap =
        testDoLookup addr diffMap >>= \case
            Right _ -> liftIO $ assertFailure "Did not expect an entry"
            Left size -> do
                expectedSize <- length <$> DiffMap.flatten diffMap
                liftIO $ assertEqual "Size reported back should match flattened size" expectedSize size

-- | Test the 'fromList' function.
testFromList :: Assertion
testFromList = do
    emptyRoot <- liftIO DiffMap.newEmptyReference
    -- check creating from empty list
    let emptyDiffMap = DiffMap.empty emptyRoot
    liftIO $ assertBool "fromList on empty list should yield the empty difference map" (emptyDiffMap == DiffMap.fromAccountList emptyRoot [])
    -- check for a difference map with 1 element.
    let nonEmptyDiffMap = uncurry DiffMap.insertFreshAccount (dummyPair 1) $ DiffMap.empty emptyRoot
    liftIO $ assertBool "fromList on empty list should yield the empty difference map" (nonEmptyDiffMap == DiffMap.fromAccountList emptyRoot [dummyPair 1])

tests :: Spec
tests = describe "AccountMap.DifferenceMap" $ do
    it "Test insert and lookup account" testInsertLookupAccount
    it "test lookups" testLookups
    it "Test flatten" testFlatten
    it "test lookups on branches" testMultipleChildrenDifferenceMaps
    it "test fromList" testFromList
    insertionsAndLookups
