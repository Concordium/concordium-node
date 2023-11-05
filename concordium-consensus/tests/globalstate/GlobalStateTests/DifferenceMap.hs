{-# LANGUAGE BangPatterns #-}

-- | A module that tests the functionality of the 'DiffMap.DifferenceMap'.
--  * Insert and lookup operations.
--  * Flattening the 'DiffMap.DifferenceMap'.
module GlobalStateTests.DifferenceMap where

import Concordium.ID.Types (accountAddressSize, randomAccountAddress)
import Concordium.Types
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
testDoLookup :: (MonadIO m) => AccountAddress -> DiffMap.DifferenceMap -> m (Maybe AccountIndex)
testDoLookup accAddr diffMap = do
    res1 <- DiffMap.lookupViaEquivalenceClass (accountAddressEmbed $ createAlias accAddr 42) diffMap
    res2 <- DiffMap.lookupExact accAddr diffMap
    liftIO $ assertEqual "results should be the same" res1 res2
    return res1

-- | Test that an account can be inserted and looked up in the 'DiffMap.DifferenceMap'.
testInsertLookupAccount :: Assertion
testInsertLookupAccount = do
    emptyParentMap <- mkParentPointer Absent
    let diffMap = uncurry DiffMap.insert acc $ DiffMap.empty emptyParentMap
    testDoLookup (fst acc) diffMap >>= \case
        Nothing -> assertFailure "account should be present in diff map"
        Just accIdx -> assertEqual "account should be there" (snd acc) accIdx
  where
    acc = dummyPair 1

-- | Create a parent pointer for the provided difference map.
mkParentPointer :: Option DiffMap.DifferenceMap -> IO (IORef (Option DiffMap.DifferenceMap))
mkParentPointer diffMap = newIORef diffMap >>= return

-- | Testing lookups in flat and nested difference maps.
testLookups :: Assertion
testLookups = do
    emptyParentMap <- mkParentPointer Absent
    let diffMap1 = uncurry DiffMap.insert (dummyPair 1) $ DiffMap.empty emptyParentMap
    diffMap1Pointer <- mkParentPointer $ Present diffMap1
    let diffMap2 = uncurry DiffMap.insert (dummyPair 2) (DiffMap.empty diffMap1Pointer)
    diffMap2Pointer <- mkParentPointer $ Present diffMap2
    let diffMap3 = uncurry DiffMap.insert (dummyPair 3) (DiffMap.empty diffMap2Pointer)
    checkExists (dummyPair 1) diffMap1
    checkExists (dummyPair 1) diffMap2
    checkExists (dummyPair 2) diffMap2
    checkExists (dummyPair 1) diffMap3
    checkExists (dummyPair 2) diffMap3
    checkExists (dummyPair 3) diffMap3
  where
    checkExists pair diffMap =
        testDoLookup (fst pair) diffMap >>= \case
            Nothing -> assertFailure "account should be present"
            Just accIdx -> assertEqual "wrong account index" (snd pair) accIdx

-- | Test flattening a difference map i.e. return all accounts as one flat map.
testFlatten :: Assertion
testFlatten = do
    emptyParentMap <- mkParentPointer Absent
    let diffMap1 = uncurry DiffMap.insert (dummyPair 1) $ DiffMap.empty emptyParentMap
    diffMap1Pointer <- mkParentPointer $ Present diffMap1
    let diffMap2 = uncurry DiffMap.insert (dummyPair 2) (DiffMap.empty diffMap1Pointer)
    diffMap2Pointer <- mkParentPointer $ Present diffMap2
    let diffMap3 = uncurry DiffMap.insert (dummyPair 3) (DiffMap.empty diffMap2Pointer)
    assertEqual "accounts should be the same" (map dummyPair [1 .. 3]) =<< DiffMap.flatten diffMap3

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

-- | Test insertions and lookups on the difference map.
insertionsAndLookups :: Spec
insertionsAndLookups = it "insertions and lookups" $
    withMaxSuccess 10000 $
        forAll genInputs $ \(inputs, noDifferenceMaps) -> do
            let reference = HM.fromList inputs
            emptyRef <- mkParentPointer Absent
            diffMap <- populateDiffMap inputs noDifferenceMaps $ DiffMap.empty emptyRef
            checkAll reference diffMap
  where
    checkAll ref diffMap = forM_ (HM.toList ref) (check diffMap)
    check diffMap (accAddr, accIdx) = do
        testDoLookup accAddr diffMap >>= \case
            Nothing -> liftIO $ assertFailure "account address should be present"
            Just actualAccIdx -> liftIO $ assertEqual "account index should be equal" accIdx actualAccIdx
    -- return the generated difference map(s)
    populateDiffMap [] _ !accum = return accum
    -- dump any remaining accounts at the top most difference map.
    populateDiffMap ((accAddr, accIdx) : rest) 0 !accum = populateDiffMap rest 0 $ DiffMap.insert accAddr accIdx accum
    -- create a new layer and insert an account.
    populateDiffMap ((accAddr, accIdx) : rest) remaining !accum = do
        pRef <- mkParentPointer (Present accum)
        let accumDiffMap'' = DiffMap.insert accAddr accIdx $ DiffMap.empty pRef
        populateDiffMap rest (remaining - 1) accumDiffMap''

tests :: Spec
tests = describe "AccountMap.DifferenceMap" $ do
    it "Test insert and lookup account" testInsertLookupAccount
    it "test lookups" testLookups
    it "Test flatten" testFlatten
    insertionsAndLookups
