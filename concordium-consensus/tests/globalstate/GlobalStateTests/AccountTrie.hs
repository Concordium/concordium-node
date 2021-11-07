{-# LANGUAGE DataKinds, TypeApplications, ScopedTypeVariables #-}
{-|
  Tests of the account map related operations.
-}
module GlobalStateTests.AccountTrie where

import Concordium.Types
import Concordium.ID.Types
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import qualified Concordium.GlobalState.AccountMap as AccountMap
import Data.List (sort, sortOn, nubBy, nub)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Identity
import qualified Data.FixedByteString as FBS
import Data.Word
import Data.Maybe
import Data.Fix
import Data.Function (on)
import Data.Proxy

import Test.QuickCheck
import Test.Hspec

genAccountAddress :: Gen AccountAddress
genAccountAddress = AccountAddress . FBS.pack <$> vector accountAddressSize

genInputs :: Gen ([(AccountAddress, AccountIndex)], [AccountAddress])
genInputs = sized $ \n -> do
  len <- choose (0, min n 10000)
  inputs <- replicateM len ((,) <$> genAccountAddress <*> (AccountIndex <$> arbitrary))
  extraKeysLen <- choose (0, min n 1000)
  extraKeys <- replicateM extraKeysLen genAccountAddress
  return (inputs, extraKeys)

type Trie = Trie.TrieN Fix AccountAddress AccountIndex

-- Construct the reference map for comparing lookups.
constructReference :: [(AccountAddress, AccountIndex)] -> Map.Map AccountAddress AccountIndex
constructReference = Map.fromList

constructTrie :: [(AccountAddress, AccountIndex)] -> Trie
constructTrie = runIdentity . Trie.fromList

-- Helper to look up in the trie.
trieLookup :: AccountAddress -> Trie -> Maybe AccountIndex
trieLookup addr = runIdentity . Trie.lookup addr

-- Look up all the keys that match the given prefix.
triePrefixLookup :: [Word8] -> Trie -> [(AccountAddress, AccountIndex)]
triePrefixLookup prefix = runIdentity . Trie.lookupPrefix prefix

-- Insert and then lookup. Compares results to the reference map using Data.Map.
insertionTests :: Word -> Spec
insertionTests lvl = it "insert and lookup" $
    withMaxSuccess (10000 * fromIntegral lvl) $ property $
    forAll genInputs $ \(inputs, extraKeys) ->
                         let referenceMap = constructReference inputs
                         in let trie = constructTrie inputs
                            in conjoin (map (\k -> Map.lookup k referenceMap === trieLookup k trie) (map fst inputs ++ extraKeys))

-- |Check that using the empty prefix with 'Trie.lookupPrefix' is equivalent to
-- 'toList' modulo the order.
emptyPrefixTest :: Word -> Spec
emptyPrefixTest lvl = it "lookup empty prefix is the same as toList" $
    withMaxSuccess (10000 * fromIntegral lvl) $ property $
    forAll genInputs $ \(inputs, _) ->
                         let referenceMap = constructReference inputs
                         in let trie = constructTrie inputs
                            in sort (triePrefixLookup [] trie) === Map.toAscList referenceMap

-- |Check that 'Trie.lookupPrefix' reduces to 'Trie.lookup' in case of an exact
-- match. That this is a property relies on the fact that all keys have the same
-- length, which is a requirement for this trie implementation.
exactPrefixTest :: Word -> Spec
exactPrefixTest lvl = it "lookup exact prefix matches lookup" $
    withMaxSuccess (10000 * fromIntegral lvl) $ property $
    forAll genInputs $ \(inputs, extraKeys) ->
                         let trie = constructTrie inputs
                         in conjoin (map (\k -> ((k,) <$> (maybeToList (trieLookup k trie))) === triePrefixLookup (Trie.unpackKey k) trie)
                                    (map fst inputs ++ extraKeys))

-- |Generate all possible prefix sizes, including empty prefix.
genPrefixSizes :: Gen Int
genPrefixSizes = choose (0, 32)

-- |Check that 'Trie.lookupPrefix' returns exactly the right keys. This is
-- compared to the obvious implementation using the reference map.
allPrefixes :: Word -> Spec
allPrefixes lvl = it "lookup prefix" $
    withMaxSuccess (10000 * fromIntegral lvl) $ property $
    forAll genInputs $ \(inputs, _) -> forAll genPrefixSizes $ \prefixLen -> 
                         let trie = constructTrie inputs
                             -- Take the prefix of the key of specified length.
                             takePrefix = take prefixLen . Trie.unpackKey
                             -- All the unique addresses in the generated input.
                             inputAddresses =
                               nub .
                               sort .
                               map fst $ inputs
                         in conjoin (map (\k -> sort (map fst (triePrefixLookup (takePrefix k) trie)) ===
                                               filter ((== (takePrefix k)) . takePrefix) inputAddresses
                                                )
                                    (map fst inputs))

-- |Generate some aliases for the given account address. All aliases are unique
-- and the return list will have at least one.
genAliases :: AccountAddress -> Gen [AccountAddress]
genAliases (AccountAddress addr) = sized $ \n -> do
  len <- choose (1, min n 1000)
  aliases <- replicateM len (AccountAddress . FBS.pack . take 29 . (FBS.unpack addr ++) <$> vector 3)
  return (nub . sort $ aliases)

constructAccountMap :: forall pv . [(AccountAddress, AccountIndex)] -> AccountMap.PureAccountMap pv
constructAccountMap = AccountMap.AccountMap . runIdentity . Trie.fromList

-- |Check that for protocol version 3 
checkClashesP3 :: Word -> Spec
checkClashesP3 lvl = it "check aliases for P3" $
    withMaxSuccess (10000 * fromIntegral lvl) $ property $
    forAll genInputs $ \(inputs, _) -> 
       let am = constructAccountMap @'P3 inputs
       in conjoin (map (\(k, _) -> forAll (genAliases k) $
                         \aliases -> conjoin (map (flip AccountMap.addressWouldClashPure am) aliases))
                       inputs)

-- |Check that for protocol versions P1 and P2 addressWouldClash implies
-- equality of addresses. This test is only meant to be instantiated with P1 and
-- P2 protocol versions since it should fail for protocol version 3.
-- See 'checkClashesP3' for that protocol version.
checkClashesP1P2 :: forall pv . IsProtocolVersion pv => Proxy pv -> Word -> Spec
checkClashesP1P2 Proxy lvl = it ("check aliases for protocol version " ++ show (demoteProtocolVersion (protocolVersion @pv))) $
    withMaxSuccess (10000 * fromIntegral lvl) $ property $
    forAll genInputs $ \(inputs, _) -> 
       let mkPrefix (AccountAddress addr, _) = take 29 . FBS.unpack $ addr
           withUniquePrefixes = nubBy ((==) `on` mkPrefix) . sortOn mkPrefix $ inputs
           am = constructAccountMap @pv withUniquePrefixes
       in conjoin (map (\(addr, _) -> forAll (genAliases addr) $
                         \aliases -> conjoin (map (\alias -> alias == addr || not (AccountMap.addressWouldClashPure alias am)) aliases))
                       withUniquePrefixes)


tests :: Word -> Spec
tests lvl = do
  describe "GlobalStateTests.AccountTrie" $ do
    insertionTests lvl
    emptyPrefixTest lvl
    exactPrefixTest lvl
    allPrefixes lvl
  describe "GlobalStateTests.AccountMap" $ do
    checkClashesP1P2 (Proxy @'P1) lvl
    checkClashesP1P2 (Proxy @'P2) lvl
    checkClashesP3 lvl
