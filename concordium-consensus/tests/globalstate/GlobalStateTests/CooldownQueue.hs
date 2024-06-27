{-# LANGUAGE TypeApplications #-}

module GlobalStateTests.CooldownQueue where

import qualified Data.Map.Strict as Map
import Data.Serialize
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import Concordium.Types
import Concordium.Types.Option

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.CooldownQueue
import Concordium.Types.HashableTo

genCooldowns :: Gen Cooldowns
genCooldowns = do
    inCooldown <- Map.fromList <$> listOf (((,) . Timestamp <$> arbitrary) <*> arbitrary)
    preCooldown <- oneof [return Absent, Present <$> arbitrary]
    prePreCooldown <- oneof [return Absent, Present <$> arbitrary]
    return Cooldowns{..}

-- | Test that serializing and deserializing 'Cooldowns' is the identity.
testSerialize :: Property
testSerialize = property $ forAll genCooldowns $ \cds -> decode (encode cds) === Right cds

-- | Test that 'isEmptyCooldowns' only holds for the empty cooldowns.
testIsEmptyCooldowns :: Property
testIsEmptyCooldowns = property $ forAll genCooldowns $ \cds ->
    isEmptyCooldowns cds === (cds == emptyCooldowns)

-- | Test that hashing two different cooldowns gives different hashes.
testHashCooldowns :: Property
testHashCooldowns = property $ forAll genCooldowns $ \cds1 -> forAll genCooldowns $ \cds2 ->
    cds1 /= cds2 ==> getHash @Hash.Hash cds1 /= getHash cds2

cooldown1 :: Cooldowns
cooldown1 =
    Cooldowns
        { inCooldown = Map.fromList [(Timestamp 10, 1), (Timestamp 20, 15), (Timestamp 30, 100)],
          preCooldown = Present 150,
          prePreCooldown = Present 2000
        }

cooldown2 :: Cooldowns
cooldown2 = cooldown1{preCooldown = Absent}

cooldown3 :: Cooldowns
cooldown3 = cooldown2{prePreCooldown = Absent}

testProcessCooldowns :: Assertion
testProcessCooldowns = do
    assertEqual
        "after 5"
        cooldown1
        (processCooldowns 5 cooldown1)
    assertEqual
        "after 10"
        cooldown1{inCooldown = Map.fromList [(Timestamp 20, 15), (Timestamp 30, 100)]}
        (processCooldowns 10 cooldown1)
    assertEqual
        "after 20"
        cooldown1{inCooldown = Map.fromList [(Timestamp 30, 100)]}
        (processCooldowns 20 cooldown1)
    assertEqual
        "after 25"
        cooldown1{inCooldown = Map.fromList [(Timestamp 30, 100)]}
        (processCooldowns 25 cooldown1)
    assertEqual
        "after 30"
        cooldown1{inCooldown = Map.empty}
        (processCooldowns 30 cooldown1)
    assertEqual
        "after 30000"
        cooldown1{inCooldown = Map.empty}
        (processCooldowns 30000 cooldown1)

testProcessPreCooldown :: Assertion
testProcessPreCooldown = do
    assertEqual "no pre-cooldown" cooldown2 (processPreCooldown 25 cooldown2)
    assertEqual
        "at 4000"
        cooldown2{inCooldown = Map.insert (Timestamp 4000) 150 (inCooldown cooldown1)}
        (processPreCooldown 4000 cooldown1)
    assertEqual
        "at 25"
        cooldown2{inCooldown = Map.insert (Timestamp 25) 150 (inCooldown cooldown1)}
        (processPreCooldown 25 cooldown1)
    assertEqual
        "at 20"
        cooldown2{inCooldown = Map.insert (Timestamp 20) 165 (inCooldown cooldown1)}
        (processPreCooldown 20 cooldown1)

testProcessPrePreCooldown :: Assertion
testProcessPrePreCooldown = do
    assertEqual "no pre-pre-cooldown" (processPrePreCooldown cooldown3) cooldown3
    assertEqual
        "no pre-cooldown"
        cooldown2{preCooldown = Present 2000}
        (processPrePreCooldown (cooldown1{preCooldown = Absent}))
    assertEqual
        "with pre-cooldown"
        cooldown3{preCooldown = Present 2150}
        (processPrePreCooldown cooldown1)

tests :: Spec
tests = describe "GlobalStateTests.CooldownQueue" $ parallel $ do
    it "Cooldowns serialization" $ withMaxSuccess 1000 testSerialize
    it "Empty cooldowns is empty" $ isEmptyCooldowns emptyCooldowns
    it "isEmptyCooldowns" testIsEmptyCooldowns
    it "Hashing cooldowns" $ withMaxSuccess 10000 testHashCooldowns
    it "cooldownTotal" $ cooldownTotal cooldown1 == 2266
    it "processCooldowns" testProcessCooldowns
    it "processPreCooldown" testProcessPreCooldown
    it "processPrePrecooldown" testProcessPrePreCooldown
