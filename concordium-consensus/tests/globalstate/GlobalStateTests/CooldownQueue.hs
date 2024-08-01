{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests the 'CooldownQueue' structure, and the various functions that operate on it.
module GlobalStateTests.CooldownQueue where

import qualified Data.Map.Strict as Map
import Data.Serialize
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.HashableTo
import Concordium.Types.Option

import Concordium.GlobalState.CooldownQueue

-- | Generate a 'Cooldowns' with arbitrary values.
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

-- | An example 'Cooldowns' structure with 3 amounts in cooldown, a pre-cooldown of 150, and a
-- pre-pre-cooldown of 2000.
cooldown1 :: Cooldowns
cooldown1 =
    Cooldowns
        { inCooldown = Map.fromList [(Timestamp 10, 1), (Timestamp 20, 15), (Timestamp 30, 100)],
          preCooldown = Present 150,
          prePreCooldown = Present 2000
        }

-- | An example 'Cooldowns' structure with 3 amounts in cooldown, no pre-cooldown, and a
-- pre-pre-cooldown of 2000.
cooldown2 :: Cooldowns
cooldown2 = cooldown1{preCooldown = Absent}

-- | An example 'Cooldowns' structure with 3 amounts in cooldown, no pre-cooldown, and no
-- pre-pre-cooldown.
cooldown3 :: Cooldowns
cooldown3 = cooldown2{prePreCooldown = Absent}

testReactivateCooldownAmount :: Assertion
testReactivateCooldownAmount = do
    assertEqual "reactivate 0" cooldown1 (reactivateCooldownAmount 0 cooldown1)
    assertEqual
        "reactivate 10"
        cooldown1{prePreCooldown = Present 1990}
        (reactivateCooldownAmount 10 cooldown1)
    assertEqual
        "reactivate 2000"
        cooldown1{prePreCooldown = Absent}
        (reactivateCooldownAmount 2000 cooldown1)
    assertEqual
        "reactivate 2010"
        cooldown1{prePreCooldown = Absent, preCooldown = Present 140}
        (reactivateCooldownAmount 2010 cooldown1)
    assertEqual
        "reactivate 2150"
        cooldown3
        (reactivateCooldownAmount 2150 cooldown1)
    assertEqual
        "reactivate 2200"
        cooldown3{inCooldown = Map.fromList [(Timestamp 10, 1), (Timestamp 20, 15), (Timestamp 30, 50)]}
        (reactivateCooldownAmount 2200 cooldown1)
    assertEqual
        "reactivate 2250"
        cooldown3{inCooldown = Map.fromList [(Timestamp 10, 1), (Timestamp 20, 15)]}
        (reactivateCooldownAmount 2250 cooldown1)
    assertEqual
        "reactivate 2255"
        cooldown3{inCooldown = Map.fromList [(Timestamp 10, 1), (Timestamp 20, 10)]}
        (reactivateCooldownAmount 2255 cooldown1)
    assertEqual
        "reactivate 2265"
        cooldown3{inCooldown = Map.fromList [(Timestamp 10, 1)]}
        (reactivateCooldownAmount 2265 cooldown1)
    assertEqual
        "reactivate 2266"
        emptyCooldowns
        (reactivateCooldownAmount 2266 cooldown1)
    assertEqual
        "reactivate 5000"
        emptyCooldowns
        (reactivateCooldownAmount 5000 cooldown1)
    assertEqual
        "reactivate 2050 (cooldown2)"
        cooldown3{inCooldown = Map.fromList [(Timestamp 10, 1), (Timestamp 20, 15), (Timestamp 30, 50)]}
        (reactivateCooldownAmount 2050 cooldown2)
    assertEqual
        "reactivate 50 (cooldown 3)"
        cooldown3{inCooldown = Map.fromList [(Timestamp 10, 1), (Timestamp 20, 15), (Timestamp 30, 50)]}
        (reactivateCooldownAmount 50 cooldown3)

-- | Unit test for 'processCooldowns'.
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
        (processCooldowns 30_000 cooldown1)

-- | Unit test for 'processPreCooldown'.
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

-- | Unit test for 'processPrePreCooldown'.
testProcessPrePreCooldown :: Assertion
testProcessPrePreCooldown = do
    assertEqual "no pre-pre-cooldown" (processPrePreCooldown cooldown3) cooldown3
    assertEqual
        "no pre-cooldown"
        cooldown3{preCooldown = Present 2000}
        (processPrePreCooldown cooldown2)
    assertEqual
        "with pre-cooldown"
        cooldown3{preCooldown = Present 2150}
        (processPrePreCooldown cooldown1)

-- | Example 'CooldownCalculationParameters' for testing.
ccp1 :: CooldownCalculationParameters
ccp1 =
    CooldownCalculationParameters
        { ccpEpochDuration = Duration 1000,
          ccpCurrentEpoch = 10,
          ccpTriggerTime = Timestamp 1_000_000,
          ccpNextPayday = 20,
          ccpRewardPeriodLength = RewardPeriodLength 20,
          ccpCooldownDuration = DurationSeconds 1000
        }

-- | Example 'CooldownCalculationParameters' for testing, where the next epoch is a payday.
ccp2 :: CooldownCalculationParameters
ccp2 = ccp1{ccpCurrentEpoch = 19}

-- | Test that 'preCooldownTimestamp' calculates the correct timestamp.
testPreCooldownTimestamp :: Assertion
testPreCooldownTimestamp = do
    assertEqual
        "pre-cooldown timestamp 1"
        ( 1_000_000 -- Trigger time
            + 9 * 1000 -- 10 epochs till next payday
            + 1_000_000 -- Cooldown duration
        )
        (preCooldownTimestamp ccp1)
    assertEqual
        "pre-cooldown timestamp 2"
        ( 1_000_000 -- Trigger time
            + 0 * 1000 -- 1 epoch till next payday
            + 1_000_000 -- Cooldown duration
        )
        (preCooldownTimestamp ccp2)

-- | Test that 'prePreCooldownTimestamp' calculates the correct timestamp.
testPrePreCooldownTimestamp :: Assertion
testPrePreCooldownTimestamp = do
    assertEqual
        "pre-pre-cooldown timestamp 1"
        ( 1_000_000 -- Trigger time
            + 9 * 1000 -- 10 epochs till next payday
            + 1_000_000 -- Cooldown duration
        )
        (prePreCooldownTimestamp ccp1)
    assertEqual
        "pre-pre-cooldown timestamp 2"
        ( 1_000_000 -- Trigger time
            + 20 * 1000 -- Next epoch is payday; 20 epochs till next payday after that
            + 1_000_000 -- Cooldown duration
        )
        (prePreCooldownTimestamp ccp2)

-- | Unit test for 'toCooldownList'.
testToCooldownList :: Assertion
testToCooldownList = do
    assertEqual
        "empty cooldowns"
        []
        (toCooldownList ccp1 emptyCooldowns)
    assertEqual
        "cooldowns"
        [ Cooldown 10 1 StatusCooldown,
          Cooldown 20 15 StatusCooldown,
          Cooldown 30 100 StatusCooldown,
          Cooldown 2_000_000 150 StatusPreCooldown,
          Cooldown 2_020_000 2000 StatusPrePreCooldown
        ]
        (toCooldownList ccp2 cooldown1)

testAddPrePreCooldownEmpty :: Assertion
testAddPrePreCooldownEmpty = do
    let amount = 100
    let newCooldowns = addPrePreCooldown amount emptyCooldowns
    assertEqual "pre-pre-cooldown" (Present amount) (prePreCooldown newCooldowns)
    assertEqual "pre-cooldown" Absent (preCooldown newCooldowns)
    assertEqual "in-cooldown" Map.empty (inCooldown newCooldowns)

testAddPrePreCooldownNonEmpty :: Assertion
testAddPrePreCooldownNonEmpty = do
    let amount = 100
    let newCooldowns = addPrePreCooldown amount cooldown1
    assertEqual "pre-pre-cooldown" (Present $ 2000 + amount) (prePreCooldown newCooldowns)
    assertEqual "pre-cooldown" (preCooldown cooldown1) (preCooldown newCooldowns)
    assertEqual "in-cooldown" (inCooldown cooldown1) (inCooldown newCooldowns)

tests :: Spec
tests = describe "GlobalStateTests.CooldownQueue" $ parallel $ do
    it "Cooldowns serialization" $ withMaxSuccess 1000 testSerialize
    it "Empty cooldowns is empty" $ isEmptyCooldowns emptyCooldowns
    it "isEmptyCooldowns" testIsEmptyCooldowns
    it "Hashing cooldowns" $ withMaxSuccess 10_000 testHashCooldowns
    it "cooldownTotal" $ cooldownTotal cooldown1 == 2266
    it "processCooldowns" testProcessCooldowns
    it "processPreCooldown" testProcessPreCooldown
    it "processPrePrecooldown" testProcessPrePreCooldown
    it "preCooldownTimestamp" testPreCooldownTimestamp
    it "prePreCooldownTimestamp" testPrePreCooldownTimestamp
    it "toCooldownList" testToCooldownList
    it "addPrePreCooldown empty" testAddPrePreCooldownEmpty
    it "addPrePreCooldown non-empty" testAddPrePreCooldownNonEmpty
    it "reactivateCooldownAmount" testReactivateCooldownAmount
