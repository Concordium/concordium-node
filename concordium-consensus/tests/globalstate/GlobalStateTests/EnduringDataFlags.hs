{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests the serialization and deserialization of the 'EnduringDataFlags', which
--  are used in the persistent storage of accounts from 'P5' onwards.
module GlobalStateTests.EnduringDataFlags where

import Data.Singletons
import Test.Hspec
import Test.QuickCheck

import Concordium.Types
import Concordium.Types.Conditionally

import Concordium.GlobalState.Persistent.Account.StructureV1

genPendingChangeFlags :: Gen PendingChangeFlags
genPendingChangeFlags = elements [PendingChangeNone, PendingChangeReduce, PendingChangeRemove]

genStakeFlags :: Bool -> Gen StakeFlags
genStakeFlags flexCooldown =
    oneof
        [ return StakeFlagsNone,
          StakeFlagsBaker <$> arbitrary <*> genPCF,
          StakeFlagsDelegator <$> arbitrary <*> arbitrary <*> genPCF
        ]
  where
    genPCF = if flexCooldown then return PendingChangeNone else genPendingChangeFlags

genEnduringDataFlags ::
    forall av.
    (IsAccountVersion av) =>
    SAccountVersion av ->
    Gen (EnduringDataFlags av)
genEnduringDataFlags _ =
    EnduringDataFlags
        <$> arbitrary
        <*> arbitrary
        <*> genStakeFlags (fromSing $ sSupportsFlexibleCooldown (accountVersion @av))
        <*> conditionallyA (sSupportsFlexibleCooldown (accountVersion @av)) arbitrary

-- | Test that converting 'EnduringDataFlags' to bits and big is the identity.
testToFromBits :: (IsAccountVersion av) => SAccountVersion av -> Property
testToFromBits sav = forAll (genEnduringDataFlags sav) $ \edf ->
    Right edf === enduringDataFlagsFromBits (enduringDataFlagsToBits edf)

-- | Test that converting bits to 'EnduringDataFlags' and back is the identity where the first
--  conversion is well-defined.
testFromToBits :: forall av. (IsAccountVersion av) => SAccountVersion av -> Property
testFromToBits sav = property $ \bs -> case enduringDataFlagsFromBits @av bs of
    Left _ -> property ()
    Right edf -> bs === enduringDataFlagsToBits edf

tests :: Spec
tests = parallel $ do
    describe "AccountV2" $ avTests SAccountV2
    describe "AccountV3" $ avTests SAccountV3
  where
    avTests :: (IsAccountVersion av) => SAccountVersion av -> Spec
    avTests sav = do
        it "EnduringDataFlags to then from bits" $ withMaxSuccess 10000 (testToFromBits sav)
        it "EnduringDataFlags from then to bits" $ withMaxSuccess 10000 (testFromToBits sav)
