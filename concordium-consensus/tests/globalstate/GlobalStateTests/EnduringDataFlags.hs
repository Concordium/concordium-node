-- |This module tests the serialization and deserialization of the 'EnduringDataFlags', which
-- are used in the persistent storage of accounts from 'P5' onwards.
module GlobalStateTests.EnduringDataFlags where

import Test.Hspec
import Test.QuickCheck

import Concordium.GlobalState.Persistent.Account.StructureV1

genPendingChangeFlags :: Gen PendingChangeFlags
genPendingChangeFlags = elements [PendingChangeNone, PendingChangeReduce, PendingChangeRemove]

genStakeFlags :: Gen StakeFlags
genStakeFlags = oneof [
        pure StakeFlagsNone,
        StakeFlagsBaker <$> arbitrary <*> genPendingChangeFlags,
        StakeFlagsDelegator <$> arbitrary <*> arbitrary <*> genPendingChangeFlags
    ]

genEnduringDataFlags :: Gen EnduringDataFlags
genEnduringDataFlags = EnduringDataFlags <$> arbitrary <*> arbitrary <*> genStakeFlags

-- |Test that converting 'EnduringDataFlags' to bits and big is the identity.
testToFromBits :: Property
testToFromBits = forAll genEnduringDataFlags $ \edf ->
    Right edf === enduringDataFlagsFromBits (enduringDataFlagsToBits edf)

-- |Test that converting bits to 'EnduringDataFlags' and back is the identity where the first
-- conversion is well-defined.
testFromToBits :: Property
testFromToBits = property $ \bs -> case enduringDataFlagsFromBits bs of
    Left _ -> property ()
    Right edf -> bs === enduringDataFlagsToBits edf

tests :: Spec
tests = parallel $ do
    it "EnduringDataFlags to then from bits" $ withMaxSuccess 10000 testToFromBits
    it "EnduringDataFlags from then to bits" $ withMaxSuccess 10000 testFromToBits
