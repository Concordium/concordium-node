{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- |Tests for the functions in 'Concordium.KonsensusV1.LeaderElection'.
module ConcordiumTests.KonsensusV1.LeaderElectionTest where

import Data.Serialize
import qualified Data.Vector as Vec
import System.Random
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types
import Concordium.Types.SeedState

import Concordium.GlobalState.BakerInfo
import Concordium.KonsensusV1.LeaderElection
import Concordium.Types.Accounts

dummyVRFKeys :: VRF.KeyPair
dummyVRFKeys = fst $ VRF.randomKeyPair (mkStdGen 0)

dummyBlockNonce :: BlockNonce
dummyBlockNonce = VRF.prove dummyVRFKeys ""

-- |This 'Get FullBakers' implementation exists only so
-- we are sure we are hashing the things we expect when
-- updating the leader election nonce.
-- Hence 'putFullBakers' is the function that creates the
-- bytes for hashing in practice.
getFullBakers :: Get FullBakers
getFullBakers = do
    count <- getWord64be
    fullBakerInfos <- Vec.replicateM (fromIntegral count) get
    let bakerTotalStake = Vec.foldl' (\acc fbi -> acc + _bakerStake fbi) 0 fullBakerInfos
    return FullBakers{..}

-- |A dummy `FullBakers` suitable for testing
-- leader election nonce derivation (i.e. hashing the serialized form) and a basic serialization / deserialization.
dummyFullBakers :: FullBakers
dummyFullBakers =
    FullBakers
        { fullBakerInfos = Vec.fromList [FullBakerInfo (BakerInfo 1 bek bsk bak) 1000000000000, FullBakerInfo (BakerInfo 2 bek bsk bak) 1000000000000],
          bakerTotalStake = 2000000000000
        }
  where
    (Right bek) = decode "\ESC\222==\210(r%dNG\SOHl\161\160w\238\NAKx\205?\180\137=L\156\203\181\\\155\131\232"
    (Right bsk) = decode "\200\SI\250\177\231!\178\142\218\246\152u2\DC1D= b\208\132\245\137\133\206\FS\217)\246q\242\229\235"
    (Right bak) = decode "\x8e\xa8\x59\x44\x28\x81\xdd\xc9\x48\x91\xb1\x99\x3e\x5e\x5d\x18\x44\xe4\x2c\x31\xf1\xf2\x27\x1a\xb4\x50\xff\xb2\x7a\x17\x5b\x42\x39\xaa\xdf\x3c\x4f\xf0\x94\xec\x19\x6e\x5f\xb9\x4f\x73\x7b\x94\x0f\xfb\x0a\x73\x93\x59\x47\x76\xc7\xe6\x7a\x43\x35\x6d\x60\xc8\xf9\x25\x12\x1b\x3b\xf6\x23\xb9\xae\xcb\x4b\x50\xf3\xd9\xe2\xaf\x31\x21\xa1\xd3\xf0\xf8\x7e\xfe\x11\xc5\x83\xf3\x88\xe5\x42\x77"

-- |Serialization test for FullBakers.
-- Note that we are never deserializing the full bakers in practice,
-- as we are only using the serialization instance for generating the bytes
-- for hashing.
-- But the test exists so we are sure that we are hashing what we expect.
serializeDeserializeFullBakers :: Spec
serializeDeserializeFullBakers = describe "serialize and deserialize full bakers" $ do
    it "serialize/deserialize" $ do
        case runGet getFullBakers $! runPut (putFullBakers dummyFullBakers) of
            Left err -> assertFailure err
            Right bkrs -> assertEqual "The full bakers should be the same" dummyFullBakers bkrs

-- |Tests for 'getLeader' on specific values with specific outcomes.
-- These are intended as a regression test, as a change in the behaviour of 'getLeader' would likely
-- cause them to fail.
-- The test cases demonstrate that different inputs (leader election nonce, round, baker stakes,
-- number of bakers) result in different outputs from 'getLeader'.
testGetLeader :: Spec
testGetLeader = describe "getLeader" $ do
    it "1" $ testIt [(i, 1000000000000000) | i <- [0 .. 50]] len1 0 31
    it "2" $ testIt [(i, 1000000000000000) | i <- [0 .. 50]] len2 0 6
    it "3" $ testIt [(i, 1000000000000000) | i <- [0 .. 50]] len1 1 7
    it "4" $ testIt [(i, 1000000000000001) | i <- [0 .. 50]] len1 0 47
    it "5" $ testIt [(i, 1000000000000000) | i <- [0 .. 500]] len1 0 106
    it "6" $ testIt [(i, 1000000000000000) | i <- [0 .. 500]] len2 0 39
  where
    len1 = Hash.hash "LEN1"
    len2 = Hash.hash "LEN2"
    testIt bakers len rnd expec =
        assertEqual
            ("getLeader " ++ show bakers ++ " " ++ show len ++ " " ++ show rnd)
            (expec :: Int)
            (getLeader bakers len rnd)

testUpdateSeedStateForBlock :: Spec
testUpdateSeedStateForBlock = describe "updateSeedStateForBlock" $ do
    it "normal" $
        updateSeedStateForBlock 100 bn ss
            `shouldBe` ss
                { updatedNonce = read "0f392d28b4de2f783a927a78373acbc7238ecf1f288cf0796bb55c6d5f786d0b"
                }
    it "trigger" $
        updateSeedStateForBlock 600 bn ss
            `shouldBe` ss
                { updatedNonce = read "0f392d28b4de2f783a927a78373acbc7238ecf1f288cf0796bb55c6d5f786d0b",
                  epochTransitionTriggered = True
                }
    it "already triggered" $
        updateSeedStateForBlock 700 bn ss{epochTransitionTriggered = True}
            `shouldBe` ss{epochTransitionTriggered = True}
  where
    ss = initialSeedStateV1 (Hash.hash "LEN1") 600
    bn = dummyBlockNonce

-- |Test 'updateSeedStateForEpoch'. This tests one specific evaluation.
testUpdateSeedStateForEpoch :: Spec
testUpdateSeedStateForEpoch =
    it "updateSeedStateForEpoch" $
        updateSeedStateForEpoch dummyFullBakers 1000 ss `shouldBe` ss'
  where
    ss =
        SeedStateV1
            { epoch = 27,
              epochTransitionTriggered = True,
              triggerBlockTime = 91000,
              updatedNonce = read "0f392d28b4de2f783a927a78373acbc7238ecf1f288cf0796bb55c6d5f786d0b",
              currentLeadershipElectionNonce = read "f1f288cf0796bb55c6d5f786d0b0f392d28b4de2f783a927a78373acbc7238ec"
            }
    ss' =
        SeedStateV1
            { epoch = 28,
              epochTransitionTriggered = False,
              triggerBlockTime = 92000,
              updatedNonce = read "ba3aba3b6c31fb6b0251a19c83666cd90da9a0835a2b54dc4f01c6d451ab24e8",
              currentLeadershipElectionNonce = read "ba3aba3b6c31fb6b0251a19c83666cd90da9a0835a2b54dc4f01c6d451ab24e8"
            }

tests :: Spec
tests = describe "KonsensusV1.LeadershipElection" $ do
    testGetLeader
    testUpdateSeedStateForBlock
    testUpdateSeedStateForEpoch
    serializeDeserializeFullBakers
