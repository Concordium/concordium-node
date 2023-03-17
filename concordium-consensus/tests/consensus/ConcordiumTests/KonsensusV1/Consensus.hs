{-# LANGUAGE OverloadedStrings #-}

-- |Module testing functions from the 'Concordium.KonsensusV1.Consensus' module.
module ConcordiumTests.KonsensusV1.Consensus(tests) where

import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec
import Data.Ratio
import Data.Word
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

import Concordium.Types
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.KonsensusV1.Consensus

import Test.QuickCheck


import ConcordiumTests.KonsensusV1.Types


dummyLeadershipElectionNonce :: LeadershipElectionNonce
dummyLeadershipElectionNonce = Hash.hash "LeadershipElectionNonce"

dummyGenesisHash :: BlockHash
dummyGenesisHash = BlockHash $ Hash.hash "genesis"

dummyRoundStatus :: Round -> Duration -> RoundStatus
dummyRoundStatus currentRound baseTimeout = (initialRoundStatus baseTimeout dummyLeadershipElectionNonce dummyGenesisHash){rsCurrentRound = currentRound}

-- |Test that 'updateRoundStatus' updates the fields 'rsNextSignableRound' and 'rsCurrentTimeout'
-- of 'RoundStatus' correctly.
testCase :: Round -> Duration -> Ratio Word64 -> Round -> Duration -> Assertion
testCase currentRound baseTimeout timeoutIncrease expectedRound expectedTimeout  = do
    let newRoundStatus = updateRoundStatus timeoutIncrease $ dummyRoundStatus currentRound baseTimeout
    let actualRound = rsNextSignableRound newRoundStatus
    assertEqual "Round number should be correct" expectedRound actualRound 
    let actualTimeout = rsCurrentTimeout newRoundStatus
    assertEqual "Timeout duration should be correct" expectedTimeout actualTimeout 


-- tests :: Spec
-- tests = describe "KonsensusV1.Consensus" $ do
--     it "Test updateRoundStatus" $ do
--         testCase 0 10000 (3 % 2) 1 15000
--         testCase 1 10000 (4 % 3) 2 13333
--         testCase 2 10000 (5 % 3) 3 16666
--         testCase 3 3000 (4 % 3) 4 4000
--         testCase 4 80000 (10 % 9) 5 88888
--         testCase 5 8000 (8 % 7) 6 9142


-- |Checking that advancing rounds via a quorum certificate yields
-- the correct 'RoundStatus'
propAdvanceRoundStatusFromQuorumRound :: Property
propAdvanceRoundStatusFromQuorumRound =
    forAll genRoundStatus $ \fromRoundStatus ->
        forAll genRound $ \toRound ->
            forAll genQuorumCertificate $ \highestQC -> do
                let newRoundStatus = advanceRoundStatus toRound (Right highestQC) fromRoundStatus
                assertEqual
                    "RoundStatus current round should be advanced"
                    toRound
                    (newRoundStatus ^. rsCurrentRound)
                assertEqual
                    "RoundStatus previous round TC should be absent"
                    Absent
                    (newRoundStatus ^. rsPreviousRoundTC)
                assertEqual
                    "QC signatures for current round should be empty"
                    (Present highestQC)
                    (newRoundStatus ^. rsHighestQC)

-- |Checking that advancing rounds via a timeout certificate yields
-- the correct 'RoundStatus'
propAdvanceRoundStatusFromTCRound :: Property
propAdvanceRoundStatusFromTCRound =
    forAll genRoundStatus $ \fromRoundStatus ->
        forAll genTimeoutCertificate $ \tc ->
            forAll genQuorumCertificate $ \qc ->
                forAll genRound $ \toRound -> do
                    let tcQc = Left (tc, qc)
                        newRoundStatus = advanceRoundStatus toRound tcQc fromRoundStatus
                    assertEqual
                        "RoundStatus current round should be advanced"
                        toRound
                        (newRoundStatus ^. rsCurrentRound)
                    assertEqual
                        "RoundStatus previous round TC should be present"
                        (Present (tc, qc))
                        (newRoundStatus ^. rsPreviousRoundTC)

tests :: Spec
tests = describe "KonsensusV1.Consensus" $ do
    it "RoundStatus advances from quorum round" propAdvanceRoundStatusFromQuorumRound
    it "RoundStatus advances from timed out round" propAdvanceRoundStatusFromTCRound
    it "Test updateRoundStatus" $ do
        testCase 0 10000 (3 % 2) 1 15000
        testCase 1 10000 (4 % 3) 2 13333
        testCase 2 10000 (5 % 3) 3 16666
        testCase 3 3000 (4 % 3) 4 4000
        testCase 4 80000 (10 % 9) 5 88888
        testCase 5 8000 (8 % 7) 6 9142
