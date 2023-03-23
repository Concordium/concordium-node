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
import Concordium.KonsensusV1.Consensus.Timeout

import Test.QuickCheck


import ConcordiumTests.KonsensusV1.Types hiding (tests)


dummyLeadershipElectionNonce :: LeadershipElectionNonce
dummyLeadershipElectionNonce = Hash.hash "LeadershipElectionNonce"

dummyGenesisHash :: BlockHash
dummyGenesisHash = BlockHash $ Hash.hash "genesis"

dummyRoundStatus :: Round -> Duration -> RoundStatus
dummyRoundStatus currentRound baseTimeout = (initialRoundStatus dummyGenesisHash){_rsCurrentRound = currentRound}

-- |Test that 'updateCurrentTimeout' correctly calculates a new timeout given the current timeout and the
-- `timeoutIncrease` parameter.
testUpdateCurrentTimeout :: Duration -> Ratio Word64 -> Duration -> Assertion
testUpdateCurrentTimeout baseTimeout timeoutIncrease expectedTimeout  = do
    let actualTimeout = updateCurrentTimeout timeoutIncrease baseTimeout 
    assertEqual "Timeout duration should be correct" expectedTimeout actualTimeout 

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
                    highestQC
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
    it "Test updateCurrentTimeout" $ do
        testUpdateCurrentTimeout 10000 (3 % 2) 15000
        testUpdateCurrentTimeout 10000 (4 % 3) 13333
        testUpdateCurrentTimeout 10000 (5 % 3) 16666
        testUpdateCurrentTimeout 3000 (4 % 3) 4000
        testUpdateCurrentTimeout 80000 (10 % 9) 88888
        testUpdateCurrentTimeout 8000 (8 % 7) 9142
