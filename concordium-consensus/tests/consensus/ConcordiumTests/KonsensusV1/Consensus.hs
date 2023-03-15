-- |Module testing functions from the 'Concordium.KonsensusV1.Consensus' module.
module ConcordiumTests.KonsensusV1.Consensus where

import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

import ConcordiumTests.KonsensusV1.Types

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
                    (rsCurrentRound newRoundStatus)
                assertEqual
                    "RoundStatus current epoch should remain"
                    (rsCurrentEpoch fromRoundStatus)
                    (rsCurrentEpoch newRoundStatus)
                assertEqual
                    "RoundStatus previous round TC should be absent"
                    Absent
                    (rsPreviousRoundTC newRoundStatus)
                assertEqual
                    "Timeout signatures for current round should be empty"
                    emptySignatureMessages
                    (rsCurrentTimeoutSignatureMessages newRoundStatus)
                assertEqual
                    "QC signatures for current round should be empty"
                    emptySignatureMessages
                    (rsCurrentQuorumSignatureMessages newRoundStatus)
                assertEqual
                    "QC signatures for current round should be empty"
                    (Present highestQC)
                    (rsHighestQC newRoundStatus)

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
                        (rsCurrentRound newRoundStatus)
                    assertEqual
                        "RoundStatus current epoch should remain"
                        (rsCurrentEpoch fromRoundStatus)
                        (rsCurrentEpoch newRoundStatus)
                    assertEqual
                        "RoundStatus previous round TC should be present"
                        (Present (tc, qc))
                        (rsPreviousRoundTC newRoundStatus)
                    assertEqual
                        "Timeout signatures for current round should be empty"
                        emptySignatureMessages
                        (rsCurrentTimeoutSignatureMessages newRoundStatus)
                    assertEqual
                        "QC signatures for current round should be empty"
                        emptySignatureMessages
                        (rsCurrentQuorumSignatureMessages newRoundStatus)

-- |Checking that advancing epochs yields
-- the correct 'RoundStatus'
propAdvanceRoundStatusEpoch :: Property
propAdvanceRoundStatusEpoch =
    forAll genRoundStatus $ \fromRoundStatus ->
        forAll genEpoch $ \toEpoch ->
            forAll genFinalizationEntry $ \finalizationEntry ->
                forAll genLeadershipElectionNonce $ \newLeadershipElectionNonce -> do
                    let newRoundStatus = advanceRoundStatusEpoch toEpoch finalizationEntry newLeadershipElectionNonce fromRoundStatus
                    assertEqual
                        "RoundStatus should have advanced epoch"
                        toEpoch
                        (rsCurrentEpoch newRoundStatus)
                    assertEqual
                        "RoundStatus should have a present finalization entry"
                        (Present finalizationEntry)
                        (rsLatestEpochFinEntry newRoundStatus)
                    assertEqual
                        "RoundStatus should have updated the leadership election nonce"
                        newLeadershipElectionNonce
                        (rsLeadershipElectionNonce newRoundStatus)

tests :: Spec
tests = describe "KonsensusV1.Consensus" $ do
    it "RoundStatus advances from quorum round" propAdvanceRoundStatusFromQuorumRound
    it "RoundStatus advances from timed out round" propAdvanceRoundStatusFromTCRound
    it "RoundStatus advances epoch" propAdvanceRoundStatusEpoch
