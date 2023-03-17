-- |Module testing functions from the 'Concordium.KonsensusV1.Consensus' module.
module ConcordiumTests.KonsensusV1.Consensus where

import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import Lens.Micro.Platform

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
