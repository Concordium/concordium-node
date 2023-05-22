{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |Module testing functions from the 'Concordium.KonsensusV1.Consensus' module.
module ConcordiumTests.KonsensusV1.Consensus (tests) where

import Control.Monad.IO.Class
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Data.Map.Strict as Map

import qualified Concordium.Crypto.DummyData as Dummy
import Concordium.Genesis.Data
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Startup
import Concordium.Types
import qualified Concordium.Types.DummyData as Dummy
import ConcordiumTests.KonsensusV1.TreeStateTest (dummyBlock)

genesisData :: GenesisData 'P6
(genesisData, _, _) =
    makeGenesisDataV1 @'P6
        0
        10
        3_600_000
        Dummy.dummyCryptographicParameters
        Dummy.dummyIdentityProviders
        Dummy.dummyArs
        [ foundationAcct
        ]
        Dummy.dummyKeyCollection
        Dummy.dummyChainParameters
  where
    foundationAcct =
        Dummy.createCustomAccount
            1_000_000_000_000
            (Dummy.deterministicKP 0)
            (Dummy.accountAddressFrom 0)

-- |A dummy certified block for the provided round.
dummyCertifiedBlock :: Round -> CertifiedBlock 'P6
dummyCertifiedBlock r =
    CertifiedBlock
        { cbQuorumCertificate = qc,
          cbQuorumBlock = dummyBlock 0
        }
  where
    -- A quorum certificate for a provided round.
    -- The qc otherwise contains dummy values.
    qc =
        QuorumCertificate
            { qcBlock = BlockHash minBound,
              qcRound = r,
              qcEpoch = 0,
              qcAggregateSignature = mempty,
              qcSignatories = FinalizerSet 0
            }

-- |Checking that advancing rounds via a quorum certificate results
-- in the expected state.
testAdvanceByQuorum :: Spec
testAdvanceByQuorum = describe "Advance by QuorumCertificate" $ do
    it "A new quorum certificate advances the round" $ do
        runTestMonad @'P6 noBaker theTime genesisData $ do
            currentRound <- use $ roundStatus . rsCurrentRound
            advanceRoundWithQuorum $ dummyCertifiedBlock 41
            newRound <- use $ roundStatus . rsCurrentRound
            liftIO $
                assertBool
                    "Current round should be different"
                    (currentRound /= newRound)
            liftIO $
                assertEqual
                    "Current round should have progessed to round 42."
                    42
                    newRound
            highestCertifiedBlock <- use $ roundStatus . rsHighestCertifiedBlock
            liftIO $
                assertEqual
                    "Highest certified block should have been updated"
                    (dummyCertifiedBlock 41)
                    highestCertifiedBlock
            previousRoundTimeout <- use $ roundStatus . rsPreviousRoundTimeout
            liftIO $
                assertEqual
                    "Previous round timeout should be absent"
                    Absent
                    previousRoundTimeout
            eligibleToBake <- use $ roundStatus . rsRoundEligibleToBake
            liftIO $
                assertEqual
                    "Consensus runner should be eligible to bake for the new round"
                    True
                    eligibleToBake
  where
    noBaker = BakerContext Nothing
    theTime = timestampToUTCTime 1

-- |Checking that advancing a round via a timeout results
-- in the expected state.
testAdvanceByTimeout :: Spec
testAdvanceByTimeout = describe "Advance round by TimeoutCertificate." $ do
    it "Advancing by TimeoutCertificate makes the consensus eligible for baking (tc and certified block is new)" $ do
        runTestMonad @'P6 noBaker theTime genesisData $ do
            currentRound <- use $ roundStatus . rsCurrentRound
            advanceRoundWithTimeout $ roundTimeout 41 1
            newRound <- use $ roundStatus . rsCurrentRound
            liftIO $
                assertBool
                    "Current round should have changed"
                    (currentRound /= newRound)
            liftIO $
                assertEqual
                    "Round should have advanced"
                    42
                    newRound
            eligibleToBake <- use $ roundStatus . rsRoundEligibleToBake
            liftIO $
                assertEqual
                    "Consensus runner should be eligible to bake for the new round"
                    True
                    eligibleToBake
            newHighestCertifiedBlock <- use $ roundStatus . rsHighestCertifiedBlock
            liftIO $
                assertEqual
                    "Highest certified block should have been updated"
                    (dummyCertifiedBlock 1)
                    newHighestCertifiedBlock
            previousRoundTimeout <- use $ roundStatus . rsPreviousRoundTimeout
            liftIO $
                assertEqual
                    "Previous round timeout should be present"
                    (Present $ roundTimeout 41 1)
                    previousRoundTimeout
    it "Advancing by TimeoutCertificate makes the consensus eligible for baking (certified block is not new)" $ do
        runTestMonad @'P6 noBaker theTime genesisData $ do
            oldHighestCertifiedBlock <- use $ roundStatus . rsHighestCertifiedBlock
            advanceRoundWithTimeout $ roundTimeout 41 0
            newHighestCertifiedBlock <- use $ roundStatus . rsHighestCertifiedBlock
            liftIO $
                assertEqual
                    "Highest certified block should not have been updated"
                    oldHighestCertifiedBlock
                    newHighestCertifiedBlock
  where
    noBaker = BakerContext Nothing
    theTime = timestampToUTCTime 1
    -- A certified block for a particular round with
    -- a dummy block pointer.
    roundTimeout r cbr =
        RoundTimeout
            { rtTimeoutCertificate = tc r,
              rtCertifiedBlock = dummyCertifiedBlock cbr
            }
    -- A quorum certificate for a provided round.
    -- The c otherwise contains dummy values.
    tc r =
        TimeoutCertificate
            { tcRound = Round r,
              tcMinEpoch = 0,
              tcFinalizerQCRoundsFirstEpoch = FinalizerRounds Map.empty,
              tcFinalizerQCRoundsSecondEpoch = FinalizerRounds Map.empty,
              tcAggregateSignature = mempty
            }

tests :: Spec
tests = describe "KonsensusV1.Consensus" $ do
    testAdvanceByQuorum
    testAdvanceByTimeout
