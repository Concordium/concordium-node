-- |Module testing functions from the 'Concordium.KonsensusV1.Quorum' module.
module ConcordiumTests.KonsensusV1.Quorum where

import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import Concordium.Types

import Concordium.KonsensusV1.Consensus.Quorum
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

import ConcordiumTests.KonsensusV1.Types

-- |Generate a random 'VoterPower'.
genFinalizerWeight :: Gen VoterPower
genFinalizerWeight = VoterPower <$> arbitrary

-- |Generate a 'QuorumMessage' for a particular block.
genQuorumMessageFor :: BlockHash -> Gen QuorumMessage
genQuorumMessageFor bh = do
    qmSignature <- genQuorumSignature
    qmFinalizerIndex <- genFinalizerIndex
    qmRound <- genRound
    qmEpoch <- genEpoch
    return QuorumMessage{qmBlock = bh, ..}

propAddQuorumMessage :: Property
propAddQuorumMessage =
    forAll genQuorumMessage $ \qm0 ->
        forAll (genQuorumMessageFor (qmBlock qm0)) $ \qm1 ->
            (qm0 /= qm1) ==>
                forAll genFinalizerWeight $ \weight -> do
                    let qsm' = addQuorumMessage weight qm0 emptyQuorumMessages
                    assertEqual
                        "The quorum message should have been added"
                        (qsm' ^? smFinalizerToQuorumMessage . ix (qmFinalizerIndex qm0))
                        (Just qm0)
                    assertBool
                        "The block hash can be looked up"
                        (isJust (qsm' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm0)))
                    assertEqual
                        "The finalizer weight, signature and finalizer index should be present"
                        (weight, qmSignature qm0, Set.singleton $ qmFinalizerIndex qm0)
                        (fromJust $! qsm' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm0))
                    let qsm'' = addQuorumMessage weight qm1 qsm'
                    assertEqual
                        "The quorum message should have been added"
                        (qsm'' ^? smFinalizerToQuorumMessage . ix (qmFinalizerIndex qm1))
                        (Just qm1)
                    assertBool
                        "The block hash can be looked up"
                        (isJust (qsm'' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm1)))
                    assertEqual
                        "The finalizer weight, aggregated signature and finalizer indecies should be present"
                        (2 * weight, qmSignature qm1 <> qmSignature qm0, Set.insert (qmFinalizerIndex qm1) (Set.singleton $ qmFinalizerIndex qm0))
                        (fromJust $! qsm'' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm1))

tests :: Spec
tests = describe "KonsensusV1.Quorum" $ do
    it "Adding a quorum message" propAddQuorumMessage
