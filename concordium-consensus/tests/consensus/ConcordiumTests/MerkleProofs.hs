{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ConcordiumTests.MerkleProofs where

import qualified Data.ByteString as BS
import Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import Data.Serialize
import Test.Hspec
import Test.QuickCheck

import Concordium.MerkleProofs
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Parameters

import Concordium.KonsensusV1.Types
import Concordium.Types.Option

import ConcordiumTests.KonsensusV1.Common
import ConcordiumTests.KonsensusV1.Consensus.Blocks
import ConcordiumTests.KonsensusV1.Types (
    genBakedBlock,
    genQuorumCertificate,
    genTimeoutCertificate,
 )

-- | Test that the root hash of a quorum certificate Merkle proof matches the hash of the QC.
propQCMerkleProofMatchesHash :: Property
propQCMerkleProofMatchesHash = forAll genQuorumCertificate $ \qc ->
    getHash qc === toRootHash (runIdentity (buildMerkleProof (const True) qc))

-- | Test that the root hash of a timeout certificate Merkle proof matches the hash of the TC.
propTCMerkleProofMatchesHash :: Property
propTCMerkleProofMatchesHash = forAll genTimeoutCertificate $ \tc ->
    getHash (Present tc) === toRootHash (runIdentity (buildMerkleProof (const True) (Present tc)))

-- | Test that the root hash of a baked block Merkle proof matches the block hash.
propBBMerkleProofMatchesHash ::
    (IsProtocolVersion pv, BlockHashVersionFor pv ~ 'BlockHashVersion1) =>
    SProtocolVersion pv ->
    Property
propBBMerkleProofMatchesHash spv = forAll (genBakedBlock spv) $ \bb ->
    blockHash (getHash bb) === toRootHash (runIdentity (buildMerkleProof (const True) bb))

-- | Test that parsing a (full) baked block Merkle proof gives the expected structure.
propBBMerkleProofParse ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv, BlockHashVersionFor pv ~ 'BlockHashVersion1) =>
    SProtocolVersion pv ->
    Property
propBBMerkleProofParse spv =
    conjoin (theTest <$> [testBB1, testBB2, testBB3, testBB2', testBB3', testBB4', testBB3'', testBB1E, testBB2E, testBB3EX])
        .&&. forAll (genBakedBlock spv) theTest
  where
    theTest :: BakedBlock pv -> Property
    theTest bb@BakedBlock{..} =
        let proof = runIdentity (buildMerkleProof (const True) bb)
        in  case uncurry parseMerkleProof (blockSchema $ demoteProtocolVersion spv) proof of
                Left err -> counterexample ("Failed to parse proof" ++ show err) False
                Right (pt, hsh) ->
                    blockHash (getHash bb) === hsh
                        .&&. pt
                            === ( HM.singleton "root" . Node . HM.fromList $
                                    [   ( "header",
                                          Node
                                            ( HM.fromList
                                                [ ("epoch", Leaf (encode bbEpoch)),
                                                  ("parent", Leaf (encode (qcBlock bbQuorumCertificate))),
                                                  ("round", Leaf (encode bbRound))
                                                ]
                                            )
                                        ),
                                        ( "quasi",
                                          Node
                                            ( HM.fromList
                                                [   ( "data",
                                                      Node
                                                        ( HM.fromList
                                                            [ ("transactions", Leaf (encode (computeTransactionsHash SBlockHashVersion1 bbTransactions))),
                                                              ("result", Leaf (encode (case bbDerivableHashes of DerivableBlockHashesV1{..} -> dbhv1BlockResultHash)))
                                                            ]
                                                        )
                                                    ),
                                                    ( "meta",
                                                      Node
                                                        ( HM.fromList
                                                            [   ( "certificatesHash",
                                                                  Node
                                                                    ( HM.fromList
                                                                        [   ( "timeoutFinalization",
                                                                              Node
                                                                                ( HM.fromList
                                                                                    [ ("epochFinalizationEntry", Node finEntry),
                                                                                      ("timeoutCertificate", Node timeoutCert)
                                                                                    ]
                                                                                )
                                                                            ),
                                                                          ("quorumCertificate", Node quorumCert)
                                                                        ]
                                                                    )
                                                                ),
                                                                ( "bakerInfo",
                                                                  Node
                                                                    ( HM.fromList
                                                                        [   ( "nonce",
                                                                              Node
                                                                                ( HM.fromList
                                                                                    [ ("blockNonce", Leaf (encode bbNonce))
                                                                                    ]
                                                                                )
                                                                            ),
                                                                            ( "timestampBaker",
                                                                              Node
                                                                                ( HM.fromList
                                                                                    [ ("bakerId", Leaf (encode bbBaker)),
                                                                                      ("timestamp", Leaf (encode bbTimestamp))
                                                                                    ]
                                                                                )
                                                                            )
                                                                        ]
                                                                    )
                                                                )
                                                            ]
                                                        )
                                                    )
                                                ]
                                            )
                                        )
                                    ]
                                )
                  where
                    finalizerQCRoundsFor rounds =
                        Node . HM.fromList . zip [show n | n <- [0 :: Integer ..]] $
                            ( \(rnd, finSet) ->
                                Node (HM.fromList [("round", Leaf (encode rnd)), ("finalizers", Leaf (encodeFinSet finSet))])
                            )
                                <$> finalizerRoundsList rounds
                    encodeFinSet finSet = BS.drop 4 (encode finSet)
                    timeoutCert = case bbTimeoutCertificate of
                        Absent -> HM.fromList [("null", Leaf "")]
                        Present TimeoutCertificate{..} ->
                            HM.fromList
                                [ ("round", Leaf (encode tcRound)),
                                  ("minEpoch", Leaf (encode tcMinEpoch)),
                                  ("finalizerQCRoundsFirstEpoch", finalizerQCRoundsFor tcFinalizerQCRoundsFirstEpoch),
                                  ("finalizerQCRoundsSecondEpoch", finalizerQCRoundsFor tcFinalizerQCRoundsSecondEpoch),
                                  ("aggregateSignature", Leaf (encode tcAggregateSignature))
                                ]
                    finEntry = case bbEpochFinalizationEntry of
                        Absent -> HM.fromList [("null", Leaf "")]
                        Present FinalizationEntry{..} ->
                            HM.fromList
                                [ ("finalizedBlock", Leaf (encode (qcBlock feFinalizedQuorumCertificate))),
                                  ("finalizedRound", Leaf (encode (qcRound feFinalizedQuorumCertificate))),
                                  ("epoch", Leaf (encode (qcEpoch feFinalizedQuorumCertificate))),
                                  ("finalizedAggregateSignature", Leaf (encode (qcAggregateSignature feFinalizedQuorumCertificate))),
                                  ("finalizedSignatories", Leaf (encodeFinSet (qcSignatories feFinalizedQuorumCertificate))),
                                  ("successorAggregateSignature", Leaf (encode (qcAggregateSignature feSuccessorQuorumCertificate))),
                                  ("successorSignatories", Leaf (encodeFinSet (qcSignatories feSuccessorQuorumCertificate))),
                                  ("successorProof", Leaf (encode feSuccessorProof))
                                ]
                    quorumCert =
                        let QuorumCertificate{..} = bbQuorumCertificate
                        in  HM.fromList
                                [ ("block", Leaf (encode qcBlock)),
                                  ("round", Leaf (encode qcRound)),
                                  ("epoch", Leaf (encode qcEpoch)),
                                  ("aggregateSignature", Leaf (encode qcAggregateSignature)),
                                  ("signatories", Leaf (encodeFinSet qcSignatories))
                                ]

tests :: Spec
tests = describe "MerkleProofs" $ parallel $ do
    it "Check hash result for QuorumCertificate" propQCMerkleProofMatchesHash
    it "Check hash result for TimeoutCertificate" propTCMerkleProofMatchesHash
    forEveryProtocolVersionBHV1 $ \spv pvString -> describe pvString $ do
        it "Check hash result for BakedBlock" (propBBMerkleProofMatchesHash spv)
        it "Correct parse of BakedBlock proof" (propBBMerkleProofParse spv)
