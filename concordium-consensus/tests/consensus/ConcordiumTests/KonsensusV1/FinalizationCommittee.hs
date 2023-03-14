module ConcordiumTests.KonsensusV1.FinalizationCommittee where

import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Parameters
import Concordium.KonsensusV1.Consensus (computeFinalizationCommittee)
import Concordium.KonsensusV1.Types
import Concordium.Types
import Concordium.Types.Accounts

-- |A 'FullBakerInfo' with the given 'BakerId' and stake.
-- The keys are (deterministically) randomly generated.
dummyFullBakerInfo :: BakerId -> Amount -> FullBakerInfo
dummyFullBakerInfo bid stake = mkFullBaker (fromIntegral bid) bid ^. _1 & bakerStake .~ stake

-- |A test case for 'computeFinalizationCommittee'.
data FinalizationCommitteeTestCase = FinalizationCommitteeTestCase
    { -- |Description to show for this case.
      description :: String,
      -- |Stake for bakers [0..].
      bakerStakes :: [Amount],
      -- |Finalization committee parameters to use.
      parameters :: FinalizationCommitteeParameters,
      -- |The expected set of bakers in the resulting committee.
      expectedFinalizers :: [BakerId]
    }
    deriving (Show)

-- |Test 'computeFinalizationCommittee' with a given test case.
testFinalizationCommitteeTestCase :: FinalizationCommitteeTestCase -> Spec
testFinalizationCommitteeTestCase tc@FinalizationCommitteeTestCase{..} =
    it description $ assertEqual ("Finalizers for " ++ show tc) expect actual
  where
    fullBakerInfos = Vec.fromList (zipWith dummyFullBakerInfo [0 ..] bakerStakes)
    bakerTotalStake = sum $ view bakerStake <$> fullBakerInfos
    bakers = FullBakers{..}
    actual = computeFinalizationCommittee bakers parameters
    expectedFinBakers = filter ((`elem` expectedFinalizers) . view bakerIdentity) (Vec.toList fullBakerInfos)
    mkFinalizer finalizerIndex bi =
        FinalizerInfo
            { finalizerWeight = fromIntegral (bi ^. bakerStake),
              finalizerSignKey = bi ^. bakerSignatureVerifyKey,
              finalizerVRFKey = bi ^. bakerElectionVerifyKey,
              finalizerBlsKey = bi ^. bakerAggregationVerifyKey,
              finalizerBakerId = bi ^. bakerIdentity,
              ..
            }
    committeeFinalizers = Vec.fromList $ zipWith mkFinalizer [FinalizerIndex 0 ..] expectedFinBakers
    committeeTotalWeight = sum $ finalizerWeight <$> committeeFinalizers
    expect = FinalizationCommittee{..}

tests :: Spec
tests = describe "KonsensusV1.FinalizationCommittee" $ do
    describe "computeFinalizationCommittee" $
        mapM_
            testFinalizationCommitteeTestCase
            [ FinalizationCommitteeTestCase
                { description = "limit max",
                  bakerStakes = [100, 200],
                  parameters =
                    FinalizationCommitteeParameters
                        { _fcpMinFinalizers = 1,
                          _fcpMaxFinalizers = 1,
                          _fcpFinalizerRelativeStakeThreshold = PartsPerHundredThousands 100
                        },
                  expectedFinalizers = [1]
                },
              FinalizationCommitteeTestCase
                { description = "both pass",
                  bakerStakes = [100, 200],
                  parameters =
                    FinalizationCommitteeParameters
                        { _fcpMinFinalizers = 1,
                          _fcpMaxFinalizers = 2,
                          _fcpFinalizerRelativeStakeThreshold = PartsPerHundredThousands 100
                        },
                  expectedFinalizers = [0, 1]
                },
              FinalizationCommitteeTestCase
                { description = "limit max - baker ID tiebreak",
                  bakerStakes = [100, 200, 100],
                  parameters =
                    FinalizationCommitteeParameters
                        { _fcpMinFinalizers = 1,
                          _fcpMaxFinalizers = 2,
                          _fcpFinalizerRelativeStakeThreshold = PartsPerHundredThousands 25000
                        },
                  expectedFinalizers = [0, 1]
                },
              FinalizationCommitteeTestCase
                { description = "limit max - baker ID tiebreak (2)",
                  bakerStakes = [100, 100, 100, 100],
                  parameters =
                    FinalizationCommitteeParameters
                        { _fcpMinFinalizers = 1,
                          _fcpMaxFinalizers = 2,
                          _fcpFinalizerRelativeStakeThreshold = PartsPerHundredThousands 25000
                        },
                  expectedFinalizers = [0, 1]
                },
              FinalizationCommitteeTestCase
                { description = "threshold - just over",
                  bakerStakes = [100, 100, 100, 101],
                  parameters =
                    FinalizationCommitteeParameters
                        { _fcpMinFinalizers = 1,
                          _fcpMaxFinalizers = 2,
                          _fcpFinalizerRelativeStakeThreshold = PartsPerHundredThousands 25000
                        },
                  expectedFinalizers = [3]
                },
              FinalizationCommitteeTestCase
                { description = "limit min",
                  bakerStakes = [100, 100, 100, 101],
                  parameters =
                    FinalizationCommitteeParameters
                        { _fcpMinFinalizers = 2,
                          _fcpMaxFinalizers = 3,
                          _fcpFinalizerRelativeStakeThreshold = PartsPerHundredThousands 25000
                        },
                  expectedFinalizers = [3, 0]
                },
              FinalizationCommitteeTestCase
                { description = "all pass",
                  bakerStakes = [100, 100, 100, 101],
                  parameters =
                    FinalizationCommitteeParameters
                        { _fcpMinFinalizers = 2,
                          _fcpMaxFinalizers = 5,
                          _fcpFinalizerRelativeStakeThreshold = PartsPerHundredThousands 2500
                        },
                  expectedFinalizers = [0, 1, 2, 3]
                }
            ]
