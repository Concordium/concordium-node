{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module ConcordiumTests.KonsensusV1.Consensus.Blocks (tests) where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.DummyData as Dummy
import Concordium.Genesis.Data
import Concordium.Types
import qualified Concordium.Types.DummyData as Dummy
import Concordium.Types.HashableTo
import Concordium.Types.SeedState
import Concordium.Types.Transactions

import qualified Concordium.Genesis.Data.P6 as P6
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Blocks
import Concordium.KonsensusV1.LeaderElection
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Startup
import Concordium.Types.BakerIdentity
import Data.Maybe
import Data.Time
import Lens.Micro.Platform

maxBaker :: Integral a => a
maxBaker = 5

genTime :: Timestamp
genTime = 0

genEpochDuration :: Duration
genEpochDuration = 3_600_000

genesisData :: GenesisData 'P6
bakers :: [(BakerIdentity, FullBakerInfo)]
(genesisData, bakers, _) =
    makeGenesisDataV1
        genTime
        (maxBaker + 1)
        genEpochDuration
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

-- |Hash of the genesis block.
genesisHash :: BlockHash
genesisHash = genesisBlockHash genesisData

-- |Leadership election nonce at genesis
genesisLEN :: LeadershipElectionNonce
genesisLEN = genesisLeadershipElectionNonce $ P6.genesisInitialState $ unGDP6 genesisData

-- |Full bakers at genesis
genesisFullBakers :: FullBakers
genesisFullBakers = FullBakers{..}
  where
    fullBakerInfos = Vec.fromList $ snd <$> bakers
    bakerTotalStake = sum $ _bakerStake <$> fullBakerInfos

-- |Seed state at genesis
genesisSeedState :: SeedState 'SeedStateVersion1
genesisSeedState = initialSeedStateV1 genesisLEN (addDuration genTime genEpochDuration)

bakerKey :: Integral a => a -> BakerSignPrivateKey
bakerKey i = bakerSignKey $ fst (bakers !! fromIntegral i)

bakerVRFKey :: Integral a => a -> BakerElectionPrivateKey
bakerVRFKey i = bakerElectionKey $ fst (bakers !! fromIntegral i)

bakerAggKey :: Integral a => a -> BakerAggregationPrivateKey
bakerAggKey i = bakerAggregationKey $ fst (bakers !! fromIntegral i)

theFinalizers :: [Int]
theFinalizers = [0 .. maxBaker]

-- |Finalizer set of all finalizers.
allFinalizers :: FinalizerSet
allFinalizers = finalizerSet $ FinalizerIndex <$> [0 .. maxBaker]

validQCFor :: BakedBlock -> QuorumCertificate
validQCFor bb =
    QuorumCertificate
        { qcSignatories = allFinalizers,
          qcRound = bbRound bb,
          qcEpoch = bbEpoch bb,
          qcBlock = block,
          qcAggregateSignature = sig
        }
  where
    block = getHash bb
    qsm =
        QuorumSignatureMessage
            { qsmGenesis = genesisHash,
              qsmBlock = block,
              qsmRound = bbRound bb,
              qsmEpoch = bbEpoch bb
            }
    sig = fold [signQuorumSignatureMessage qsm (bakerAggKey i) | i <- theFinalizers]

validSignBlock :: BakedBlock -> SignedBlock
validSignBlock bb = signBlock (bakerKey (bbBaker bb)) genesisHash bb

invalidSignBlock :: BakedBlock -> SignedBlock
invalidSignBlock bb = signBlock (bakerKey (bbBaker bb)) (getHash bb) bb

-- |Create a valid timeout message given a QC and a round.
-- All finalizers sign the certificate and they all have the QC as their highest QC.
validTimeoutFor :: QuorumCertificate -> Round -> TimeoutCertificate
validTimeoutFor qc rnd =
    TimeoutCertificate
        { tcRound = rnd,
          tcMinEpoch = qcEpoch qc,
          tcFinalizerQCRoundsFirstEpoch = FinalizerRounds (Map.singleton (qcRound qc) allFinalizers),
          tcFinalizerQCRoundsSecondEpoch = FinalizerRounds Map.empty,
          tcAggregateSignature =
            fold
                [signTimeoutSignatureMessage tsm (bakerAggKey i) | i <- theFinalizers]
        }
  where
    tsm =
        TimeoutSignatureMessage
            { tsmGenesis = genesisHash,
              tsmRound = rnd,
              tsmQCRound = qcRound qc,
              tsmQCEpoch = qcEpoch qc
            }

-- |Valid block for round 1.
testBB1 :: BakedBlock
testBB1 =
    BakedBlock
        { bbRound = 1,
          bbEpoch = 0,
          bbTimestamp = 1_000,
          bbBaker = bakerId,
          bbQuorumCertificate = genesisQuorumCertificate genesisHash,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 1 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "a8c8ba5539e9f257a23b1f9457de32330e2957f9d6e6b5f8abdee1ce8f5d173d"
        }
  where
    bakerId = 2

-- |Valid block for round 2, descended from 'testBB1'.
testBB2 :: BakedBlock
testBB2 =
    BakedBlock
        { bbRound = 2,
          bbEpoch = 0,
          bbTimestamp = 2_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB1,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 2 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "0410e11225a07abbb8d543f4a90c3e70f2a9d9a38b0f6493ab9353848b00ed7a"
        }
  where
    bakerId = 4

-- |Valid block for round 3, descended from 'testBB2'.
testBB3 :: BakedBlock
testBB3 =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 0,
          bbTimestamp = 3_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB2,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 3 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "81b3eb5d59df684f29887ec7cb6d75ebcd77bfe61aa9c519157912dd777ca366"
        }
  where
    bakerId = 4

-- |A valid block for round 2 where round 1 timed out.
testBB2' :: BakedBlock
testBB2' =
    testBB2
        { bbQuorumCertificate = genQC,
          bbTimeoutCertificate = Present (validTimeoutFor genQC 1),
          bbStateHash = read "0f6abbc518b1052319601591a805d815f410a5da63d49030259678436c93339c"
        }
  where
    genQC = genesisQuorumCertificate genesisHash

-- |A valid block for round 3 descended from 'testBB2''.
testBB3' :: BakedBlock
testBB3' =
    testBB3
        { bbQuorumCertificate = validQCFor testBB2',
          bbStateHash = read "eebc524ccbabe3001f5e64bce9af4a9da044c64c1cead31f9aabfcd6a39833da"
        }

-- |A valid block for round 4 descended from 'testBB3''.
testBB4' :: BakedBlock
testBB4' =
    BakedBlock
        { bbRound = 4,
          bbEpoch = 0,
          bbTimestamp = 4_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB3',
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 4 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "f1e0b621419dc113a5223d49e24a67bfecc9e5f7369ba412b54b961b31a48459"
        }
  where
    bakerId = 3

testBB3'' :: BakedBlock
testBB3'' =
    testBB3
        { bbQuorumCertificate = genQC,
          bbTimeoutCertificate = Present (validTimeoutFor genQC 2),
          bbStateHash = read "2b81e5943112b9a9916e57980a4b17b5b3b329eba0402d14201bfe1c9551a16d"
        }
  where
    genQC = genesisQuorumCertificate genesisHash

-- |Valid block for round 1.
-- This should be past the epoch transition trigger time.
testBB1E :: BakedBlock
testBB1E =
    BakedBlock
        { bbRound = 1,
          bbEpoch = 0,
          bbTimestamp = 3_600_000,
          bbBaker = bakerId,
          bbQuorumCertificate = genesisQuorumCertificate genesisHash,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 1 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "ac8e0ac03625706bc350b65557aaae1774d687810c9029d85db0e4ffcd229f56"
        }
  where
    bakerId = 2

-- |Valid block for round 2.
testBB2E :: BakedBlock
testBB2E =
    BakedBlock
        { bbRound = 2,
          bbEpoch = 0,
          bbTimestamp = 3_601_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB1E,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 2 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "ac8e0ac03625706bc350b65557aaae1774d687810c9029d85db0e4ffcd229f56"
        }
  where
    bakerId = 4

-- |Epoch finalization entry based on QCs for 'testBB1E' and 'testBB2E'.
testEpochFinEntry :: FinalizationEntry
testEpochFinEntry =
    FinalizationEntry
        { feFinalizedQuorumCertificate = validQCFor testBB1E,
          feSuccessorQuorumCertificate = validQCFor testBB2E,
          feSuccessorProof = getHash testBB2E
        }

-- |Epoch leadership election nonce for epoch 1, assuming that block 'testBB1E' is finalized.
testEpochLEN :: LeadershipElectionNonce
testEpochLEN = nonceForNewEpoch genesisFullBakers $ upd testBB1E genesisSeedState
  where
    upd b = updateSeedStateForBlock (bbTimestamp b) (bbNonce b)

testBB3E :: BakedBlock
testBB3E =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 1,
          bbTimestamp = 3_602_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB2E,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Present testEpochFinEntry,
          bbNonce = computeBlockNonce testEpochLEN 3 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "15ea86764d286c8712be0eb581849cd030014fbf702b943afcff7d7636c97745"
        }
  where
    bakerId = 5

testBB3E' :: BakedBlock
testBB3E' =
    testBB3E
        { bbQuorumCertificate = validQCFor testBB1E,
          bbTimeoutCertificate = Present (validTimeoutFor (validQCFor testBB1E) 2)
        }

testBB4E :: BakedBlock
testBB4E =
    BakedBlock
        { bbRound = 4,
          bbEpoch = 1,
          bbTimestamp = 3_603_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB3E,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce testEpochLEN 4 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "16bf4b495a3db3801ec60f5983818f68a2ec2763f6799f3a6b8da4aa5e81b64e"
        }
  where
    bakerId = 1

testBB5E' :: BakedBlock
testBB5E' =
    BakedBlock
        { bbRound = rnd,
          bbEpoch = 1,
          bbTimestamp = 3_604_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB3E,
          bbTimeoutCertificate = Present tc,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce testEpochLEN rnd (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "e2eb8a6bfed0aa4185bdde80dd0db086da6ea9630c2114d17b4923a7d1f90be0"
        }
  where
    bakerId = 2
    rnd = 5
    qc1 = validQCFor testBB2E
    qc2 = validQCFor testBB3E
    tc =
        TimeoutCertificate
            { tcRound = rnd - 1,
              tcMinEpoch = 0,
              tcFinalizerQCRoundsFirstEpoch = FinalizerRounds (Map.singleton (qcRound qc1) (finalizerSet $ FinalizerIndex <$> finsA)),
              tcFinalizerQCRoundsSecondEpoch = FinalizerRounds (Map.singleton (qcRound qc2) (finalizerSet $ FinalizerIndex <$> finsB)),
              tcAggregateSignature =
                fold $
                    [signTimeoutSignatureMessage (tsm qc1) (bakerAggKey i) | i <- finsA]
                        ++ [signTimeoutSignatureMessage (tsm qc2) (bakerAggKey i) | i <- finsB]
            }
    tsm qc =
        TimeoutSignatureMessage
            { tsmGenesis = genesisHash,
              tsmRound = rnd - 1,
              tsmQCRound = qcRound qc,
              tsmQCEpoch = qcEpoch qc
            }
    finsA = take 3 [0 .. maxBaker]
    finsB = drop 3 [0 .. maxBaker]

testBB2Ex :: BakedBlock
testBB2Ex =
    testBB2E
        { bbQuorumCertificate = genQC,
          bbTimeoutCertificate = Present (validTimeoutFor genQC 1),
          bbStateHash = read "4821535805ebb2cbb0a17440be27ea85992fa508e829ccb5aeee124f5968095a"
        }
  where
    genQC = genesisQuorumCertificate genesisHash

-- |Epoch leadership election nonce for epoch 1, assuming that block 'testBB2Ex' is finalized.
testEpochLENx :: LeadershipElectionNonce
testEpochLENx = nonceForNewEpoch genesisFullBakers $ upd testBB2Ex genesisSeedState
  where
    upd b = updateSeedStateForBlock (bbTimestamp b) (bbNonce b)

testBB3Ex :: BakedBlock
testBB3Ex =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 1,
          bbTimestamp = 3_602_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB2Ex,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Present testEpochFinEntry,
          bbNonce = computeBlockNonce testEpochLENx 3 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "15ea86764d286c8712be0eb581849cd030014fbf702b943afcff7d7636c97745"
        }
  where
    bakerId = 2

succeedReceiveBlock :: PendingBlock -> TestMonad 'P6 ()
succeedReceiveBlock pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultSuccess vb -> do
            executeBlock vb
            status <- getBlockStatus (getHash pb) =<< get
            case status of
                BlockAlive _ -> return ()
                BlockFinalized _ -> return ()
                _ -> liftIO . assertFailure $ "Expected BlockAlive after executeBlock, but found: " ++ show status ++ "\n" ++ show pb
        _ -> liftIO . assertFailure $ "Expected BlockResultSuccess after uponReceivingBlock, but found: " ++ show res ++ "\n" ++ show pb

duplicateReceiveBlock :: PendingBlock -> TestMonad 'P6 ()
duplicateReceiveBlock pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultDoubleSign vb -> do
            executeBlock vb
            status <- getBlockStatus (getHash pb) =<< get
            case status of
                BlockAlive _ -> return ()
                _ -> liftIO . assertFailure $ "Expected BlockAlive after executeBlock, but found: " ++ show status ++ "\n" ++ show pb
        _ -> liftIO . assertFailure $ "Expected BlockResultDoubleSign after uponReceivingBlock, but found: " ++ show res ++ "\n" ++ show pb

succeedReceiveBlockFailExecute :: PendingBlock -> TestMonad 'P6 ()
succeedReceiveBlockFailExecute pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultSuccess vb -> do
            executeBlock vb
            status <- getBlockStatus (getHash pb) =<< get
            case status of
                BlockUnknown -> return ()
                BlockDead -> return ()
                _ -> liftIO . assertFailure $ "Expected BlockUnknown or BlockDead after executeBlock, but found: " ++ show status ++ "\n" ++ show pb
        _ -> liftIO . assertFailure $ "Expected BlockResultSuccess after uponReceivingBlock, but found: " ++ show res ++ "\n" ++ show pb

duplicateReceiveBlockFailExecute :: PendingBlock -> TestMonad 'P6 ()
duplicateReceiveBlockFailExecute pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultDoubleSign vb -> do
            executeBlock vb
            status <- getBlockStatus (getHash pb) =<< get
            case status of
                BlockUnknown -> return ()
                BlockDead -> return ()
                _ -> liftIO . assertFailure $ "Expected BlockUnknown or BlockDead after executeBlock, but found: " ++ show status ++ "\n" ++ show pb
        _ -> liftIO . assertFailure $ "Expected BlockResultDoubleSign after uponReceivingBlock, but found: " ++ show res ++ "\n" ++ show pb

pendingReceiveBlock :: PendingBlock -> TestMonad 'P6 ()
pendingReceiveBlock pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultPending -> return ()
        _ -> liftIO . assertFailure $ "Expected BlockResultPending after uponReceivingBlock, but found: " ++ show res

staleReceiveBlock :: PendingBlock -> TestMonad 'P6 ()
staleReceiveBlock sb = do
    res <- uponReceivingBlock sb
    liftIO $ res `shouldBe` BlockResultStale

invalidReceiveBlock :: PendingBlock -> TestMonad 'P6 ()
invalidReceiveBlock sb = do
    res <- uponReceivingBlock sb
    liftIO $ res `shouldBe` BlockResultInvalid

earlyReceiveBlock :: PendingBlock -> TestMonad 'P6 ()
earlyReceiveBlock sb = do
    res <- uponReceivingBlock sb
    liftIO $ res `shouldBe` BlockResultEarly

checkLive :: HashableTo BlockHash b => b -> TestMonad 'P6 ()
checkLive b =
    get >>= getBlockStatus bh >>= \case
        BlockAlive _ -> return ()
        status ->
            liftIO . assertFailure $
                "Expected BlockFinalized for block "
                    ++ show bh
                    ++ " but found: "
                    ++ show status
  where
    bh = getHash b

checkFinalized :: HashableTo BlockHash b => b -> TestMonad 'P6 ()
checkFinalized b =
    get >>= getBlockStatus bh >>= \case
        BlockFinalized _ -> return ()
        status ->
            liftIO . assertFailure $
                "Expected BlockFinalized for block "
                    ++ show bh
                    ++ " but found: "
                    ++ show status
  where
    bh = getHash b

checkDead :: HashableTo BlockHash b => b -> TestMonad 'P6 ()
checkDead b =
    get >>= getBlockStatus bh >>= \case
        BlockDead -> return ()
        status ->
            liftIO . assertFailure $
                "Expected BlockDead for block "
                    ++ show bh
                    ++ " but found: "
                    ++ show status
  where
    bh = getHash b

signedPB :: BakedBlock -> PendingBlock
signedPB bb =
    PendingBlock
        { pbReceiveTime = timestampToUTCTime $ bbTimestamp bb,
          pbBlock = validSignBlock bb
        }

-- |Baker context with no baker.
noBaker :: BakerContext
noBaker = BakerContext Nothing

-- |Current time used for running (some) tests. 5 seconds after genesis.
testTime :: UTCTime
testTime = timestampToUTCTime 5_000

-- |Receive 3 valid blocks in consecutive rounds.
testReceive3 :: Assertion
testReceive3 = runTestMonad noBaker testTime genesisData $ do
    let b1 = signedPB testBB1
    succeedReceiveBlock b1
    let b2 = signedPB testBB2
    succeedReceiveBlock b2
    let b3 = signedPB testBB3
    succeedReceiveBlock b3
    -- b3's QC is for b2, which has QC for b1, so b1 should now be finalized.
    checkFinalized b1

-- |Receive 3 valid blocks in consecutive rounds, but with the block for round 2 being received first.
testReceive3Reordered :: Assertion
testReceive3Reordered = runTestMonad noBaker testTime genesisData $ do
    let b2 = signedPB testBB2
    pendingReceiveBlock b2
    let b1 = signedPB testBB1
    succeedReceiveBlock b1
    let b3 = signedPB testBB3
    succeedReceiveBlock b3
    -- b3's QC is for b2, which has QC for b1, so b1 should now be finalized.
    checkFinalized b1

-- |Receive 4 valid blocks reordered across epochs.
-- Also receive 2 pending blocks that should get pruned.
testReceive4Reordered :: Assertion
testReceive4Reordered = runTestMonad noBaker testTime genesisData $ do
    pendingReceiveBlock (signedPB testBB4E)
    pendingReceiveBlock (signedPB testBB2E)
    pendingReceiveBlock (signedPB testBB3E)
    pendingReceiveBlock (signedPB testBB3')
    pendingReceiveBlock (signedPB testBB4')
    succeedReceiveBlock (signedPB testBB1E)
    checkFinalized testBB1E
    -- Note: testBB2E is not finalized because it is in a different epoch from testBB3E.
    checkLive testBB2E
    checkLive testBB3E
    checkLive testBB4E
    checkDead testBB3'
    checkDead testBB4'

-- |Receive 3 blocks where the first round is skipped due to timeout.
testReceiveWithTimeout :: Assertion
testReceiveWithTimeout = runTestMonad noBaker testTime genesisData $ do
    let b2' = signedPB testBB2'
    succeedReceiveBlock b2'
    succeedReceiveBlock $ signedPB testBB3'
    succeedReceiveBlock $ signedPB testBB4'
    checkFinalized b2'

-- |Receive a block twice.
testReceiveDuplicate :: Assertion
testReceiveDuplicate = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlock $ signedPB testBB1
    res <- uponReceivingBlock $ signedPB testBB1
    liftIO $ res `shouldBe` BlockResultDuplicate

testReceiveStale :: Assertion
testReceiveStale = runTestMonad noBaker testTime genesisData $ do
    mapM_ (succeedReceiveBlock . signedPB) [testBB2', testBB3', testBB4']
    res <- uponReceivingBlock (signedPB testBB1)
    liftIO $ res `shouldBe` BlockResultStale

testReceiveEpoch :: Assertion
testReceiveEpoch = runTestMonad noBaker testTime genesisData $ do
    mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB3E, testBB4E]

-- |Test that blocks are correctly marked dead when a block gets finalized.
-- Test that blocks arriving on dead branches are handled as stale.
testReceiveBlockDies :: Assertion
testReceiveBlockDies = runTestMonad noBaker testTime genesisData $ do
    -- Receive blocks testBB1, testBB2', testBB2 and testBB3.
    mapM_ (succeedReceiveBlock . signedPB) [testBB1, testBB2']
    -- (testBB2 and testBB2' are both in round 2, so the second is handled as a duplicate)
    duplicateReceiveBlock $ signedPB testBB2
    succeedReceiveBlock $ signedPB testBB3
    -- Receiving testBB3 finalizes testBB1, so testBB2' should now be dead
    checkDead testBB2'
    -- Check that blocks descending from a dead block (testBB3') and an old finalized block
    -- (testBB3'') fail.
    mapM_ (staleReceiveBlock . signedPB) [testBB3', testBB3'']

-- |Test receiving a block from future epochs.
testReceiveBadFutureEpoch :: Assertion
testReceiveBadFutureEpoch = runTestMonad noBaker testTime genesisData $ do
    invalidReceiveBlock $ signedPB (testBB1{bbEpoch = 500})
    invalidReceiveBlock $ signedPB (testBB1{bbEpoch = 2})

-- |Test receiving a block from a past epoch.
testReceiveBadPastEpoch :: Assertion
testReceiveBadPastEpoch = runTestMonad noBaker testTime genesisData $ do
    mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB3E]
    invalidReceiveBlock $ signedPB testBB4'{bbQuorumCertificate = bbQuorumCertificate testBB4E}

-- |Test receiving a block from future epochs.
testReceiveBadEpochTransition :: Assertion
testReceiveBadEpochTransition = runTestMonad noBaker testTime genesisData $ do
    invalidReceiveBlock $ signedPB (testBB1{bbEpoch = 1})

-- |Test receiving a block with an incorrect signature, followed by the same block with the
-- correct signature.
testReceiveBadSignature :: Assertion
testReceiveBadSignature = runTestMonad noBaker testTime genesisData $ do
    invalidReceiveBlock $
        PendingBlock
            { pbBlock = invalidSignBlock testBB1,
              pbReceiveTime = testTime
            }
    -- Receiving the same block with the correct signature should still succeed afterwards.
    succeedReceiveBlock $ signedPB testBB1

-- |Test receiving a block baked by the wrong baker.
testReceiveWrongBaker :: Assertion
testReceiveWrongBaker = runTestMonad noBaker testTime genesisData $ do
    let claimedBakerId = 0
    invalidReceiveBlock $
        signedPB
            testBB1
                { bbBaker = claimedBakerId,
                  bbNonce = computeBlockNonce genesisLEN 1 (bakerVRFKey claimedBakerId)
                }

-- |Test receiving an (out-of-order) block from the far future.
testReceiveEarlyUnknownParent :: Assertion
testReceiveEarlyUnknownParent = runTestMonad noBaker testTime genesisData $ do
    res <- uponReceivingBlock $ (signedPB testBB2E){pbReceiveTime = testTime}
    liftIO $ res `shouldBe` BlockResultEarly

-- |Test receiving a block with an unknown parent and incorrect signature, followed by the parent
-- block and the same block with the correct signature.
testReceiveBadSignatureUnknownParent :: Assertion
testReceiveBadSignatureUnknownParent = runTestMonad noBaker testTime genesisData $ do
    invalidReceiveBlock $
        PendingBlock
            { pbBlock = invalidSignBlock testBB2,
              pbReceiveTime = testTime
            }
    -- Receiving the same block with the correct signature should still succeed afterwards.
    mapM_ (succeedReceiveBlock . signedPB) [testBB1, testBB2]

-- |Test receiving a block with an unknown parent from a far distant epoch. (But the timestamp
-- is still in the present.)
testReceiveFutureEpochUnknownParent :: Assertion
testReceiveFutureEpochUnknownParent = runTestMonad noBaker testTime genesisData $ do
    earlyReceiveBlock $ signedPB testBB2{bbEpoch = 30}

-- |Test receiving a block where the QC is not consistent with the round of the parent block.
testReceiveInconsistentQCRound :: Assertion
testReceiveInconsistentQCRound = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlock $ signedPB testBB1
    succeedReceiveBlockFailExecute $
        signedPB
            testBB2
                { bbQuorumCertificate = (bbQuorumCertificate testBB2){qcRound = 0}
                }

-- |Test receiving a block where the QC is not consistent with the epoch of the parent block.
testReceiveInconsistentQCEpoch :: Assertion
testReceiveInconsistentQCEpoch = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlock $ signedPB testBB1
    succeedReceiveBlockFailExecute $
        signedPB
            testBB2{bbQuorumCertificate = (bbQuorumCertificate testBB2){qcEpoch = 1}}

-- |Test receiving a block where the round is earlier than or equal to the round of the parent
-- block.
testReceiveRoundInconsistent :: Assertion
testReceiveRoundInconsistent = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlock $ signedPB testBB2'
    -- Earlier round
    succeedReceiveBlockFailExecute $
        signedPB
            testBB1{bbQuorumCertificate = validQCFor testBB2'}
    -- Equal round
    duplicateReceiveBlockFailExecute $ signedPB testBB2{bbQuorumCertificate = validQCFor testBB2'}

-- |Test processing a block where the epoch is inconsistent with the parent block's epoch.
testProcessEpochInconsistent :: Assertion
testProcessEpochInconsistent = runTestMonad noBaker testTime genesisData $ do
    mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB3E]
    -- We skip 'uponReceivingBlock' and go straight to processing, because the condition would be
    -- caught in 'uponReceivingBlock'.
    let pb = signedPB testBB4'{bbQuorumCertificate = bbQuorumCertificate testBB4E}
    bf <- use $ epochBakers . currentEpochBakers
    executeBlock $
        VerifiedBlock
            { vbBlock = pb,
              vbBakersAndFinalizers = bf,
              vbBakerInfo = fromJust $ fullBaker genesisFullBakers (blockBaker pb),
              vbLeadershipElectionNonce = genesisLEN
            }
    status <- getBlockStatus (getHash pb) =<< get
    case status of
        BlockUnknown -> return ()
        BlockDead -> return ()
        _ -> liftIO . assertFailure $ "Expected BlockUnknown or BlockDead after executeBlock, but found: " ++ show status ++ "\n" ++ show pb

-- |Test receiving a block where the block nonce is incorrect.
testReceiveIncorrectNonce :: Assertion
testReceiveIncorrectNonce = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlockFailExecute $
        signedPB
            testBB1{bbNonce = computeBlockNonce genesisLEN 0 (bakerVRFKey (0 :: Int))}

-- |Test receiving a block where the block nonce is incorrect.
testReceiveTooFast :: Assertion
testReceiveTooFast = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlockFailExecute $
        signedPB
            testBB1{bbTimestamp = 10}

-- |Test receiving a block that should have a timeout certificate, but doesn't.
testReceiveMissingTC :: Assertion
testReceiveMissingTC = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlockFailExecute $
        signedPB
            testBB2'{bbTimeoutCertificate = Absent}

-- |Test receiving a block with a timeout certificate for an incorrect round.
testReceiveTCIncorrectRound :: Assertion
testReceiveTCIncorrectRound = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlockFailExecute $
        signedPB
            testBB2'{bbTimeoutCertificate = Present (validTimeoutFor genQC 2)}
  where
    genQC = genesisQuorumCertificate genesisHash

-- |Test receiving a block with a timeout certificate where none is expected.
testReceiveTCUnexpected :: Assertion
testReceiveTCUnexpected = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlockFailExecute $
        signedPB
            testBB1{bbTimeoutCertificate = Present (validTimeoutFor genQC 0)}
  where
    genQC = genesisQuorumCertificate genesisHash

-- |Test receiving a block with a timeout certificate that is inconsistent with the QC, in that
-- it has a more recent QC.
testReceiveTCInconsistent1 :: Assertion
testReceiveTCInconsistent1 = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlockFailExecute $
        signedPB
            testBB2'{bbTimeoutCertificate = Present (validTimeoutFor (validQCFor testBB1) 1)}

-- |Test receiving a block that is the start of a new epoch, but with the previous round
-- timing out.
testReceiveTimeoutPastEpoch :: Assertion
testReceiveTimeoutPastEpoch = runTestMonad noBaker testTime genesisData $ do
    mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB3E']

-- |Test receiving a block that is the start of a new epoch, but with an epoch finalization entry
-- for a different branch.
testReceiveFinalizationBranch :: Assertion
testReceiveFinalizationBranch = runTestMonad noBaker testTime genesisData $ do
    mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2Ex]
    succeedReceiveBlockFailExecute $ signedPB testBB3Ex
    checkFinalized testBB1E
    checkDead testBB2Ex

testReceiveEpochTransitionTimeout :: Assertion
testReceiveEpochTransitionTimeout = runTestMonad noBaker testTime genesisData $ do
    mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB3E, testBB5E']

testReceiveIncorrectTransactionOutcomesHash :: Assertion
testReceiveIncorrectTransactionOutcomesHash = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlockFailExecute $
        signedPB
            testBB1
                { bbTransactionOutcomesHash = read "0000000000000000000000000000000000000000000000000000000000000000"
                }

testReceiveIncorrectStateHash :: Assertion
testReceiveIncorrectStateHash = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlockFailExecute $
        signedPB
            testBB1
                { bbStateHash = read "0000000000000000000000000000000000000000000000000000000000000000"
                }

-- |Test receiving a block where the QC signature is invalid.
testReceiveInvalidQC :: Assertion
testReceiveInvalidQC = runTestMonad noBaker testTime genesisData $ do
    succeedReceiveBlock $ signedPB testBB1
    succeedReceiveBlockFailExecute $
        signedPB
            testBB2
                { bbQuorumCertificate = (bbQuorumCertificate testBB2){qcAggregateSignature = mempty}
                }

tests :: Spec
tests = describe "KonsensusV1.Consensus.Blocks" $ do
    describe "uponReceiveingBlock" $ do
        it "receive 3 consecutive blocks" testReceive3
        it "receive 3 blocks reordered" testReceive3Reordered
        it "receive 4 blocks reordered, multiple epochs" testReceive4Reordered
        it "skip round 1, receive rounds 2,3,4" testReceiveWithTimeout
        it "receive duplicate block" testReceiveDuplicate
        it "receive stale round" testReceiveStale
        it "epoch transition" testReceiveEpoch
        it "block dies on old branch" testReceiveBlockDies
        it "receive a block in a bad future epoch" testReceiveBadFutureEpoch
        it "receive a block in a bad past epoch" testReceiveBadPastEpoch
        it "receive a block in the next epoch where the transition is not allowed" testReceiveBadEpochTransition
        it "receive a block with a bad signature" testReceiveBadSignature
        it "receive a block from a baker that did not win the round" testReceiveWrongBaker
        it "receive a block from the future with an unknown parent" testReceiveEarlyUnknownParent
        it "receive a block with a bad signature and unknown parent" testReceiveBadSignatureUnknownParent
        it "receive a block with an unknown parent and epoch" testReceiveFutureEpochUnknownParent
        it "receive a block with QC round inconsistent with the parent block" testReceiveInconsistentQCRound
        it "receive a block with QC epoch inconsistent with the parent block" testReceiveInconsistentQCEpoch
        it "receive a block with round before parent block round" testReceiveRoundInconsistent
        it "process a block with epoch before parent block epoch" testProcessEpochInconsistent
        it "receive a block with incorrect block nonce" testReceiveIncorrectNonce
        it "receive a block with timestamp too soon after parent" testReceiveTooFast
        it "receive a block with a missing TC" testReceiveMissingTC
        it "receive a block with a TC for incorrect round" testReceiveTCIncorrectRound
        it "receive a block with an unexpected TC" testReceiveTCUnexpected
        it "receive a block with a QC behind the max QC of the TC" testReceiveTCInconsistent1
        it "receive a block with a timeout where the block is in a new epoch" testReceiveTimeoutPastEpoch
        it "receive a block with an epoch finalization certificate for a different branch" testReceiveFinalizationBranch
        it "receive a block with a timeout spanning epochs" testReceiveEpochTransitionTimeout
        it "receive a block with an incorrect transaction outcomes hash" testReceiveIncorrectTransactionOutcomesHash
        it "receive a block with an incorrect state hash" testReceiveIncorrectStateHash
        it "receive a block with an invalid QC signature" testReceiveInvalidQC
