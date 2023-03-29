{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module ConcordiumTests.KonsensusV1.Consensus.Blocks where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.DummyData as Dummy
import Concordium.Genesis.Data
import Concordium.Types
import qualified Concordium.Types.DummyData as Dummy
import Concordium.Types.HashableTo
import Concordium.Types.Transactions

import qualified Concordium.Genesis.Data.P6 as P6
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Blocks
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Startup
import Concordium.Types.BakerIdentity
import Concordium.Types.SeedState
import Control.Monad.State
import Data.Foldable
import qualified Data.Map.Strict as Map

genesisData :: GenesisData 'P6
bakers :: [(BakerIdentity, FullBakerInfo)]
genesisTotalAmount :: Amount
(genesisData, bakers, genesisTotalAmount) =
    makeGenesisDataV1
        0
        5
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

genesisHash :: BlockHash
genesisHash = genesisBlockHash genesisData

genesisLEN :: LeadershipElectionNonce
genesisLEN = genesisLeadershipElectionNonce $ P6.genesisInitialState $ unGDP6 genesisData

bakerKey :: Integral a => a -> BakerSignPrivateKey
bakerKey i = bakerSignKey $ fst (bakers !! fromIntegral i)

bakerVRFKey :: Integral a => a -> BakerElectionPrivateKey
bakerVRFKey i = bakerElectionKey $ fst (bakers !! fromIntegral i)

bakerAggKey :: Integral a => a -> BakerAggregationPrivateKey
bakerAggKey i = bakerAggregationKey $ fst (bakers !! fromIntegral i)

theFinalizers :: [Int]
theFinalizers = [0 .. 4]

-- |Finalizer set of all finalizers.
allFinalizers :: FinalizerSet
allFinalizers = finalizerSet $ FinalizerIndex <$> [0 .. 4]

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
-- This is baked by baker 2.
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
          bbStateHash = read "0eb9a986aaef3669b4435bde935b6a11d529f1e919b48907f8cefc06ff705848"
        }
  where
    bakerId = 2

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
          bbStateHash = read "6c08c8fff794df3f844df8c3c7ca7b1192cf584eb6c5830e14db0f48c7d17a90"
        }
  where
    bakerId = 4

-- |A valid block for round 2 where round 1 timed out.
testBB2' :: BakedBlock
testBB2' =
    BakedBlock
        { bbRound = 2,
          bbEpoch = 0,
          bbTimestamp = 2_000,
          bbBaker = bakerId,
          bbQuorumCertificate = genQC,
          bbTimeoutCertificate = Present (validTimeoutFor genQC 1),
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 2 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "058e4a4aebd80dd02ab7f405247db4dc0ff6f7ad5a31eae726fcf60f9f885b19"
        }
  where
    bakerId = 4
    genQC = genesisQuorumCertificate genesisHash

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
          bbStateHash = read "11d14dba27b18f262a9b10c6939a5a4ab3aeffa56e5e3b16797db5e28a3f25df"
        }
  where
    bakerId = 3

testBB3' :: BakedBlock
testBB3' =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 0,
          bbTimestamp = 3_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB2',
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 3 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "d36fe1a018a55a7e9837a1de363e882247c7503f9077063cbd35ffad09f82246"
        }
  where
    bakerId = 3

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
          bbStateHash = read "834aa43e0b7173e98f0c7fffaf48ae5554477917e86dcdbd79841c2a286a333a"
        }
  where
    bakerId = 2

-- |Valid block for round 1. This is baked by baker 2.
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
          bbStateHash = read "983ac54373f21d99a9a42555f7d8f40e9bb24f92b3f71da62c72473ab6d7f24f"
        }
  where
    bakerId = 2

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
          bbStateHash = read "983ac54373f21d99a9a42555f7d8f40e9bb24f92b3f71da62c72473ab6d7f24f"
        }
  where
    bakerId = 4

testEpochFinEntry :: FinalizationEntry
testEpochFinEntry =
    FinalizationEntry
        { feFinalizedQuorumCertificate = validQCFor testBB1E,
          feSuccessorQuorumCertificate = validQCFor testBB2E,
          feSuccessorProof = getHash testBB2E
        }

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
          bbNonce = computeBlockNonce genesisLEN 3 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
          bbStateHash = read "983ac54373f21d99a9a42555f7d8f40e9bb24f92b3f71da62c72473ab6d7f24f"
        }
  where
    bakerId = 3

succeedReceiveBlock :: PendingBlock -> TestMonad 'P6 ()
succeedReceiveBlock pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultSuccess vb -> do
            executeBlock vb
            status <- getBlockStatus (getHash pb) =<< get
            case status of
                BlockAlive _ -> return ()
                _ -> liftIO . assertFailure $ "Expected BlockAlive after executeBlock, but found: " ++ show status ++ "\n" ++ show pb
        _ -> liftIO . assertFailure $ "Expected BlockResultSuccess after uponReceivingBlock, but found: " ++ show res ++ "\n" ++ show pb

pendingReceiveBlock :: PendingBlock -> TestMonad 'P6 ()
pendingReceiveBlock pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultPending -> return ()
        _ -> liftIO . assertFailure $ "Expected BlockResultPending after uponReceivingBlock, but found: " ++ show res

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

signedPB :: BakedBlock -> PendingBlock
signedPB bb =
    PendingBlock
        { pbReceiveTime = timestampToUTCTime $ bbTimestamp bb,
          pbBlock = validSignBlock bb
        }

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
  where
    noBaker = BakerContext Nothing
    testTime = timestampToUTCTime 5_000

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
  where
    noBaker = BakerContext Nothing
    testTime = timestampToUTCTime 5_000

-- |Receive 3 blocks where the first round is skipped due to timeout.
testReceiveWithTimeout :: Assertion
testReceiveWithTimeout = runTestMonad noBaker testTime genesisData $ do
    let b2' = signedPB testBB2'
    succeedReceiveBlock b2'
    succeedReceiveBlock $ signedPB testBB3'
    succeedReceiveBlock $ signedPB testBB4'
    checkFinalized b2'
  where
    noBaker = BakerContext Nothing
    testTime = timestampToUTCTime 5_000

testReceiveStale :: Assertion
testReceiveStale = runTestMonad noBaker testTime genesisData $ do
    mapM_ (succeedReceiveBlock . signedPB) [testBB2', testBB3', testBB4']
    res <- uponReceivingBlock (signedPB testBB1)
    liftIO $ res `shouldBe` BlockResultStale
  where
    noBaker = BakerContext Nothing
    testTime = timestampToUTCTime 5_000

testReceiveEpoch :: Assertion
testReceiveEpoch = runTestMonad noBaker testTime genesisData $ do
    mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB3E]
  where
    noBaker = BakerContext Nothing
    testTime = timestampToUTCTime 5_000

tests :: Spec
tests = describe "KonsensusV1.Consensus.Blocks" $ do
    describe "uponReceiveingBlock" $ do
        it "receive 3 consecutive blocks" testReceive3
        it "receive 3 blocks reordered" testReceive3Reordered
        it "skip round 1, receive rounds 2,3,4" testReceiveWithTimeout
        it "receive stale round" testReceiveStale
        it "epoch transition" testReceiveEpoch
