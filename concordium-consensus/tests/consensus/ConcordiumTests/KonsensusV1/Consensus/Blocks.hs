{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests block verification, processing, advancing round and baking.
--  The below tests are intended to test the functionality exposed by the 'Concordium.KonsensusV1.Consensus.Blocks' module.
--
--  In particular block processing and hence round/epoch progression are being tested.
module ConcordiumTests.KonsensusV1.Consensus.Blocks where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Writer.Class
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.DummyData as Dummy
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Crypto.SignatureScheme as Sig
import Concordium.Genesis.Data
import Concordium.Types
import Concordium.Types.BakerIdentity
import qualified Concordium.Types.DummyData as Dummy
import Concordium.Types.HashableTo
import Concordium.Types.Parameters
import Concordium.Types.SeedState
import qualified Concordium.Types.TransactionOutcomes as TransactionOutcomes
import Concordium.Types.Transactions
import qualified Concordium.Types.Transactions as Transactions

import qualified Concordium.Genesis.Data.P10 as P10
import qualified Concordium.Genesis.Data.P6 as P6
import qualified Concordium.Genesis.Data.P7 as P7
import qualified Concordium.Genesis.Data.P8 as P8
import qualified Concordium.Genesis.Data.P9 as P9
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.LFMBTree (hashAsLFMBTV0)
import Concordium.GlobalState.BlockState (TransactionSummaryV1)
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Blocks
import Concordium.KonsensusV1.Consensus.Timeout hiding (Received)
import Concordium.KonsensusV1.LeaderElection
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Startup
import Concordium.TimerMonad
import Concordium.Types.Option

import Concordium.GlobalState.TransactionTable (LiveTransactionStatus (..))
import Concordium.Types.Execution (TransactionIndex)
import qualified ConcordiumTests.KonsensusV1.Common as Common

type PV = 'P6

maxBaker :: (Integral a) => a
maxBaker = 5

genTime :: Timestamp
genTime = 0

genEpochDuration :: Duration
genEpochDuration = 3_600_000

genesisDataV1 ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    (GenesisData pv, [(BakerIdentity, FullBakerInfo)], Amount)
genesisDataV1 sProtocolVersion =
    makeGenesisDataV1 @pv
        genTime
        (maxBaker + 1)
        genEpochDuration
        Dummy.dummyCryptographicParameters
        Dummy.dummyIdentityProviders
        Dummy.dummyArs
        [ foundationAcct
        ]
        (withIsAuthorizationsVersionFor sProtocolVersion Dummy.dummyKeyCollection)
        Dummy.dummyChainParameters
  where
    foundationAcct =
        Dummy.createCustomAccount
            1_000_000_000_000
            foundationKeyPair
            foundationAccountAddress

genesisData ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    GenesisData pv
genesisData sProtocolVersion = let (genData, _, _) = genesisDataV1 sProtocolVersion in genData

bakers ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    [(BakerIdentity, FullBakerInfo)]
bakers sProtocolVersion = let (_, theBakers, _) = genesisDataV1 sProtocolVersion in theBakers

-- | Key pair for the foundation account
foundationKeyPair :: Sig.KeyPair
foundationKeyPair = Dummy.deterministicKP 0

-- | Account address for the foundation account
foundationAccountAddress :: AccountAddress
foundationAccountAddress = Dummy.accountAddressFrom 0

-- | Hash of the genesis block.
genesisHash :: (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> BlockHash
genesisHash sProtocolVersion = genesisBlockHash (genesisData sProtocolVersion)

-- | Leadership election nonce at genesis
genesisLEN :: (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> LeadershipElectionNonce
genesisLEN sProtocolVersion = case sProtocolVersion of
    SP6 -> genesisLeadershipElectionNonce $ P6.genesisInitialState $ unGDP6 $ genesisData sProtocolVersion
    SP7 -> genesisLeadershipElectionNonce $ P7.genesisInitialState $ unGDP7 $ genesisData sProtocolVersion
    SP8 -> genesisLeadershipElectionNonce $ P8.genesisInitialState $ unGDP8 $ genesisData sProtocolVersion
    SP9 -> genesisLeadershipElectionNonce $ P9.genesisInitialState $ unGDP9 $ genesisData sProtocolVersion
    SP10 -> genesisLeadershipElectionNonce $ P10.genesisInitialState $ unGDP10 $ genesisData sProtocolVersion

-- | Full bakers at genesis
genesisFullBakers :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> FullBakers
genesisFullBakers sProtocolVersion = FullBakers{..}
  where
    fullBakerInfos = Vec.fromList $ snd <$> bakers sProtocolVersion
    bakerTotalStake = sum $ _bakerStake <$> fullBakerInfos

-- | Seed state at genesis
genesisSeedState :: (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> SeedState 'SeedStateVersion1
genesisSeedState sProtocolVersion = initialSeedStateV1 (genesisLEN sProtocolVersion) (addDuration genTime genEpochDuration)

bakerKey :: forall a pv. (IsConsensusV1 pv, IsProtocolVersion pv, Integral a) => SProtocolVersion pv -> a -> BakerSignPrivateKey
bakerKey sProtocolVersion i = bakerSignKey $ fst (bakers sProtocolVersion !! fromIntegral i)

bakerVRFKey :: forall a pv. (IsConsensusV1 pv, IsProtocolVersion pv, Integral a) => SProtocolVersion pv -> a -> BakerElectionPrivateKey
bakerVRFKey sProtocolVersion i = bakerElectionKey $ fst (bakers sProtocolVersion !! fromIntegral i)

bakerAggKey :: forall a pv. (IsConsensusV1 pv, IsProtocolVersion pv, Integral a) => SProtocolVersion pv -> a -> BakerAggregationPrivateKey
bakerAggKey sProtocolVersion i = bakerAggregationKey $ fst (bakers sProtocolVersion !! fromIntegral i)

theFinalizers :: [Int]
theFinalizers = [0 .. maxBaker]

-- | Finalizer set of all finalizers.
allFinalizers :: FinalizerSet
allFinalizers = finalizerSet $ FinalizerIndex <$> [0 .. maxBaker]

validQCFor :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv -> QuorumCertificate
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
            { qsmGenesis = genesisHash (protocolVersion @pv),
              qsmBlock = block,
              qsmRound = bbRound bb,
              qsmEpoch = bbEpoch bb
            }
    sig = fold [signQuorumSignatureMessage qsm (bakerAggKey (protocolVersion @pv) i) | i <- theFinalizers]

validSignBlock :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv -> SignedBlock pv
validSignBlock bb =
    let sProtocolVersion = protocolVersion @pv
    in  signBlock (bakerKey sProtocolVersion (bbBaker bb)) (genesisHash sProtocolVersion) bb

invalidSignBlock :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv -> SignedBlock pv
invalidSignBlock bb =
    let sProtocolVersion = protocolVersion @pv
    in  signBlock (bakerKey sProtocolVersion (bbBaker bb)) (getHash bb) bb

-- | Create a valid timeout message given a QC and a round.
--  All finalizers sign the certificate and they all have the QC as their highest QC.
validTimeoutFor ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    QuorumCertificate ->
    Round ->
    TimeoutCertificate
validTimeoutFor sProtocolVersion = validTimeoutForFinalizers sProtocolVersion theFinalizers

validTimeoutForFinalizers ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    [Int] ->
    QuorumCertificate ->
    Round ->
    TimeoutCertificate
validTimeoutForFinalizers sProtocolVersion finalizers qc rnd =
    TimeoutCertificate
        { tcRound = rnd,
          tcMinEpoch = qcEpoch qc,
          tcFinalizerQCRoundsFirstEpoch = FinalizerRounds (Map.singleton (qcRound qc) finSet),
          tcFinalizerQCRoundsSecondEpoch = FinalizerRounds Map.empty,
          tcAggregateSignature =
            fold
                [signTimeoutSignatureMessage tsm (bakerAggKey sProtocolVersion i) | i <- finalizers]
        }
  where
    finSet = finalizerSet $ FinalizerIndex . fromIntegral <$> finalizers
    tsm =
        TimeoutSignatureMessage
            { tsmGenesis = genesisHash sProtocolVersion,
              tsmRound = rnd,
              tsmQCRound = qcRound qc,
              tsmQCEpoch = qcEpoch qc
            }

-- | Produce properly-signed timeout message for the given QC, round and current epoch from each
--  baker. (This assumes that all of the bakers are finalizers for the purposes of computing
--  finalizer indexes.)
timeoutMessagesFor ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    QuorumCertificate ->
    Round ->
    Epoch ->
    [TimeoutMessage]
timeoutMessagesFor sProtocolVersion qc curRound curEpoch = mkTm <$> bakers sProtocolVersion
  where
    mkTm (BakerIdentity{..}, _) =
        signTimeoutMessage (tmb bakerId bakerAggregationKey) (genesisHash sProtocolVersion) bakerSignKey
    tmb bid aggKey =
        TimeoutMessageBody
            { tmFinalizerIndex = FinalizerIndex (fromIntegral bid),
              tmRound = curRound,
              tmEpoch = curEpoch,
              tmQuorumCertificate = qc,
              tmAggregateSignature = signTimeoutSignatureMessage tsm aggKey
            }
    tsm =
        TimeoutSignatureMessage
            { tsmGenesis = genesisHash sProtocolVersion,
              tsmRound = curRound,
              tsmQCRound = qcRound qc,
              tsmQCEpoch = qcEpoch qc
            }

-- | Produce a properly-signed timeout message for the given QC, round and current epoch from the
--  first baker. (This assumes that the first baker is a finalizer for the purposes of computing
--  finalizer indexes.)
firstTimeoutMessageFor ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    QuorumCertificate ->
    Round ->
    Epoch ->
    TimeoutMessage
firstTimeoutMessageFor sProtocolVersion qc curRound curEpoch =
    case timeoutMessagesFor sProtocolVersion qc curRound curEpoch of
        (tm : _) -> tm
        [] -> error "Impossible: there must be at least one baker"

-- | Helper to compute the transaction outcomes hash for a given set of transaction outcomes and
--  special transaction outcomes.
transactionOutcomesHashV1 ::
    [TransactionSummaryV1 tov] ->
    [Transactions.SpecialTransactionOutcome] ->
    TransactionOutcomes.TransactionOutcomesHash
transactionOutcomesHashV1 outcomes specialOutcomes =
    TransactionOutcomes.TransactionOutcomesHash $
        H.hashShort $
            "TransactionOutcomesHashV1"
                <> H.hashToShortByteString out
                <> H.hashToShortByteString special
  where
    lfmbHash :: (HashableTo H.Hash a) => [a] -> H.Hash
    lfmbHash = hashAsLFMBTV0 (H.hash "EmptyLFMBTree") . fmap getHash
    out = lfmbHash outcomes
    special = lfmbHash specialOutcomes

-- | Compute the transaction outcomes hash for a block with no transactions.
emptyBlockTOHV1 :: BakerId -> TransactionOutcomes.TransactionOutcomesHash
emptyBlockTOHV1 bid = transactionOutcomesHashV1 [] [BlockAccrueReward 0 0 0 0 0 0 bid]

setStateHash :: StateHash -> BakedBlock PV -> BakedBlock PV
setStateHash newStateHash block = case bbDerivableHashes block of
    hashes@DerivableBlockHashesV0{} ->
        block{bbDerivableHashes = hashes{dbhv0BlockStateHash = newStateHash}}

-- | Valid block for round 1.
testBB1 :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB1 =
    BakedBlock
        { bbRound = 1,
          bbEpoch = 0,
          bbTimestamp = 1_000,
          bbBaker = bakerId,
          bbQuorumCertificate = genesisQuorumCertificate (genesisHash sProtocolVersion),
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 1 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "dee89435dba1609a84fa62283d2f63ec50f85b9c22f8815daf348df5428ccb65"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "0970b0f7459e5150a56ac283eee6f587fc49cb1c3408146b46ee05457235bec7"
                    }
        }
  where
    bakerId = 2
    sProtocolVersion = protocolVersion @pv

-- | Valid block for round 2, descended from 'testBB1'.
testBB2 :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB2 =
    BakedBlock
        { bbRound = 2,
          bbEpoch = 0,
          bbTimestamp = 2_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB1,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 2 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "d36974d10f1331559e396be5f8e31ecedc2042ebf941bc2fad6050e9e082f206"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "2e6636b8275663e44452650e4b7968ecb26a32d57998fbbccc0292fdecb1522d"
                    }
        }
  where
    bakerId = 4
    sProtocolVersion = protocolVersion @pv

-- | Valid block for round 3, descended from 'testBB2'.
testBB3 :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3 =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 0,
          bbTimestamp = 3_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB2,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 3 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "50998f735737ce13b35715a173efb7a3ad20cba597ba540985cd562a0b7bed74"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "5777ce2df452ce52ee6beb43c555051588cdad67ad742e7be438cf9d22e31950"
                    }
        }
  where
    bakerId = 4
    sProtocolVersion = protocolVersion @pv

-- | A valid block for round 2 where round 1 timed out.
testBB2' :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB2' =
    (testBB2 @pv)
        { bbQuorumCertificate = genQC,
          bbTimeoutCertificate = Present (validTimeoutFor sProtocolVersion genQC 1),
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "20cd8fe8689b17850e73e8322b53398a49df5e4723eaa77acaf5474e94915c0b"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "04185ac844f6aea6e32b667debd1e9a337d67a80350d12f1cb813bf212a4bc23"
                    }
        }
  where
    bakerId :: BakerId
    bakerId = 4
    sProtocolVersion = protocolVersion @pv
    genQC = genesisQuorumCertificate (genesisHash sProtocolVersion)

-- | A valid block for round 3 descended from 'testBB2''.
testBB3' :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3' =
    (testBB3 @pv)
        { bbQuorumCertificate = validQCFor @pv testBB2',
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "784471f09f9678a2cf8208af45186f553406430b67e035ebf1b772e7c39fbd97"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "11e59c1a721359a64b86a0c6bcee8f6cac7c5bc3a6b98517b0b4f8c5a726f9c5"
                    }
        }
  where
    bakerId :: BakerId
    bakerId = 4
    sProtocolVersion = protocolVersion @pv

-- | A valid block for round 4 descended from 'testBB3''.
testBB4' :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB4' =
    BakedBlock
        { bbRound = 4,
          bbEpoch = 0,
          bbTimestamp = 4_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB3',
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 4 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "3bb5b307d7abc6fad2464455f604d63512fff93d7fdeb2aa08d5a8f2720340fe"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "ad8a288f88806037899d782b7dd3a37ade59e0d5f3e7a90b1db2b722ae9cbe3d"
                    }
        }
  where
    bakerId = 3
    sProtocolVersion = protocolVersion @pv

-- | A valid block for round 3 descended from the genesis block with a timeout for round 2.
testBB3'' :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3'' =
    (testBB3 @pv)
        { bbQuorumCertificate = genQC,
          bbTimeoutCertificate = Present (validTimeoutFor sProtocolVersion genQC 2),
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "2b81e5943112b9a9916e57980a4b17b5b3b329eba0402d14201bfe1c9551a16d"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "daa799010a8b4acb47fa97b876abed73621db292029360734d9c8978b5859e7b"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId :: BakerId
    bakerId = 4
    genQC = genesisQuorumCertificate (genesisHash sProtocolVersion)

-- | Valid block for round 1.
--  This should be past the epoch transition trigger time.
testBB1E :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB1E =
    BakedBlock
        { bbRound = 1,
          bbEpoch = 0,
          bbTimestamp = 3_600_000,
          bbBaker = bakerId,
          bbQuorumCertificate = genesisQuorumCertificate (genesisHash sProtocolVersion),
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 1 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "3ce2fe0d538434fa7677549a4acbdecea606bd47a61fa39735de1dc144c95eab"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "1811353ec3811af6241f3e5dc2e19740acf518f02dccc51f427310b8cfe9ca6c"
                    }
        }
  where
    bakerId = 2
    sProtocolVersion = protocolVersion @pv

-- | Valid block for round 2. Descends from 'testBB1E'.
testBB2E :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB2E =
    BakedBlock
        { bbRound = 2,
          bbEpoch = 0,
          bbTimestamp = 3_601_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB1E,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 2 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "df5d25b8ffbad7be62be0ae2ce1a4730018062c3bda6d6caa02ea03545a263fd"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "99fc52af1ee2336f0d353a84c4d6c15345882271f648f7b84e69d9c40d5571c2"
                    }
        }
  where
    bakerId = 4
    sProtocolVersion = protocolVersion @pv

-- | A block that is valid for round 3, descending from 'testBB2E', but which should not be validated
--  by a finalizer because it is in epoch 0. With the QC for 'testBB2E', a finalizer should move into
--  epoch 1, and thus refuse to validate this block.
testBB3EX :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3EX =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 0,
          bbTimestamp = 3_602_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB2E,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 3 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "81e1b33e20088562fcb48c619ea16e800d7fba58995fa6487a6209cf448c7d08"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "0236b72bafe1575fdde0b01b35d24ad16613569600b83ed46b7e17c3c3dbaf28"
                    }
        }
  where
    bakerId = 4
    sProtocolVersion = protocolVersion @pv

-- | Epoch finalization entry based on QCs for 'testBB1E' and 'testBB2E'.
testEpochFinEntry :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => SProtocolVersion pv -> FinalizationEntry pv
testEpochFinEntry _ =
    FinalizationEntry
        { feFinalizedQuorumCertificate = validQCFor @pv testBB1E,
          feSuccessorQuorumCertificate = validQCFor @pv testBB2E,
          feSuccessorProof = getHash (testBB2E @pv)
        }

-- | Epoch leadership election nonce for epoch 1, assuming that block 'testBB1E' is finalized.
testEpochLEN :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> LeadershipElectionNonce
testEpochLEN sProtocolVersion =
    nonceForNewEpoch (genesisFullBakers sProtocolVersion) $
        upd (testBB1E @pv) (genesisSeedState sProtocolVersion)
  where
    upd b = updateSeedStateForBlock (bbTimestamp b) (bbNonce b) False

-- | Valid block for round 3, epoch 1. Descends from 'testBB2E'. The finalization entry is
--  'testEpochFinEntry'.
testBB3E :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3E =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 1,
          bbTimestamp = 3_602_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB2E,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Present (testEpochFinEntry sProtocolVersion),
          bbNonce = computeBlockNonce (testEpochLEN sProtocolVersion) 3 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "dc31a663a0bd166507e21cc641759018651c716b3571531672956abf24b0f4bc"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "2a12a1b02d8cfb835d6f572ffd3a0156145ec2142b47ef7b9e9495b61ff241b7"
                    }
        }
  where
    bakerId = 5
    sProtocolVersion = protocolVersion @pv

-- | Invalid block for round 3, epoch 1. Descends from 'testBB1E', with finalization entry
--  'testEpochFinEntry'. The block contains a valid timeout certificate for round 2.
--  The block is not valid, because the highest round in the finalization entry is lower than the
--  round of the parent block.
testBB3E' :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3E' =
    (testBB3E @pv)
        { bbQuorumCertificate = validQCFor @pv testBB1E,
          bbTimeoutCertificate = Present (validTimeoutFor (protocolVersion @pv) (validQCFor @pv testBB1E) 2)
        }

-- | Valid block for round 3, epoch 1. Descends from 'testBB3E'.
testBB4E :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB4E =
    BakedBlock
        { bbRound = 4,
          bbEpoch = 1,
          bbTimestamp = 3_603_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB3E,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (testEpochLEN sProtocolVersion) 4 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "daa799010a8b4acb47fa97b876abed73621db292029360734d9c8978b5859e7b"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "66cf8d97a956e2306e60337848775d606f575bd48f4d1e4420d4cf579d5bfb0e"
                    }
        }
  where
    bakerId = 1
    sProtocolVersion = protocolVersion @pv

-- | Valid block for round 4 epoch 1. Descends from 'testBB2E', with finalization entry
--  'testEpochFinEntry'. The block contains a valid timeout for round 3.
testBB4E' :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB4E' =
    (testBB4E @pv)
        { bbQuorumCertificate = validQCFor @pv testBB2E,
          bbTimeoutCertificate = Present (validTimeoutFor sProtocolVersion (validQCFor @pv testBB1E) 3),
          bbEpochFinalizationEntry = Present (testEpochFinEntry sProtocolVersion),
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "41b44dd4db52dae4021a0d71fbec00a423ffc9892cf97bf6e506d722cdaaeb0d"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "e0d460456228a923c4d0116b6add192b491279b24ce160067652e1afa11bac56"
                    }
        }
  where
    bakerId :: BakerId
    bakerId = 1
    sProtocolVersion = protocolVersion @pv

-- | Valid block for round 5, epoch 1. Descends from 'testBB3E'. The timeout certificate for round
--  4 spans epoch 0 and 1.
testBB5E' :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB5E' =
    BakedBlock
        { bbRound = rnd,
          bbEpoch = 1,
          bbTimestamp = 3_604_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB3E,
          bbTimeoutCertificate = Present tc,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (testEpochLEN sProtocolVersion) rnd (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "ff8cd1198e3926f743e91a97484d75f1109534aaf9655e1c8c9507d4d0ebd8b3"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "2c8ff8fbc07b5e1486ebad3e241fa0aefdb7637651c6120b0a50a27057f7431a"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 2
    rnd = 5
    qc1 = validQCFor @pv testBB2E
    qc2 = validQCFor @pv testBB3E
    tc =
        TimeoutCertificate
            { tcRound = rnd - 1,
              tcMinEpoch = 0,
              tcFinalizerQCRoundsFirstEpoch = FinalizerRounds (Map.singleton (qcRound qc1) (finalizerSet $ FinalizerIndex <$> finsA)),
              tcFinalizerQCRoundsSecondEpoch = FinalizerRounds (Map.singleton (qcRound qc2) (finalizerSet $ FinalizerIndex <$> finsB)),
              tcAggregateSignature =
                fold $
                    [signTimeoutSignatureMessage (tsm qc1) (bakerAggKey sProtocolVersion i) | i <- finsA]
                        ++ [signTimeoutSignatureMessage (tsm qc2) (bakerAggKey sProtocolVersion i) | i <- finsB]
            }
    tsm qc =
        TimeoutSignatureMessage
            { tsmGenesis = genesisHash sProtocolVersion,
              tsmRound = rnd - 1,
              tsmQCRound = qcRound qc,
              tsmQCEpoch = qcEpoch qc
            }
    finsA = take 3 [0 .. maxBaker]
    finsB = drop 3 [0 .. maxBaker]

testBB2Ex :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB2Ex =
    (testBB2E @pv)
        { bbQuorumCertificate = genQC,
          bbTimeoutCertificate = Present (validTimeoutFor sProtocolVersion genQC 1),
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "df76421871484a877532dc9b748fcf248bd186898def8bd40fee0a3cf9636b92"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "a562963223c07cbcee46e78bba06968d578120b71eaf59d8ce12f3f384b21f47"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId :: BakerId
    bakerId = 4
    genQC = genesisQuorumCertificate (genesisHash sProtocolVersion)

-- | Epoch leadership election nonce for epoch 1, assuming that block 'testBB2Ex' is finalized.
testEpochLENx ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    LeadershipElectionNonce
testEpochLENx sProtocolVersion =
    nonceForNewEpoch (genesisFullBakers sProtocolVersion) $
        upd (testBB2Ex @pv) (genesisSeedState sProtocolVersion)
  where
    upd b = updateSeedStateForBlock (bbTimestamp b) (bbNonce b) False

testBB3Ex :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3Ex =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 1,
          bbTimestamp = 3_602_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB2Ex,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Present (testEpochFinEntry sProtocolVersion),
          bbNonce =
            computeBlockNonce
                (testEpochLENx sProtocolVersion)
                3
                (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "dc31a663a0bd166507e21cc641759018651c716b3571531672956abf24b0f4bc"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "2ea4f556dc29b5a1774635cb670f7b9aa3182eb2cc685b0e4f59daf9541cb539"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 2

-- | Valid block in round 3 descended from 'testBB1E' with a timeout.
testBB3EA :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3EA =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 0,
          bbTimestamp = 3_603_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB1E,
          bbTimeoutCertificate =
            Present $
                validTimeoutFor
                    sProtocolVersion
                    (validQCFor @pv testBB1E)
                    2,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 3 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "df5d25b8ffbad7be62be0ae2ce1a4730018062c3bda6d6caa02ea03545a263fd"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "99fc52af1ee2336f0d353a84c4d6c15345882271f648f7b84e69d9c40d5571c2"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 4

-- | Valid block in round 4, epoch 1, descended from 'testBB3EA', with a finalization proof based on
--  'testBB2E'.
testBB4EA :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB4EA =
    BakedBlock
        { bbRound = 4,
          bbEpoch = 1,
          bbTimestamp = 3_604_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB3EA,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Present (testEpochFinEntry sProtocolVersion),
          bbNonce =
            computeBlockNonce
                (testEpochLEN sProtocolVersion)
                4
                (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 bakerId,
                      dbhv0BlockStateHash = read "41b44dd4db52dae4021a0d71fbec00a423ffc9892cf97bf6e506d722cdaaeb0d"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "e0d460456228a923c4d0116b6add192b491279b24ce160067652e1afa11bac56"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 1

testTrans1 :: BlockItem
testTrans1 = Dummy.makeTransferTransaction (foundationKeyPair, foundationAccountAddress) foundationAccountAddress 100 1

testTrans2 :: BlockItem
testTrans2 = Dummy.makeTransferTransaction (foundationKeyPair, foundationAccountAddress) foundationAccountAddress 200 2

testTrans3 :: BlockItem
testTrans3 = Dummy.makeTransferTransaction (foundationKeyPair, foundationAccountAddress) foundationAccountAddress 300 3

testTrans3' :: BlockItem
testTrans3' = Dummy.makeTransferTransaction (foundationKeyPair, foundationAccountAddress) foundationAccountAddress 350 3

testTrans4 :: BlockItem
testTrans4 = Dummy.makeTransferTransaction (foundationKeyPair, foundationAccountAddress) foundationAccountAddress 400 4

testTrans5 :: BlockItem
testTrans5 = Dummy.makeTransferTransaction (foundationKeyPair, foundationAccountAddress) foundationAccountAddress 500 5

-- | Valid block for round 1 with transactions.
testBB1T :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB1T =
    BakedBlock
        { bbRound = 1,
          bbEpoch = 0,
          bbTimestamp = 1_000,
          bbBaker = bakerId,
          bbQuorumCertificate = genesisQuorumCertificate (genesisHash sProtocolVersion),
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 1 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions =
            Vec.fromList [testTrans1, testTrans2],
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "0e16397614dae92cc51f47b1e32fdfbd4ae37776dfe36ae33e59d958929796b3",
                      dbhv0BlockStateHash = read "161d4a2e780cfa5d4bea2d5e2ee11a7b89ef3f77a79c6ab3172c972f84e366c4"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "e9f625190459013f6530d044d80d91182b91f440cbb0a8933d5ba9e5dff06236"
                    }
        }
  where
    bakerId = 2
    sProtocolVersion = protocolVersion @pv

-- | Valid block for round 2, descended from 'testBB1T', with transactions.
testBB2T :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB2T =
    BakedBlock
        { bbRound = 2,
          bbEpoch = 0,
          bbTimestamp = 2_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB1T,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 2 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.fromList [testTrans3, testTrans4, testTrans5],
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "da9360655107d529218ed9586783200ceca9c965396e340d55cc3e3ec5787255",
                      dbhv0BlockStateHash = read "552c1a805700e33f7ca62906a71f9c99a072cb10cc167571ba4c16143e79e936"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "a8a6f224567aaac2ef40a7ddc2b186c47485acd3bb685d6216af25fdddd9b580"
                    }
        }
  where
    bakerId = 4
    sProtocolVersion = protocolVersion @pv

-- | Valid block for round 3, descended from 'testBB1T'.
testBB3T :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3T =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 0,
          bbTimestamp = 3_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB1T,
          bbTimeoutCertificate =
            Present $
                validTimeoutFor
                    sProtocolVersion
                    (validQCFor @pv testBB1T)
                    2,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 3 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.fromList [testTrans3', testTrans4],
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash =
                        read "25f8c365829b3acbde7c9cb9400b5c1b687aee3927e635cd6d8e8744d5ebb18f",
                      dbhv0BlockStateHash = read "50ad522662f6db9b87ec2c73ddafe0165a262653f9a16cdd91b1444efc679c9f"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "a502a11146284e4667d509da46a8c0c437533a1325d76d8e0b2422c00be90bca"
                    }
        }
  where
    bakerId = 4
    sProtocolVersion = protocolVersion @pv

-- | Valid block for round 4, descended from 'testBB3T'.
testBB4T :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB4T =
    BakedBlock
        { bbRound = 4,
          bbEpoch = 0,
          bbTimestamp = 4_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB3T,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 4 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash =
                        read "b97b08a99493fe5d26f7c3b1dcc447c26993b513cbc7c6ac13e8ae4e03bd2f05",
                      dbhv0BlockStateHash = read "301388fbc931ac1930d201ecb3f3f9ffe3d9e7c3e97220abc6df97bee4b30e36"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "9fd40b91df19dd8391585b8497c493f950e1b2615680a8693bfded6172bd896d"
                    }
        }
  where
    bakerId = 3
    sProtocolVersion = protocolVersion @pv

-- | Valid block for round 5, descended from 'testBB4T'.
testBB5T :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB5T =
    BakedBlock
        { bbRound = 5,
          bbEpoch = 0,
          bbTimestamp = 5_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB4T,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 5 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash =
                        read "c2f7bd7729b51551f85f2227fa3fc0e19b743eee227c7156fc6c49bd11f36f40",
                      dbhv0BlockStateHash = read "8f77d6ccd863748e07c38cb387f80b475788f019be4078b63e06da5626297383"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "d9ebdef2097ed38d5eececb6cf30c611c2d2d3a54bf7231aec40fc0c674156b4"
                    }
        }
  where
    bakerId = 3
    sProtocolVersion = protocolVersion @pv

succeedReceiveBlock :: (IsProtocolVersion pv, IsConsensusV1 pv) => PendingBlock pv -> TestMonad pv ()
succeedReceiveBlock pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultSuccess vb -> do
            ((), events) <- listen $ executeBlock vb
            status <- getBlockStatus (getHash pb) =<< get
            case status of
                BlockAlive _ -> return ()
                BlockFinalized _ -> return ()
                _ -> liftIO . assertFailure $ "Expected BlockAlive after executeBlock, but found: " ++ show status ++ "\n" ++ show pb
            case events of
                (OnBlock (NormalBlock b) : _)
                    | b == pbBlock pb -> return ()
                (OnFinalize _ : OnBlock (NormalBlock b) : _)
                    | b == pbBlock pb -> return ()
                _ -> liftIO . assertFailure $ "Expected OnBlock event on executeBlock, but saw: " ++ show events
        _ -> liftIO . assertFailure $ "Expected BlockResultSuccess after uponReceivingBlock, but found: " ++ show res ++ "\n" ++ show pb

duplicateReceiveBlock :: (IsProtocolVersion pv, IsConsensusV1 pv) => PendingBlock pv -> TestMonad pv ()
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

succeedReceiveBlockFailExecute ::
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    PendingBlock pv ->
    TestMonad pv ()
succeedReceiveBlockFailExecute pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultSuccess vb -> do
            executeBlock vb
            status <- getBlockStatus (getHash pb) =<< get
            case status of
                BlockUnknown -> return ()
                BlockDead -> return ()
                _ ->
                    liftIO . assertFailure $
                        "Expected BlockUnknown or BlockDead after executeBlock, but found: "
                            ++ show status
                            ++ "\n"
                            ++ show pb
        _ ->
            liftIO . assertFailure $
                "Expected BlockResultSuccess after uponReceivingBlock, but found: "
                    ++ show res
                    ++ "\n"
                    ++ show pb

duplicateReceiveBlockFailExecute ::
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    PendingBlock pv ->
    TestMonad pv ()
duplicateReceiveBlockFailExecute pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultDoubleSign vb -> do
            executeBlock vb
            status <- getBlockStatus (getHash pb) =<< get
            case status of
                BlockUnknown -> return ()
                BlockDead -> return ()
                _ ->
                    liftIO . assertFailure $
                        "Expected BlockUnknown or BlockDead after executeBlock, but found: "
                            ++ show status
                            ++ "\n"
                            ++ show pb
        _ ->
            liftIO . assertFailure $
                "Expected BlockResultDoubleSign after uponReceivingBlock, but found: "
                    ++ show res
                    ++ "\n"
                    ++ show pb

pendingReceiveBlock :: (IsProtocolVersion pv, IsConsensusV1 pv) => PendingBlock pv -> TestMonad pv ()
pendingReceiveBlock pb = do
    res <- uponReceivingBlock pb
    case res of
        BlockResultPending -> return ()
        _ ->
            liftIO . assertFailure $
                "Expected BlockResultPending after uponReceivingBlock, but found: " ++ show res

staleReceiveBlock :: (IsProtocolVersion pv, IsConsensusV1 pv) => PendingBlock pv -> TestMonad pv ()
staleReceiveBlock sb = do
    res <- uponReceivingBlock sb
    liftIO $ res `shouldBe` BlockResultStale

invalidReceiveBlock :: (IsProtocolVersion pv, IsConsensusV1 pv) => PendingBlock pv -> TestMonad pv ()
invalidReceiveBlock sb = do
    res <- uponReceivingBlock sb
    liftIO $ res `shouldBe` BlockResultInvalid

earlyReceiveBlock :: (IsProtocolVersion pv, IsConsensusV1 pv) => PendingBlock pv -> TestMonad pv ()
earlyReceiveBlock sb = do
    res <- uponReceivingBlock sb
    liftIO $ res `shouldBe` BlockResultEarly

checkLive :: (IsProtocolVersion pv, IsConsensusV1 pv, HashableTo BlockHash b) => b -> TestMonad pv ()
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

checkFinalized :: (HashableTo BlockHash b, IsProtocolVersion pv) => b -> TestMonad pv ()
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

checkDead :: (IsProtocolVersion pv, HashableTo BlockHash b) => b -> TestMonad pv ()
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

signedPB :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv -> PendingBlock pv
signedPB bb =
    PendingBlock
        { pbReceiveTime = timestampToUTCTime $ bbTimestamp bb,
          pbBlock = validSignBlock bb
        }

-- | Baker context with no baker.
noBaker :: BakerContext
noBaker = BakerContext Nothing

-- | Baker context with baker @i@.
baker :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> Int -> BakerContext
baker sProtocolVersion i = BakerContext $ Just $ fst $ bakers sProtocolVersion !! i

-- | Current time used for running (some) tests. 5 seconds after genesis.
testTime :: UTCTime
testTime = timestampToUTCTime 5_000

-- | Receive 3 valid blocks in consecutive rounds.
testReceive3 :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testReceive3 sProtocolVersion =
    it "receive 3 consecutive blocks" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            let b1 = signedPB testBB1
            succeedReceiveBlock b1
            let b2 = signedPB testBB2
            succeedReceiveBlock b2
            let b3 = signedPB testBB3
            succeedReceiveBlock b3
            -- b3's QC is for b2, which has QC for b1, so b1 should now be finalized.
            checkFinalized b1

-- | Receive 3 valid blocks in consecutive rounds, but with the block for round 2 being received first.
testReceive3Reordered :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testReceive3Reordered sProtocolVersion =
    it "receive 3 blocks reordered" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            let b2 = signedPB testBB2
            pendingReceiveBlock b2
            let b1 = signedPB testBB1
            succeedReceiveBlock b1
            let b3 = signedPB testBB3
            pendingReceiveBlock b3
            succeedReceiveBlock b2
            succeedReceiveBlock b3
            -- b3's QC is for b2, which has QC for b1, so b1 should now be finalized.
            checkFinalized b1

-- | Receive 4 valid blocks reordered across epochs.
testReceive4Reordered :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testReceive4Reordered sProtocolVersion =
    it "receive 4 blocks reordered, multiple epochs" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            -- We get block 4, but we don't know the parent (BB3E)
            pendingReceiveBlock (signedPB testBB4E)
            -- so we "catch up" and get BB1E
            succeedReceiveBlock (signedPB testBB1E)
            -- Now we see block 3, but we don't know the parent (BB2E)
            pendingReceiveBlock (signedPB testBB3E)
            -- so we "catch up" again and get BB2E
            succeedReceiveBlock (signedPB testBB2E)
            -- We get the rest via catch-up
            succeedReceiveBlock (signedPB testBB3E)
            succeedReceiveBlock (signedPB testBB4E)
            checkFinalized (testBB1E @pv)
            -- Note: testBB2E is not finalized because it is in a different epoch from testBB3E.
            checkLive (testBB2E @pv)
            checkLive (testBB3E @pv)
            checkLive (testBB4E @pv)

-- | Receive 3 blocks where the first round is skipped due to timeout.
testReceiveWithTimeout :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testReceiveWithTimeout sProtocolVersion =
    it "skip round 1, receive rounds 2,3,4" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            let b2' = signedPB testBB2'
            succeedReceiveBlock b2'
            succeedReceiveBlock $ signedPB testBB3'
            succeedReceiveBlock $ signedPB testBB4'
            checkFinalized b2'

-- | Receive a block twice.
testReceiveDuplicate :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testReceiveDuplicate sProtocolVersion =
    it "receive duplicate block" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlock $ signedPB testBB1
            res <- uponReceivingBlock $ signedPB testBB1
            liftIO $ res `shouldBe` BlockResultDuplicate

-- | Receive an invalid block twice.
testReceiveInvalidDuplicate :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testReceiveInvalidDuplicate sProtocolVersion =
    it "receive invalid duplicate block" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            let badBlock = signedPB badBakedBlock
            succeedReceiveBlockFailExecute badBlock
            res <- uponReceivingBlock badBlock
            liftIO $ res `shouldBe` BlockResultDuplicate
  where
    badBakedBlock =
        (testBB1 @pv)
            { bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
                SBlockHashVersion0 ->
                    DerivableBlockHashesV0
                        { dbhv0TransactionOutcomesHash = emptyBlockTOHV1 2,
                          dbhv0BlockStateHash = StateHashV0 minBound
                        }
                SBlockHashVersion1 ->
                    DerivableBlockHashesV1
                        { dbhv1BlockResultHash = BlockResultHash minBound
                        }
            }

testReceiveStale ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveStale sProtocolVersion =
    it "receive stale round" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            mapM_ (succeedReceiveBlock . signedPB) [testBB2', testBB3', testBB4']
            res <- uponReceivingBlock (signedPB testBB1)
            liftIO $ res `shouldBe` BlockResultStale

testReceiveEpoch ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveEpoch sProtocolVersion =
    it "epoch transition" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB3E, testBB4E]

-- | Test that blocks are correctly marked dead when a block gets finalized.
--  Test that blocks arriving on dead branches are handled as stale.
testReceiveBlockDies ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveBlockDies sProtocolVersion =
    it "block dies on old branch" $
        runTestMonad @pv noBaker testTime (genesisData sProtocolVersion) $ do
            -- Receive blocks testBB1, testBB2', testBB2 and testBB3.
            mapM_ (succeedReceiveBlock . signedPB) [testBB1, testBB2']
            -- (testBB2 and testBB2' are both in round 2, so the second is handled as a duplicate)
            duplicateReceiveBlock $ signedPB testBB2
            succeedReceiveBlock $ signedPB testBB3
            -- Receiving testBB3 finalizes testBB1, so testBB2' should now be dead
            checkDead (testBB2' @pv)
            -- Check that blocks descending from a dead block (testBB3') and an old finalized block
            -- (testBB3'') fail.
            mapM_ (staleReceiveBlock . signedPB) [testBB3', testBB3'']

-- | Test receiving a block from future epochs.
testReceiveBadFutureEpoch ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveBadFutureEpoch sProtocolVersion =
    it "receive a block in a bad future epoch" $
        runTestMonad @pv noBaker testTime (genesisData sProtocolVersion) $ do
            invalidReceiveBlock $ signedPB ((testBB1 @pv){bbEpoch = 500})
            invalidReceiveBlock $ signedPB ((testBB1 @pv){bbEpoch = 2})

-- | Test receiving a block from a past epoch.
testReceiveBadPastEpoch ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveBadPastEpoch sProtocolVersion =
    it "receive a block in a bad past epoch" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB3E]
            invalidReceiveBlock $ signedPB (testBB4' @pv){bbQuorumCertificate = bbQuorumCertificate (testBB4E @pv)}

-- | Test receiving a block from future epochs.
testReceiveBadEpochTransition ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveBadEpochTransition sProtocolVersion =
    it "receive a block in the next epoch where the transition is not allowed" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            invalidReceiveBlock $ signedPB ((testBB1 @pv){bbEpoch = 1})

-- | Test receiving a block with an incorrect signature, followed by the same block with the
--  correct signature.
testReceiveBadSignature ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveBadSignature sProtocolVersion =
    it "receive a block with a bad signature" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            invalidReceiveBlock $
                PendingBlock
                    { pbBlock = invalidSignBlock testBB1,
                      pbReceiveTime = testTime
                    }
            -- Receiving the same block with the correct signature should still succeed afterwards.
            succeedReceiveBlock $ signedPB testBB1

-- | Test receiving a block baked by the wrong baker.
testReceiveWrongBaker ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveWrongBaker sProtocolVersion =
    it "receive a block from a baker that did not win the round" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            let claimedBakerId = 0
            invalidReceiveBlock $
                signedPB
                    (testBB1 @pv)
                        { bbBaker = claimedBakerId,
                          bbNonce =
                            computeBlockNonce
                                (genesisLEN sProtocolVersion)
                                1
                                (bakerVRFKey sProtocolVersion claimedBakerId)
                        }

-- | Test receiving an (out-of-order) block from the far future.
testReceiveEarlyUnknownParent ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveEarlyUnknownParent sProtocolVersion =
    it "receive a block from the future with an unknown parent" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            res <- uponReceivingBlock $ (signedPB testBB2E){pbReceiveTime = testTime}
            liftIO $ res `shouldBe` BlockResultEarly

-- | Test receiving a block with an unknown parent and incorrect signature, followed by the parent
--  block and the same block with the correct signature.
testReceiveBadSignatureUnknownParent ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveBadSignatureUnknownParent sProtocolVersion =
    it "receive a block with a bad signature and unknown parent" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            pendingReceiveBlock $
                PendingBlock
                    { pbBlock = invalidSignBlock testBB2,
                      pbReceiveTime = testTime
                    }
            -- Receiving the same block with the correct signature should still succeed afterwards.
            mapM_ (succeedReceiveBlock . signedPB) [testBB1, testBB2]

-- | Test receiving a block with an unknown parent from a far distant epoch. (But the timestamp
--  is still in the present.)
testReceiveFutureEpochUnknownParent ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveFutureEpochUnknownParent sProtocolVersion =
    it "receive a block with an unknown parent and epoch" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            pendingReceiveBlock $ signedPB (testBB2 @pv){bbEpoch = 30}

-- | Test receiving a block where the QC is not consistent with the round of the parent block.
testReceiveInconsistentQCRound ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveInconsistentQCRound sProtocolVersion =
    it "receive a block with QC round inconsistent with the parent block" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlock $ signedPB testBB1
            succeedReceiveBlockFailExecute $
                signedPB
                    (testBB2 @pv)
                        { bbQuorumCertificate = (bbQuorumCertificate (testBB2 @pv)){qcRound = 0}
                        }

-- | Test receiving a block where the QC is not consistent with the epoch of the parent block.
testReceiveInconsistentQCEpoch ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveInconsistentQCEpoch sProtocolVersion =
    it "receive a block with QC epoch inconsistent with the parent block" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlock $ signedPB testBB1
            succeedReceiveBlockFailExecute $
                signedPB
                    (testBB2 @pv){bbQuorumCertificate = (bbQuorumCertificate (testBB2 @pv)){qcEpoch = 1}}

-- | Test receiving a block where the round is earlier than or equal to the round of the parent
--  block.
testReceiveRoundInconsistent ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveRoundInconsistent sProtocolVersion =
    it "receive a block with round before parent block round" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlock $ signedPB testBB2'
            -- Earlier round
            succeedReceiveBlockFailExecute $
                signedPB
                    (testBB1 @pv){bbQuorumCertificate = validQCFor @pv testBB2'}
            -- Equal round
            duplicateReceiveBlockFailExecute $ signedPB (testBB2 @pv){bbQuorumCertificate = validQCFor @pv testBB2'}

-- | Test processing a block where the epoch is inconsistent with the parent block's epoch.
testProcessEpochInconsistent ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testProcessEpochInconsistent sProtocolVersion =
    it "process a block with epoch before parent block epoch" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB3E]
            -- We skip 'uponReceivingBlock' and go straight to processing, because the condition would be
            -- caught in 'uponReceivingBlock'.
            let pb = signedPB (testBB4' @pv){bbQuorumCertificate = bbQuorumCertificate (testBB4E @pv)}
            bf <- use $ epochBakers . currentEpochBakers
            executeBlock $
                VerifiedBlock
                    { vbBlock = pb,
                      vbBakersAndFinalizers = bf,
                      vbBakerInfo = fromJust $ fullBaker (genesisFullBakers sProtocolVersion) (blockBaker pb),
                      vbLeadershipElectionNonce = genesisLEN sProtocolVersion
                    }
            status <- getBlockStatus (getHash pb) =<< get
            case status of
                BlockUnknown -> return ()
                BlockDead -> return ()
                _ -> liftIO . assertFailure $ "Expected BlockUnknown or BlockDead after executeBlock, but found: " ++ show status ++ "\n" ++ show pb

-- | Test receiving a block where the block nonce is incorrect.
testReceiveIncorrectNonce ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveIncorrectNonce sProtocolVersion =
    it "receive a block with incorrect block nonce" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlockFailExecute $
                signedPB
                    (testBB1 @pv){bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 0 (bakerVRFKey sProtocolVersion (0 :: Int))}

-- | Test receiving a block where the block nonce is incorrect.
testReceiveTooFast ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveTooFast sProtocolVersion =
    it "receive a block with timestamp too soon after parent" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlockFailExecute $
                signedPB
                    (testBB1 @pv){bbTimestamp = 10}

-- | Test receiving a block that should have a timeout certificate, but doesn't.
testReceiveMissingTC ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveMissingTC sProtocolVersion =
    it "receive a block with a missing TC" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlockFailExecute $
                signedPB
                    (testBB2' @pv){bbTimeoutCertificate = Absent}

-- | Test receiving a block with a timeout certificate for an incorrect round.
testReceiveTCIncorrectRound ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveTCIncorrectRound sProtocolVersion =
    it "receive a block with a TC for incorrect round" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlockFailExecute $
                signedPB
                    (testBB2' @pv){bbTimeoutCertificate = Present (validTimeoutFor sProtocolVersion genQC 2)}
  where
    genQC = genesisQuorumCertificate (genesisHash sProtocolVersion)

-- | Test receiving a block with a timeout certificate where none is expected.
testReceiveTCUnexpected ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveTCUnexpected sProtocolVersion =
    it "receive a block with an unexpected TC" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlockFailExecute $
                signedPB
                    (testBB1 @pv){bbTimeoutCertificate = Present (validTimeoutFor sProtocolVersion genQC 0)}
  where
    genQC = genesisQuorumCertificate (genesisHash sProtocolVersion)

-- | Test receiving a block with a timeout certificate that is inconsistent with the QC, in that
--  it has a more recent QC.
testReceiveTCInconsistent1 ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveTCInconsistent1 sProtocolVersion =
    it "receive a block with a QC behind the max QC of the TC" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlockFailExecute $
                signedPB
                    (testBB2' @pv){bbTimeoutCertificate = Present (validTimeoutFor sProtocolVersion (validQCFor @pv testBB1) 1)}

-- | Test receiving a block that is the start of a new epoch, but with the previous round
--  timing out.
testReceiveTimeoutPastEpoch ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveTimeoutPastEpoch sProtocolVersion =
    it "receive a block with a timeout where the block is in a new epoch" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB4E']

-- | Test receiving a block that is the start of a new epoch, but with the previous round
--  timing out, but where the high QC in the finalization entry is higher than the QC of the parent
--  block.
testReceiveTimeoutPastEpochInvalid ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveTimeoutPastEpochInvalid sProtocolVersion =
    it "receive a block with an invalid timeout where the block is in a new epoch" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlock $ signedPB testBB1E
            succeedReceiveBlockFailExecute $ signedPB testBB3E'

-- | Test receiving a block that is the start of a new epoch, but with an epoch finalization entry
--  for a different branch.
testReceiveFinalizationBranch :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testReceiveFinalizationBranch sProtocolVersion =
    it "receive a block with an epoch finalization certificate for a different branch" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2Ex]
            succeedReceiveBlockFailExecute $ signedPB testBB3Ex

testReceiveEpochTransitionTimeout :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testReceiveEpochTransitionTimeout sProtocolVersion =
    it "receive a block with a timeout spanning epochs" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB3E, testBB5E']

testReceiveIncorrectTransactionOutcomesHash :: Assertion
testReceiveIncorrectTransactionOutcomesHash = runTestMonad noBaker testTime (genesisData SP6) $ do
    succeedReceiveBlockFailExecute $
        signedPB
            (testBB1 @'P6)
                { bbDerivableHashes =
                    DerivableBlockHashesV0
                        { dbhv0TransactionOutcomesHash = read "0000000000000000000000000000000000000000000000000000000000000000",
                          dbhv0BlockStateHash = read "dee89435dba1609a84fa62283d2f63ec50f85b9c22f8815daf348df5428ccb65"
                        }
                }

testReceiveIncorrectStateHash :: Assertion
testReceiveIncorrectStateHash = runTestMonad noBaker testTime (genesisData SP6) $ do
    succeedReceiveBlockFailExecute $
        signedPB $
            setStateHash (read "0000000000000000000000000000000000000000000000000000000000000000") testBB1

-- | Test receiving a block where the QC signature is invalid.
testReceiveInvalidQC ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveInvalidQC sProtocolVersion =
    it "receive a block with an invalid QC signature" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlock $ signedPB testBB1
            succeedReceiveBlockFailExecute $
                signedPB
                    (testBB2 @pv)
                        { bbQuorumCertificate = (bbQuorumCertificate (testBB2 @pv)){qcAggregateSignature = mempty}
                        }

-- | Test receiving the first block in a new epoch where the parent block is not descended from
--  the successor block in the finalization entry for the trigger block.
testReceiveTimeoutEpochTransition1 ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveTimeoutEpochTransition1 sProtocolVersion =
    it "receive blocks with a timeout before an epoch transition" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            mapM_
                (succeedReceiveBlock . signedPB)
                [ testBB1E,
                  testBB3EA,
                  testBB4EA
                ]
            checkFinalized (testBB1E @pv)

-- | Test calling 'makeBlock' in the first round for a baker that should produce a block.
testMakeFirstBlock ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testMakeFirstBlock sProtocolVersion =
    it "make a block as baker 2" $
        runTestMonad (baker sProtocolVersion bakerId) testTime (genesisData sProtocolVersion) $ do
            ((), r) <- listen makeBlock
            let expectBlock = validSignBlock (testBB1 @pv){bbTimestamp = utcTimeToTimestamp testTime}
            let qsm =
                    QuorumSignatureMessage
                        { qsmGenesis = genesisHash sProtocolVersion,
                          qsmBlock = getHash expectBlock,
                          qsmRound = blockRound expectBlock,
                          qsmEpoch = blockEpoch expectBlock
                        }
            let qsig =
                    signQuorumSignatureMessage
                        qsm
                        (bakerAggregationKey . fst $ bakers sProtocolVersion !! bakerId)
            let expectQM = buildQuorumMessage qsm qsig (FinalizerIndex $ fromIntegral bakerId)
            liftIO $ assertEqual "Produced events (makeBlock)" [OnBlock (NormalBlock expectBlock)] r
            timers <- getPendingTimers
            case Map.toAscList timers of
                [(0, (DelayUntil t, a))]
                    | t == testTime -> do
                        clearPendingTimers
                        ((), r2) <- listen a
                        liftIO $
                            assertEqual
                                "Produced events (after first timer)"
                                [SendBlock expectBlock, SendQuorumMessage expectQM]
                                r2
                        timers2 <- getPendingTimers
                        liftIO $ assertBool "Timers should not be pending" (null timers2)
                _ -> timerFail timers
  where
    bakerId = 2
    timerFail timers =
        liftIO $
            assertFailure $
                "Expected a single timer event at "
                    ++ show testTime
                    ++ " but got: "
                    ++ show (fst <$> timers)

-- | Test calling 'makeBlock' in the first round for a baker that should produce a block.
--  We try to make the block earlier than it should be, which should succeed, but delay sending
--  the block.
testMakeFirstBlockEarly ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testMakeFirstBlockEarly sProtocolVersion =
    it "make a block as baker 2 early" $
        runTestMonad (baker sProtocolVersion bakerId) curTime (genesisData sProtocolVersion) $ do
            ((), r) <- listen makeBlock
            let expectBlock = validSignBlock @pv (testBB1 @pv){bbTimestamp = utcTimeToTimestamp blkTime}
            let qsm =
                    QuorumSignatureMessage
                        { qsmGenesis = genesisHash sProtocolVersion,
                          qsmBlock = getHash expectBlock,
                          qsmRound = blockRound expectBlock,
                          qsmEpoch = blockEpoch expectBlock
                        }
            let qsig = signQuorumSignatureMessage qsm (bakerAggregationKey . fst $ bakers sProtocolVersion !! bakerId)
            let expectQM = buildQuorumMessage qsm qsig (FinalizerIndex $ fromIntegral bakerId)
            liftIO $ assertEqual "Produced events (makeBlock)" [OnBlock (NormalBlock expectBlock)] r
            timers <- getPendingTimers
            case Map.toAscList timers of
                [(0, (DelayUntil t, a))]
                    | t == blkTime -> do
                        clearPendingTimers
                        ((), r2) <- listen a
                        liftIO $
                            assertEqual
                                "Produced events (after first timer)"
                                [SendBlock expectBlock]
                                r2
                        timers2 <- getPendingTimers
                        case Map.toAscList timers2 of
                            [(0, (DelayUntil t2, a2))]
                                | t2 == blkTime -> do
                                    ((), r3) <- listen a2
                                    liftIO $
                                        assertEqual
                                            "Produced events (after second timer)"
                                            [SendQuorumMessage expectQM]
                                            r3
                            _ -> timerFail timers2
                        return ()
                _ -> timerFail timers
  where
    curTime = timestampToUTCTime 250
    blkTime = timestampToUTCTime 1_000
    bakerId = 2
    timerFail timers =
        liftIO $
            assertFailure $
                "Expected a single timer event at "
                    ++ show blkTime
                    ++ " but got: "
                    ++ show (fst <$> timers)

-- | Test calling 'makeBlock' in the first round for a baker that should not produce a block.
testNoMakeFirstBlock ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testNoMakeFirstBlock sProtocolVersion =
    it "fail to make a block as baker 0" $
        runTestMonad (baker sProtocolVersion 0) testTime (genesisData sProtocolVersion) $ do
            ((), r) <- listen makeBlock
            liftIO $ assertEqual "Produced events" [] r
            timers <- getPendingTimers
            liftIO $ assertBool "Pending timers should be empty" (null timers)

-- | Test making a block in the second round, after sending timeout messages to time out the
--  first round, where the baker should win the second round.
testTimeoutMakeBlock ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testTimeoutMakeBlock sProtocolVersion =
    it "make a block after first round timeout" $
        runTestMonad (baker sProtocolVersion bakerId) testTime (genesisData sProtocolVersion) $ do
            let genQC = genesisQuorumCertificate (genesisHash sProtocolVersion)
            -- Once the timeout certificate is generated, 'processTimeout' calls 'makeBlock'.
            ((), r) <- listen $ mapM_ processTimeout $ timeoutMessagesFor sProtocolVersion genQC 1 0
            let expectBlock =
                    validSignBlock
                        (testBB2' @pv)
                            { bbTimestamp = utcTimeToTimestamp testTime,
                              bbTimeoutCertificate =
                                Present $
                                    validTimeoutForFinalizers sProtocolVersion [0 .. 3] genQC 1
                            }
            let qsm =
                    QuorumSignatureMessage
                        { qsmGenesis = genesisHash sProtocolVersion,
                          qsmBlock = getHash expectBlock,
                          qsmRound = blockRound expectBlock,
                          qsmEpoch = blockEpoch expectBlock
                        }
            let qsig = signQuorumSignatureMessage qsm (bakerAggregationKey . fst $ bakers sProtocolVersion !! bakerId)
            let expectQM = buildQuorumMessage qsm qsig (FinalizerIndex $ fromIntegral bakerId)
            liftIO $
                assertEqual
                    "Produced events (processTimeout)"
                    [ ResetTimer 10_000,
                      OnBlock (NormalBlock expectBlock)
                    ]
                    r
            timers1 <- getPendingTimers
            case Map.toAscList timers1 of
                [(0, (DelayUntil t, a))]
                    | t == testTime -> do
                        clearPendingTimers
                        ((), r2) <- listen a
                        liftIO $
                            assertEqual
                                "Produced events (after first timer)"
                                [SendBlock expectBlock, SendQuorumMessage expectQM]
                                r2
                        timers2 <- getPendingTimers
                        liftIO $ assertBool "Timers should not be pending" (null timers2)
                _ -> timerFail timers1
  where
    bakerId = 4
    timerFail timers =
        liftIO $
            assertFailure $
                "Expected a single timer event at "
                    ++ show testTime
                    ++ " but got: "
                    ++ show (fst <$> timers)

-- | Test that if we receive three blocks such that the QC contained in the last one justifies
--  transitioning to a new epoch, we do not sign the third block, since it is in an old epoch.
testNoSignIncorrectEpoch ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testNoSignIncorrectEpoch sProtocolVersion =
    it "refuse to sign a block in old epoch after epoch transition" $
        runTestMonad (baker sProtocolVersion 0) testTime (genesisData sProtocolVersion) $ do
            let blocks = [testBB1E, testBB2E, testBB3EX]
            ((), r) <- listen $ mapM_ (succeedReceiveBlock . signedPB) blocks
            let onblock = OnBlock . NormalBlock . validSignBlock
                expectEvents =
                    [ onblock testBB1E,
                      onblock testBB2E,
                      ResetTimer 10_000, -- Advance to round 2 from the QC in testBB2E
                      onblock testBB3EX,
                      OnFinalize (testEpochFinEntry sProtocolVersion), -- Finalize from the QC in testBB3EX
                      ResetTimer 10_000 -- Advance to round 3
                    ]
            liftIO $
                assertEqual
                    "Produced events (receive/execute block)"
                    expectEvents
                    r
            timers <- getPendingTimers
            case Map.toAscList timers of
                [(0, (DelayUntil to0, a0)), (1, (DelayUntil to1, a1)), (2, (DelayUntil to2, a2))]
                    | to0 == timestampToUTCTime (bbTimestamp (testBB1E @pv)),
                      to1 == timestampToUTCTime (bbTimestamp (testBB2E @pv)),
                      to2 == timestampToUTCTime (bbTimestamp (testBB3EX @pv)) ->
                        do
                            clearPendingTimers
                            ((), r0) <- listen a0
                            liftIO $ assertEqual "Timer 0 events" [] r0
                            ((), r1) <- listen a1
                            liftIO $ assertEqual "Timer 1 events" [] r1
                            ((), r2) <- listen a2
                            liftIO $ assertEqual "Timer 2 events" [] r2
                _ -> liftIO $ assertFailure $ "Unexpected timers: " ++ show (fst <$> timers)

-- | Test that if we receive 3 blocks that transition into a new epoch as a finalizer, we
--  will sign the last of the blocks.
testSignCorrectEpoch ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testSignCorrectEpoch sProtocolVersion =
    it "sign a block in a new epoch" $
        runTestMonad (baker sProtocolVersion bakerId) testTime (genesisData sProtocolVersion) $ do
            ((), r) <- listen $ mapM_ (succeedReceiveBlock . signedPB) blocks
            let onblock = OnBlock . NormalBlock . validSignBlock
                expectEvents =
                    [ onblock testBB1E,
                      onblock testBB2E,
                      ResetTimer 10_000, -- Advance to round 2 from the QC in testBB2E
                      onblock testBB3E,
                      OnFinalize (testEpochFinEntry sProtocolVersion), -- Finalize from the epoch finalization entry in testBB3E
                      ResetTimer 10_000 -- Advance to round 3
                    ]
            liftIO $
                assertEqual
                    "Produced events (receive/execute block)"
                    expectEvents
                    r
            timers <- getPendingTimers
            case Map.toAscList timers of
                [(0, (DelayUntil to0, a0)), (1, (DelayUntil to1, a1)), (2, (DelayUntil to2, a2))]
                    | to0 == timestampToUTCTime (bbTimestamp (testBB1E @pv)),
                      to1 == timestampToUTCTime (bbTimestamp (testBB2E @pv)),
                      to2 == timestampToUTCTime (bbTimestamp (testBB3E @pv)) ->
                        do
                            clearPendingTimers
                            ((), r0) <- listen a0
                            liftIO $ assertEqual "Timer 0 events" [] r0
                            ((), r1) <- listen a1
                            liftIO $ assertEqual "Timer 1 events" [] r1
                            ((), r2) <- listen a2

                            liftIO $ assertEqual "Timer 2 events" [SendQuorumMessage expectQM] r2
                _ -> liftIO $ assertFailure $ "Unexpected timers: " ++ show (fst <$> timers)
  where
    bakerId = 0
    blocks = [testBB1E, testBB2E, testBB3E]
    qsm =
        QuorumSignatureMessage
            { qsmGenesis = genesisHash sProtocolVersion,
              qsmBlock = getHash (testBB3E @pv),
              qsmRound = bbRound (testBB3E @pv),
              qsmEpoch = bbEpoch (testBB3E @pv)
            }
    qsig = signQuorumSignatureMessage qsm (bakerAggregationKey . fst $ bakers sProtocolVersion !! bakerId)
    expectQM = buildQuorumMessage qsm qsig (FinalizerIndex $ fromIntegral bakerId)

-- | Test that if we receive 3 blocks that transition into a new epoch as a finalizer, we
--  will sign the last of the blocks. Here, the blocks arrive in reverse order to begin with.
testSignCorrectEpochReordered ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testSignCorrectEpochReordered sProtocolVersion =
    it "sign a block in a new epoch where blocks are reordered" $
        runTestMonad (baker sProtocolVersion bakerId) testTime (genesisData sProtocolVersion) $ do
            ((), events0) <- listen $ pendingReceiveBlock $ signedPB testBB3E
            liftIO $ assertEqual "Events after receiving testBB3E" [] events0
            ((), events1) <- listen $ pendingReceiveBlock $ signedPB testBB2E
            liftIO $ assertEqual "Events after receiving testBB2E" [] events1
            ((), events2) <- listen $ succeedReceiveBlock $ signedPB testBB1E
            let onblock = OnBlock . NormalBlock . validSignBlock
                expectEvents3 = [onblock testBB2E, ResetTimer 10_000]
                expectEvents4 = [onblock testBB3E, OnFinalize (testEpochFinEntry sProtocolVersion), ResetTimer 10_000]
            liftIO $ assertEqual "Events after receiving testBB1E" [onblock testBB1E] events2
            ((), events3) <- listen $ succeedReceiveBlock $ signedPB testBB2E
            liftIO $ assertEqual "Events after receiving testBB2E" expectEvents3 events3
            ((), events4) <- listen $ succeedReceiveBlock $ signedPB testBB3E
            liftIO $ assertEqual "Events after receiving testBB3E" expectEvents4 events4
            timers <- getPendingTimers
            case Map.toAscList timers of
                [(0, (DelayUntil to0, a0)), (1, (DelayUntil to1, a1)), (2, (DelayUntil to2, a2))]
                    | to0 == timestampToUTCTime (bbTimestamp (testBB1E @pv)),
                      to1 == timestampToUTCTime (bbTimestamp (testBB2E @pv)),
                      to2 == timestampToUTCTime (bbTimestamp (testBB3E @pv)) ->
                        do
                            clearPendingTimers
                            ((), r0) <- listen a0
                            liftIO $ assertEqual "Timer 0 events" [] r0
                            ((), r1) <- listen a1
                            liftIO $ assertEqual "Timer 1 events" [] r1
                            ((), r2) <- listen a2

                            liftIO $ assertEqual "Timer 2 events" [SendQuorumMessage expectQM] r2
                _ -> liftIO $ assertFailure $ "Unexpected timers: " ++ show (fst <$> timers)
  where
    bakerId = 0
    qsm =
        QuorumSignatureMessage
            { qsmGenesis = genesisHash sProtocolVersion,
              qsmBlock = getHash (testBB3E @pv),
              qsmRound = bbRound (testBB3E @pv),
              qsmEpoch = bbEpoch (testBB3E @pv)
            }
    qsig = signQuorumSignatureMessage qsm (bakerAggregationKey . fst $ bakers sProtocolVersion !! bakerId)
    expectQM = buildQuorumMessage qsm qsig (FinalizerIndex $ fromIntegral bakerId)

-- | Test calling 'makeBlock' to construct the first block in a new epoch.
testMakeBlockNewEpoch ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testMakeBlockNewEpoch sProtocolVersion =
    it "make first block in a new epoch" $
        runTestMonad (baker sProtocolVersion bakerId) testTime (genesisData sProtocolVersion) $ do
            -- We use 'testBB3EX' because it contains a QC that finalizes the trigger block.
            mapM_ (succeedReceiveBlock . signedPB) [testBB1E, testBB2E, testBB3EX]
            clearPendingTimers
            ((), r) <- listen makeBlock
            let expectBlock = validSignBlock testBB3E
            liftIO $ assertEqual "Produced events (makeBlock)" [OnBlock (NormalBlock expectBlock)] r
            timers <- getPendingTimers
            let bb3Time = timestampToUTCTime $ bbTimestamp (testBB3E @pv)
            case Map.toAscList timers of
                [(0, (DelayUntil t, a))]
                    | t == bb3Time -> do
                        clearPendingTimers
                        ((), r2) <- listen a
                        liftIO $ assertEqual "Produced events (after first timer)" [SendBlock expectBlock] r2
                        timers2 <- getPendingTimers
                        case Map.toAscList timers2 of
                            [(0, (DelayUntil t2, a2))]
                                | t2 == bb3Time -> do
                                    ((), r3) <- listen a2
                                    liftIO $ assertEqual "Produced events (after second timer)" [SendQuorumMessage expectQM] r3
                            _ -> timerFail timers2
                        return ()
                _ -> timerFail timers
  where
    bakerId = 5
    timerFail timers =
        liftIO $
            assertFailure $
                "Expected a single timer event at "
                    ++ show testTime
                    ++ " but got: "
                    ++ show (fst <$> timers)
    qsm =
        QuorumSignatureMessage
            { qsmGenesis = genesisHash sProtocolVersion,
              qsmBlock = getHash (testBB3E @pv),
              qsmRound = bbRound (testBB3E @pv),
              qsmEpoch = bbEpoch (testBB3E @pv)
            }
    qsig = signQuorumSignatureMessage qsm (bakerAggregationKey . fst $ bakers sProtocolVersion !! bakerId)
    expectQM = buildQuorumMessage qsm qsig (FinalizerIndex $ fromIntegral bakerId)

-- | Check that a given transaction is committed in the specified blocks.
checkTxCommitted ::
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    BlockItem -> [(BlockHash, TransactionIndex)] -> TestMonad pv ()
checkTxCommitted tx committedTo = do
    ts <- lookupTransaction (getHash tx) =<< get
    liftIO $ case ts of
        Just (Live (Committed{tsResults = res})) ->
            assertEqual "Transaction committed blocks" (HM.fromList committedTo) res
        _ -> assertFailure $ "Expected transaction committed in blocks " ++ show committedTo ++ " but saw: " ++ show ts

-- | Check that a given transaction is committed in the specified blocks.
checkTxReceived ::
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    BlockItem -> TestMonad pv ()
checkTxReceived tx = do
    ts <- lookupTransaction (getHash tx) =<< get
    liftIO $ case ts of
        Just (Live (Received{})) ->
            return ()
        _ -> assertFailure $ "Expected transaction received, but saw: " ++ show ts

-- | Check that a given transaction is finalized in the specified block.
checkTxFinalized ::
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    BlockItem -> BlockHeight -> TransactionIndex -> TestMonad pv ()
checkTxFinalized tx bheight ti = do
    ts <- lookupTransaction (getHash tx) =<< get
    liftIO $
        assertEqual
            "Transaction status"
            (Just (Finalized (FinalizedTransactionStatus{ftsBlockHeight = bheight, ftsIndex = ti})))
            ts

-- | Check that a given transaction is absent from the transaction table.
checkTxAbsent :: (IsConsensusV1 pv, IsProtocolVersion pv) => BlockItem -> TestMonad pv ()
checkTxAbsent tx = do
    ts <- lookupTransaction (getHash tx) =<< get
    liftIO $ assertEqual "Transaction status" Nothing ts

-- | Receive with transactions.
testReceiveWithTransactions :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testReceiveWithTransactions sProtocolVersion =
    it "receive blocks with transactions, checking committed/finalized status" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            succeedReceiveBlock (signedPB testBB1T)
            checkTxCommitted testTrans1 [(getHash (testBB1T @pv), 0)]
            checkTxCommitted testTrans2 [(getHash (testBB1T @pv), 1)]
            succeedReceiveBlock (signedPB testBB2T)
            checkLive (testBB1T @pv)
            checkLive (testBB2T @pv)
            checkTxCommitted testTrans1 [(getHash (testBB1T @pv), 0)]
            checkTxCommitted testTrans2 [(getHash (testBB1T @pv), 1)]
            checkTxCommitted testTrans3 [(getHash (testBB2T @pv), 0)]
            checkTxCommitted testTrans4 [(getHash (testBB2T @pv), 1)]
            checkTxCommitted testTrans5 [(getHash (testBB2T @pv), 2)]
            succeedReceiveBlock (signedPB testBB3T)
            checkTxCommitted testTrans1 [(getHash (testBB1T @pv), 0)]
            checkTxCommitted testTrans2 [(getHash (testBB1T @pv), 1)]
            checkTxCommitted testTrans3 [(getHash (testBB2T @pv), 0)]
            checkTxCommitted testTrans3' [(getHash (testBB3T @pv), 0)]
            checkTxCommitted testTrans4 [(getHash (testBB2T @pv), 1), (getHash (testBB3T @pv), 1)]
            checkTxCommitted testTrans5 [(getHash (testBB2T @pv), 2)]
            succeedReceiveBlock (signedPB testBB4T)
            checkTxCommitted testTrans1 [(getHash (testBB1T @pv), 0)]
            checkTxCommitted testTrans2 [(getHash (testBB1T @pv), 1)]
            checkTxCommitted testTrans3 [(getHash (testBB2T @pv), 0)]
            checkTxCommitted testTrans3' [(getHash (testBB3T @pv), 0)]
            checkTxCommitted testTrans4 [(getHash (testBB2T @pv), 1), (getHash (testBB3T @pv), 1)]
            checkTxCommitted testTrans5 [(getHash (testBB2T @pv), 2)]
            succeedReceiveBlock (signedPB testBB5T)
            -- These are finalized in testBB1T
            checkTxFinalized testTrans1 1 0
            checkTxFinalized testTrans2 1 1
            -- This was in testBB2T, which is dropped from the finalized chain
            checkTxAbsent testTrans3
            -- These are finalized in testBB3T
            checkTxFinalized testTrans3' 2 0
            checkTxFinalized testTrans4 2 1
            -- Since testBB2T is pruned, testTrans5 is no longer in any live blocks
            checkTxReceived testTrans5

tests :: Spec
tests = describe "KonsensusV1.Consensus.Blocks" $ do
    describe "uponReceiveingBlockPV" $ do
        Common.forEveryProtocolVersionConsensusV1 $ \spv pvString ->
            describe pvString $ do
                testReceive3 spv
                testReceive3Reordered spv
                testReceive4Reordered spv
                testReceiveWithTimeout spv
                testReceiveDuplicate spv
                testReceiveInvalidDuplicate spv
                testReceiveStale spv
                testReceiveEpoch spv
                testReceiveBlockDies spv
                testReceiveBadFutureEpoch spv
                testReceiveBadPastEpoch spv
                testReceiveBadEpochTransition spv
                testReceiveBadSignature spv
                testReceiveWrongBaker spv
                testReceiveEarlyUnknownParent spv
                testReceiveBadSignatureUnknownParent spv
                testReceiveFutureEpochUnknownParent spv
                testReceiveInconsistentQCRound spv
                testReceiveInconsistentQCEpoch spv
                testReceiveRoundInconsistent spv
                testProcessEpochInconsistent spv
                testReceiveIncorrectNonce spv
                testReceiveTooFast spv
                testReceiveMissingTC spv
                testReceiveTCIncorrectRound spv
                testReceiveTCUnexpected spv
                testReceiveTCInconsistent1 spv
                testReceiveTimeoutPastEpoch spv
                testReceiveTimeoutPastEpochInvalid spv
                testReceiveFinalizationBranch spv
                testReceiveEpochTransitionTimeout spv
                testReceiveInvalidQC spv
                testMakeFirstBlock spv
                testMakeFirstBlockEarly spv
                testNoMakeFirstBlock spv
                testTimeoutMakeBlock spv
                testNoSignIncorrectEpoch spv
                testSignCorrectEpoch spv
                testSignCorrectEpochReordered spv
                testMakeBlockNewEpoch spv
                testReceiveTimeoutEpochTransition1 spv
                testReceiveWithTransactions spv
    describe "uponReceiveingBlock (P6 only)" $ do
        it "receive a block with an incorrect transaction outcomes hash" testReceiveIncorrectTransactionOutcomesHash
        it "receive a block with an incorrect state hash" testReceiveIncorrectStateHash
