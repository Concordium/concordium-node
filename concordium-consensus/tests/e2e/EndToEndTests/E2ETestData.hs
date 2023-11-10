{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for end-to-end tests.
module EndToEndTests.E2ETestData where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Writer.Class
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Time
import Test.HUnit

import qualified Concordium.Crypto.DummyData as Dummy
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Genesis.Data
import qualified Concordium.Genesis.Data.P6 as P6
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.LFMBTree (hashAsLFMBT)
import Concordium.GlobalState.BlockState (TransactionSummaryV1)
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Blocks
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Startup
import Concordium.Types
import Concordium.Types.BakerIdentity
import qualified Concordium.Types.DummyData as Dummy
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import qualified Concordium.Types.Transactions as Transactions

-- * Helper definitions

-- | Max bakers
noBakers :: (Integral a) => a
noBakers = 5

-- | Genesis time
genTime :: Timestamp
genTime = 0

-- | Epoch duration
genEpochDuration :: Duration
genEpochDuration = 3_600_000

-- | Genesis data used for E2E credential deployments
genesisData :: GenesisData 'P6
bakers :: [(BakerIdentity, FullBakerInfo)]
(genesisData, bakers, _) =
    makeGenesisDataV1
        genTime
        (noBakers + 1)
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

-- | Hash of the genesis block.
genesisHash :: BlockHash
genesisHash = genesisBlockHash genesisData

-- | Leadership election nonce at genesis
genesisLEN :: LeadershipElectionNonce
genesisLEN = genesisLeadershipElectionNonce $ P6.genesisInitialState $ unGDP6 genesisData

-- | Baker context with baker @i@.
baker :: Int -> BakerContext
baker i = BakerContext $ Just $ fst $ bakers !! i

-- | Private ED25519 key of the provided baker identifier.
bakerKey :: (Integral a) => a -> BakerSignPrivateKey
bakerKey i = bakerSignKey $ fst (bakers !! fromIntegral i)

-- | Private BLS key of the provided baker identifier.
bakerAggKey :: (Integral a) => a -> BakerAggregationPrivateKey
bakerAggKey i = bakerAggregationKey $ fst (bakers !! fromIntegral i)

-- | Private VRF key of the provided baker identifier.
bakerVRFKey :: (Integral a) => a -> BakerElectionPrivateKey
bakerVRFKey i = bakerElectionKey $ fst (bakers !! fromIntegral i)

-- | Finalizer set of all finalizers.
allFinalizers :: FinalizerSet
allFinalizers = finalizerSet $ FinalizerIndex <$> [0 .. noBakers]

-- | List of finalizers
theFinalizers :: [Int]
theFinalizers = [0 .. noBakers]

-- | Make a valid 'QuorumCertificate' for the provided block.
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

validTimeoutForFinalizers :: [Int] -> QuorumCertificate -> Round -> TimeoutCertificate
validTimeoutForFinalizers finalizers qc rnd =
    TimeoutCertificate
        { tcRound = rnd,
          tcMinEpoch = qcEpoch qc,
          tcFinalizerQCRoundsFirstEpoch = FinalizerRounds (Map.singleton (qcRound qc) finSet),
          tcFinalizerQCRoundsSecondEpoch = FinalizerRounds Map.empty,
          tcAggregateSignature =
            fold
                [signTimeoutSignatureMessage tsm (bakerAggKey i) | i <- finalizers]
        }
  where
    finSet = finalizerSet $ FinalizerIndex . fromIntegral <$> finalizers
    tsm =
        TimeoutSignatureMessage
            { tsmGenesis = genesisHash,
              tsmRound = rnd,
              tsmQCRound = qcRound qc,
              tsmQCEpoch = qcEpoch qc
            }

-- | Create a valid timeout message given a QC and a round.
--  All finalizers sign the certificate and they all have the QC as their highest QC.
validTimeoutFor :: QuorumCertificate -> Round -> TimeoutCertificate
validTimeoutFor = validTimeoutForFinalizers theFinalizers

-- | Make a valid signed block from the provided @BakedBlock@.
validSignBlock :: BakedBlock -> SignedBlock
validSignBlock bb = signBlock (bakerKey (bbBaker bb)) genesisHash bb

-- | Make a valid signed pending block.
signedPB :: BakedBlock -> PendingBlock
signedPB bb =
    PendingBlock
        { pbReceiveTime = timestampToUTCTime $ bbTimestamp bb,
          pbBlock = validSignBlock bb
        }

-- | Helper to compute the transaction outcomes hash for a given set of transaction outcomes and
--  special transaction outcomes.
transactionOutcomesHash ::
    [TransactionSummaryV1] ->
    [Transactions.SpecialTransactionOutcome] ->
    Transactions.TransactionOutcomesHash
transactionOutcomesHash outcomes specialOutcomes =
    Transactions.TransactionOutcomesHash $
        H.hashShort $
            "TransactionOutcomesHashV1"
                <> H.hashToShortByteString out
                <> H.hashToShortByteString special
  where
    lfmbHash :: (HashableTo H.Hash a) => [a] -> H.Hash
    lfmbHash = hashAsLFMBT (H.hash "EmptyLFMBTree") . fmap getHash
    out = lfmbHash outcomes
    special = lfmbHash specialOutcomes

-- | Compute the transaction outcomes hash for a block with no transactions.
emptyBlockTOH :: BakerId -> Transactions.TransactionOutcomesHash
emptyBlockTOH bid = transactionOutcomesHash [] [BlockAccrueReward 0 0 0 0 0 0 bid]

-- | Baker context with no baker.
noBaker :: BakerContext
noBaker = BakerContext Nothing

-- | Current time used for running (some) tests. 5 seconds after genesis.
testTime :: UTCTime
testTime = timestampToUTCTime 5_000

-- * Helper functions

-- | Receive a block -  assert success.
succeedReceiveBlock :: PendingBlock -> TestMonad 'P6 ()
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
