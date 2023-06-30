{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |A module that tests the in-band catch-up mechanism of KonsensusV1.
module ConcordiumTests.KonsensusV1.CatchUp where

import Control.Monad.State.Strict
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import System.Random
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.DummyData (randomBlsSecretKey)
import qualified Concordium.Crypto.DummyData as Dummy
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Genesis.Data
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.GlobalState.Persistent.BlobStore (BlobRef (..))
import Concordium.Startup
import Concordium.Types
import Concordium.Types.BakerIdentity
import qualified Concordium.Types.DummyData as Dummy
import Concordium.Types.HashableTo
import Concordium.Utils
import qualified Data.Map.Strict as Map

import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.CatchUp
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types.Transactions

import ConcordiumTests.KonsensusV1.TreeStateTest (dummyBlockState)

-- |Create genesis for running the tests in this module.
-- There are 3 bakers/finalizers and one additional foundation account.
genesisData :: GenesisData 'P6
bakers :: [(BakerIdentity, FullBakerInfo)]
(genesisData, bakers, _) =
    makeGenesisDataV1
        0
        3
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

-- |Helper function for getting the private key of
-- a particular baker in the 'bakers' used for these tests.
bakerKey :: Integral a => a -> BakerSignPrivateKey
bakerKey i = bakerSignKey $ fst (bakers !! fromIntegral i)

-- |Signs a baked block
signMyBlock :: BakedBlock -> SignedBlock
signMyBlock bb = signBlock (bakerKey (bbBaker bb)) genesisHash bb

-- |Hash of the genesis block.
genesisHash :: BlockHash
genesisHash = BlockHash $ Hash.hash "My dummy genesis hash"

-- |A type that informs 'advanceRound' of how it should
-- create the new block pointer.
data AdvanceRound pv
    = -- Make a block pointer for the provided 'Round' with the last certified block
      -- being the block pointer.
      Timeout !Round !(BlockPointer pv)
    | -- Make a block that extends the provided block pointer and
      -- contains a QC for it.
      Quorum !(BlockPointer pv)

blockMakeRound :: AdvanceRound pv -> Round
blockMakeRound (Timeout r _) = r
blockMakeRound (Quorum bp) = blockRound bp

blockMakeBp :: AdvanceRound pv -> BlockPointer pv
blockMakeBp (Timeout _ bp) = bp
blockMakeBp (Quorum bp) = bp

-- |Make a dummy 'BlockPointer' that is a continuation of either a timeout
-- or a block.
advanceRound :: AdvanceRound pv -> BlockPointer pv
advanceRound adv = makeBlockPointer
  where
    makeBlockPointer =
        BlockPointer
            { bpInfo = metadata,
              bpBlock = theBlock,
              bpState = dummyBlockState
            }
    makeTC parentBp r =
        TimeoutCertificate
            { tcRound = r,
              tcMinEpoch = blockEpoch parentBp,
              tcFinalizerQCRoundsFirstEpoch = FinalizerRounds Map.empty,
              tcFinalizerQCRoundsSecondEpoch = FinalizerRounds Map.empty,
              tcAggregateSignature = mempty
            }
    metadata =
        BlockMetadata
            { bmHeight = (BlockHeight 1) + (blockHeight $ blockMakeBp adv),
              bmReceiveTime = timestampToUTCTime 0,
              bmArriveTime = timestampToUTCTime 0,
              bmEnergyCost = 0,
              bmTransactionsSize = 0
            }
    theBlock = case adv of
        (Timeout r bp) -> NormalBlock $ dummySignedBlock bp theRound (Present $ makeTC bp r)
        (Quorum bp) -> NormalBlock $ dummySignedBlock bp theRound Absent
      where
        theRound = Round 1 + blockMakeRound adv

-- |Generate a dummy quorum certificate for the provided block pointer.
-- The round is incremented otherwise the qc contains dummy values.
makeQCForBlockPointer :: BlockPointer pv -> QuorumCertificate
makeQCForBlockPointer parentBp =
    QuorumCertificate
        { qcBlock = getHash parentBp,
          qcRound = 1 + blockRound parentBp,
          qcEpoch = blockEpoch parentBp,
          qcAggregateSignature = mempty,
          qcSignatories = FinalizerSet 0
        }

-- |Create a 'CertifiedBlock' with the provided 'BlockPointer' and a dummy
-- 'QuorumCertificate'.
certifyBlock :: BlockPointer pv -> CertifiedBlock pv
certifyBlock bp =
    CertifiedBlock
        { cbQuorumBlock = bp,
          cbQuorumCertificate = dummyQC roundForQC parentHash
        }
  where
    roundForQC = blockRound bp
    parentHash = getHash bp

dummyNormalBlock :: BlockPointer pv -> Round -> Option TimeoutCertificate -> Block pv
dummyNormalBlock parent n oTC = NormalBlock $ dummySignedBlock parent n oTC

dummyQC :: Round -> BlockHash -> QuorumCertificate
dummyQC n parentHash =
    QuorumCertificate
        { qcBlock = parentHash,
          qcRound = n,
          qcEpoch = 1,
          qcAggregateSignature = QuorumSignature $ Bls.sign "quorum certificate" $ fst $ randomBlsSecretKey (mkStdGen 42),
          qcSignatories = FinalizerSet 0
        }

dummyBakedBlock :: BlockHash -> Round -> Option TimeoutCertificate -> BakedBlock
dummyBakedBlock parentHash n oTC =
    BakedBlock
        { bbRound = n,
          bbEpoch = 1,
          bbTimestamp = 1_000,
          bbBaker = 1,
          bbQuorumCertificate = dummyQC n parentHash,
          bbTimeoutCertificate = oTC,
          bbEpochFinalizationEntry = Absent,
          bbNonce = VRF.prove (fst $ VRF.randomKeyPair (mkStdGen 42)) "foo",
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = TransactionOutcomesHash $ Hash.hash "outcomes hash",
          bbStateHash = StateHashV0 $ Hash.hash "state hash"
        }

dummySignedBlock :: BlockPointer pv -> Round -> Option TimeoutCertificate -> SignedBlock
dummySignedBlock parent n oTC = signMyBlock $ dummyBakedBlock parentHash n oTC
  where
    parentHash = getHash parent

genesisBlockPointer :: BlockPointer pv
genesisBlockPointer =
    let bpInfo =
            BlockMetadata
                { bmHeight = 0,
                  bmReceiveTime = timestampToUTCTime 0,
                  bmArriveTime = timestampToUTCTime 0,
                  bmEnergyCost = 0,
                  bmTransactionsSize = 0
                }
        bpBlock =
            GenesisBlock
                GenesisMetadata
                    { gmStateHash = getHash bpState,
                      gmParameters = genesisCoreParametersV1 genesisData,
                      gmFirstGenesisHash = genesisBlockHash genesisData,
                      gmCurrentGenesisHash = genesisBlockHash genesisData
                    }
        bpState = dummyBlockState
    in  BlockPointer{..}

dummyStoredBlock :: Maybe (BlockPointer pv) -> BlockHeight -> Round -> Option TimeoutCertificate -> StoredBlock pv
dummyStoredBlock Nothing h n oTC = StoredBlock (BlockMetadata h dummyTime dummyTime 0 0) (dummyNormalBlock genesisBlockPointer n oTC) (BlobRef 0)
dummyStoredBlock (Just bp) h n oTC = StoredBlock (BlockMetadata h dummyTime dummyTime 0 0) (dummyNormalBlock bp n oTC) (BlobRef 0)

dummyTime :: UTCTime
dummyTime = posixSecondsToUTCTime 0

dummyFinalizationEntry :: QuorumCertificate -> QuorumCertificate -> FinalizationEntry
dummyFinalizationEntry finQC sucQC =
    FinalizationEntry
        { feFinalizedQuorumCertificate = finQC,
          feSuccessorQuorumCertificate = sucQC,
          feSuccessorProof = BlockQuasiHash $ Hash.hash "quasi hash"
        }

assertCatchupResponse :: MonadIO m => CatchUpTerminalData -> [SignedBlock] -> CatchUpPartialResponse m -> m ()
assertCatchupResponse term [] resp = case resp of
    CatchUpPartialResponseDone actualTerm -> liftIO $ assertEqual "Unexpected terminal data" term actualTerm
    CatchUpPartialResponseBlock{..} ->
        cuprFinish >>= \case
            Absent -> liftIO $ assertFailure "Expected a terminal data as all expected blocks has been served." -- todo modify this a bit so we carry an accummulator and can check limit.
            Present actualTerm -> liftIO $ assertEqual "Unexpected terminal data" term actualTerm
assertCatchupResponse term (x : xs) resp = case resp of
    CatchUpPartialResponseDone{} -> liftIO $ assertFailure "Unexpected done result. There is still expected blocks to be served."
    CatchUpPartialResponseBlock{..} -> do
        liftIO $ assertEqual "Unexpected block served" x cuprNextBlock
        assertCatchupResponse term xs =<< cuprContinue

-- |Run a TestMonad pv a action with a no-baker context,
-- fixed 'genesisData' and fixed time.
runTest :: TestMonad 'P6 a -> IO a
runTest = runTestMonad @'P6 noBaker time genesisData
  where
    noBaker = BakerContext Nothing
    time = timestampToUTCTime 1_000

-- |A test where node N tries to catch up with peer P.
--
-- The state of N is as follows:
-- * Block 0 is the last finalized block.
-- * Last finalized round is round 0
-- * current round is round 1
--
-- The state of P is as follows:
-- * Blocks 0,1,2 are finalized
-- * Highest certified block is block 3.
-- * P also has a block 4.
-- * P is in round 4 and has received 1 quorum message pointing to block 4.
-- * P has also received a timeout message in round 4.
catchupNoBranches :: Assertion
catchupNoBranches = runTest $ do
    -- set current round to 5
    roundStatus . rsCurrentRound .= Round 5
    -- Blocks 0,1 and 2 are finalized
    storedBlockRound1 <- getStoredBlockRound1
    storedBlockRound2 <- getStoredBlockRound2
    block3 <- makeBlock3
    let finQC = dummyQC (Round 2) $ getHash storedBlockRound2
        sucQC = dummyQC (Round 3) $ getHash block3
        finEntry = dummyFinalizationEntry finQC sucQC
    let writeCert sb = writeCertifiedBlock sb (dummyQC (blockRound sb) (getHash sb))
    writeCert storedBlockRound0
    writeCert storedBlockRound1
    writeCert storedBlockRound2
    writeFinalizedBlocks [storedBlockRound0, storedBlockRound1, storedBlockRound2] finEntry
    latestFinalizationEntry .= Present finEntry
    lfb <- mkBlockPointer storedBlockRound2
    lastFinalized .=! lfb
    -- block in round 3 is the highest certified block.
    addToBranches block3
    blockTable . liveMap . at' (getHash block3) ?=! MemBlockAlive block3
    let block3Certified = certifyBlock block3
    roundStatus . rsHighestCertifiedBlock .= block3Certified
    -- the highest block
    let block4 = advanceRound $ Quorum block3
    addToBranches block4
    blockTable . liveMap . at' (getHash block4) ?=! MemBlockAlive block4
    let finToQMsgMap = Map.insert (FinalizerIndex 0) quorumMessageBlock4 Map.empty
        quorumMessageBlock4 =
            QuorumMessage
                { qmSignature = QuorumSignature $ Bls.sign "quorum message" $ fst $ randomBlsSecretKey (mkStdGen 42),
                  qmBlock = getHash block4,
                  qmFinalizerIndex = FinalizerIndex 0,
                  qmRound = Round 4,
                  qmEpoch = 1
                }
        finToTMMap = Map.insert (FinalizerIndex 1) timeoutMessageRound4 Map.empty
        timeoutMessageRound4 = signTimeoutMessage timeoutRound4Message genesisHash (bakerKey (1 :: Int))
        timeoutRound4Message =
            TimeoutMessageBody
                { tmFinalizerIndex = FinalizerIndex 2,
                  tmRound = Round 4,
                  tmEpoch = 1,
                  tmQuorumCertificate = dummyQC (Round 3) $ getHash block3,
                  tmAggregateSignature = TimeoutSignature $ Bls.sign "quorum certificate" $ fst $ randomBlsSecretKey (mkStdGen 42)
                }
    currentQuorumMessages .= QuorumMessages finToQMsgMap Map.empty
    currentTimeoutMessages .= Present (TimeoutMessages 1 finToTMMap Map.empty)
    -- The request to handle.
    let request =
            CatchUpStatus
                { cusLastFinalizedBlock = (getHash . stbBlock) storedBlockRound0,
                  cusLastFinalizedRound = Round 0,
                  cusLeaves = [],
                  cusBranches = [],
                  cusCurrentRound = Round 1,
                  cusCurrentRoundQuorum = Map.empty,
                  cusCurrentRoundTimeouts = Absent
                }
    let expectedTerminalData =
            CatchUpTerminalData
                { cutdLatestFinalizationEntry = Present finEntry,
                  cutdHighestQuorumCertificate = Present sucQC,
                  cutdTimeoutCertificate = Absent,
                  cutdCurrentRoundQuorumMessages = [quorumMessageBlock4],
                  cutdCurrentRoundTimeoutMessages = [timeoutMessageRound4]
                }

    b0Bp <- mkBlockPointer storedBlockRound0
    b1Bp <- mkBlockPointer storedBlockRound1
    b2Bp <- mkBlockPointer storedBlockRound2
    let expectedBlock0 = dummySignedBlock b0Bp 1 Absent
        expectedBlock1 = dummySignedBlock b1Bp 2 Absent
        expectedBlock2 = dummySignedBlock b2Bp 3 Absent
    let expectedBlocksServed = [expectedBlock0, expectedBlock1, expectedBlock2]
    skovData <- get
    resp <- handleCatchUpRequest request skovData
    assertCatchupResponse expectedTerminalData expectedBlocksServed resp
  where
    storedBlockRound0 = dummyStoredBlock Nothing 0 0 Absent
    getStoredBlockRound1 = do
        bp <- mkBlockPointer storedBlockRound0
        return $ dummyStoredBlock (Just bp) 1 1 Absent
    getStoredBlockRound2 = do
        storedBlockRound1 <- getStoredBlockRound1
        bp <- mkBlockPointer storedBlockRound1
        return $ dummyStoredBlock (Just bp) 2 2 Absent
    makeBlock3 = do
        storedBlockRound2 <- getStoredBlockRound2
        bp <- mkBlockPointer storedBlockRound2
        return $ advanceRound $ Quorum bp

tests :: Spec
tests = describe "KonsensusV1.CatchUp" $ do
    it "Catch up no branches" catchupNoBranches
