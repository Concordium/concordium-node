{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |Module testing functions from the 'Concordium.KonsensusV1.Consensus.Timeout' module.
module ConcordiumTests.KonsensusV1.Timeout (tests) where

import Control.Monad.State
import Control.Monad.Writer.Class
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.Ratio
import qualified Data.Vector as Vec
import Data.Word
import Lens.Micro.Platform
import System.Random
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.DummyData as Dummy
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Genesis.Data
import qualified Concordium.Genesis.Data as GD
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.GlobalState.Persistent.TreeState (insertDeadCache)
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Timeout
import Concordium.KonsensusV1.Consensus.Timeout.Internal
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.LowLevel.Memory
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Startup
import Concordium.Types
import Concordium.Types.BakerIdentity
import qualified Concordium.Types.DummyData as Dummy
import Concordium.Types.Transactions
import ConcordiumTests.KonsensusV1.Common
import ConcordiumTests.KonsensusV1.TreeStateTest hiding (tests)
import ConcordiumTests.KonsensusV1.Types hiding (tests)

import qualified ConcordiumTests.KonsensusV1.Consensus.Blocks as TestBlocks

-- |Test that 'updateCurrentTimeout' correctly calculates a new timeout given the current timeout and the
-- `timeoutIncrease` parameter.
testUpdateCurrentTimeout :: Duration -> Ratio Word64 -> Duration -> Assertion
testUpdateCurrentTimeout baseTimeout timeoutIncrease expectedTimeout = do
    let actualTimeout = updateCurrentTimeout timeoutIncrease baseTimeout
    assertEqual "Timeout duration should be correct" expectedTimeout actualTimeout

genesisData :: GenesisData 'P6
bakers :: [(BakerIdentity, FullBakerInfo)]
(genesisData, bakers, _) =
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

sigThreshold :: Rational
sigThreshold = 2 % 3

-- |Generate a timeout message signed by the finalizer with the finalizer index @fid@ from
-- 'bakers' above in epoch @e@ with a qc epoch @qce@.
dummyTimeoutMessage' :: Int -> Epoch -> Epoch -> TimeoutMessage
dummyTimeoutMessage' fid e qce =
    signTimeoutMessage timeoutMessageBody genesisHash $
        bakerSignKey $
            fst $
                bakers !! fid
  where
    timeoutMessageBody =
        TimeoutMessageBody
            { tmFinalizerIndex = FinalizerIndex $ fromIntegral fid,
              tmRound = 1,
              tmEpoch = e,
              tmQuorumCertificate = quorumCert,
              tmAggregateSignature = dummyTimeoutSig
            }
    dummyTimeoutSig =
        signTimeoutSignatureMessage dummyTimeoutSigMessage $
            bakerAggregationKey $
                fst $
                    bakers !! fid
    dummyTimeoutSigMessage =
        TimeoutSignatureMessage
            { tsmGenesis = genesisBlockHash genesisData,
              tsmRound = 1,
              tsmQCRound = 0,
              tsmQCEpoch = 0
            }
    quorumCert = QuorumCertificate genesisHash 0 qce mempty $ FinalizerSet 0

-- |Generate a timeout message signed by the finalizer index @fid@ from
-- 'bakers' above in epoch @e@ and qc epoch @e@.
dummyTimeoutMessage :: Int -> Epoch -> TimeoutMessage
dummyTimeoutMessage fid e = dummyTimeoutMessage' fid e e

-- |Test the function 'uponTimeoutEvent' using the baker context of the finalizer with index 0 from
-- @bakers@. This tests the following:
-- * that the expected timeout message is set in the @rsLastSignedTimeoutMessage@ field of
--   'RoundStatus'.
-- * that 'sendTimeoutMessage' has been called with the expected timeout message.
-- * that the field @receivedTimeoutMessages@ of 'SkovData' has been updated with
--   expected timeout message (via the call to 'processTimeout').
testUponTimeoutEvent :: Assertion
testUponTimeoutEvent = do
    runTestMonad baker testTime genesisData $ do
        lastSigned <- use $ persistentRoundStatus . prsLastSignedTimeoutMessage
        liftIO $ assertEqual "last signed timeout message should be absent" Absent lastSigned
        (_, events) <- listen uponTimeoutEvent
        lastSigned2 <- use $ persistentRoundStatus . prsLastSignedTimeoutMessage
        liftIO $
            assertEqual
                "last signed timeout message should be present and correct"
                (Present expectedMessage)
                lastSigned2

        liftIO $
            assertEqual
                "events should be SendTimeoutMessage"
                [SendTimeoutMessage expectedMessage]
                events
        receivedMessages <- use currentTimeoutMessages

        liftIO $
            assertEqual
                "Timeout message should be stored by processTimeout"
                expectedMessages
                receivedMessages
  where
    baker = BakerContext $ Just $ fst $ head bakers
    testTime = timestampToUTCTime 1_000
    expectedMessages =
        Present $
            TimeoutMessages 0 (Map.singleton (FinalizerIndex 0) expectedMessage) Map.empty
    expectedMessage = dummyTimeoutMessage 0 0

-- |Test 'processTimeout'.
-- The following is tested before receival of enough timeout messages to form a valid TC:
-- * that timeout messages are indeed stored in the field @receivedTimeoutMessages@ of 'SkovData'
-- * that the round is not advanced
-- * that rsPreviousRoundTC@ is still @Absent@
--
-- The following is tested after receival of enough timeout messages to form a valid TC:
-- * that the round is indeed advanced
-- * that the field @rsPreviousRoundTC@ is set with a valid TC
-- * that the field @receivedTimeoutMessages@ of 'SkovData' is now @Absent@
testProcessTimeout :: Assertion
testProcessTimeout = do
    runTestMonad noBaker testTime genesisData $ do
        currentRound <- use $ roundStatus . rsCurrentRound
        liftIO $ assertEqual "Round should be 1" 1 currentRound
        let message1 = dummyTimeoutMessage 0 0
        let message2 = dummyTimeoutMessage 2 0
        let message3 = dummyTimeoutMessage 4 0
        processTimeout message1

        actualMessages <- use currentTimeoutMessages
        let expectedMessage oldMessages newMessage =
                maybe oldMessages Present (updateTimeoutMessages oldMessages newMessage)

        let expectedMessages1 = expectedMessage Absent message1

        liftIO $ assertEqual "Timeout message should be stored" expectedMessages1 actualMessages

        currentRound1 <- use $ roundStatus . rsCurrentRound
        liftIO $
            assertEqual
                "Round should still be 1, since not enough time messages have been received to advance round"
                1
                currentRound1

        processTimeout message2
        actualMessages2 <- use currentTimeoutMessages

        let expectedMessages2 = expectedMessage actualMessages message2

        liftIO $ assertEqual "Timeout message should be stored" expectedMessages2 actualMessages2

        currentRound2 <- use $ roundStatus . rsCurrentRound
        liftIO $
            assertEqual
                "Round should still be 1, since not enough time messages have been received to advance round"
                1
                currentRound2

        previousRoundTC <- use $ roundStatus . rsPreviousRoundTimeout
        liftIO $
            assertEqual
                "Previous round TC should be absent since no TC has been formed yet"
                Absent
                previousRoundTC

        processTimeout message3
        actualMessages3 <- use currentTimeoutMessages

        let expectedMessages3 = Absent

        liftIO $
            assertEqual
                "No timeout message should be stored since we advanced round"
                expectedMessages3
                actualMessages3

        currentRound3 <- use $ roundStatus . rsCurrentRound
        liftIO $
            assertEqual
                "Round should now be 2, since enough time messages have been received to advance round"
                2
                currentRound3

        previousRoundTC2 <- use $ roundStatus . rsPreviousRoundTimeout
        case previousRoundTC2 of
            Absent -> liftIO $ assertFailure "TC should be present due to advanced round"
            Present (RoundTimeout tc _) -> do
                finComm <- use $ skovEpochBakers . currentEpochBakers . bfFinalizers
                liftIO $
                    assertBool "TC should be valid" $
                        checkTimeoutCertificate genesisHash sigThreshold finComm finComm finComm tc
  where
    noBaker = BakerContext Nothing
    testTime = timestampToUTCTime 1_000

-- |Test 'updateTimeoutMessages'.
testUpdateTimeoutMessages :: Spec
testUpdateTimeoutMessages =
    describe "Test updateTimeoutMessages" $ do
        it "Adding message, no messages already stored" $
            assertEqual "Should get stored" (Just messages1) $
                updateTimeoutMessages Absent $
                    dummyTimeoutMessage 0 0
        it "Adding message in first epoch, where one message is already stored in first epoch." $
            assertEqual "Should get stored" (Just messages2) $
                updateTimeoutMessages (Present messages1) $
                    dummyTimeoutMessage 1 0
        it "Adding message in first epoch + 1, where the qc is in epoch." $
            assertEqual "Should get stored" (Just messages3') $
                updateTimeoutMessages (Present messages1) $
                    dummyTimeoutMessage' 1 1 0
        it "Adding message in first epoch + 1, where one message is already stored in first epoch." $
            assertEqual "Should get stored" (Just messages3) $
                updateTimeoutMessages (Present messages1) $
                    dummyTimeoutMessage 1 1
        it "Adding message in first epoch, where messages are already stored in both epochs." $
            assertEqual "Should get stored" (Just messages4) $
                updateTimeoutMessages (Present messages3) $
                    dummyTimeoutMessage 2 0
        it "Adding message in first epoch + 1 = second epoch, where messages are already stored in both epochs." $
            assertEqual "Should get stored" (Just messages4') $
                updateTimeoutMessages (Present messages3) $
                    dummyTimeoutMessage 2 1
        it "Adding message in first epoch + 2, where one message is already stored in first epoch." $
            assertEqual "Should get stored" (Just messages5) $
                updateTimeoutMessages (Present messages1) $
                    dummyTimeoutMessage 1 2
        it "Adding message in first epoch + 2, where messages are already stored in both epochs." $
            assertEqual "Should get stored" (Just messages5') $
                updateTimeoutMessages (Present messages3) $
                    dummyTimeoutMessage 2 2
        it "Adding message in first epoch + 3, where one message is already stored in first epoch." $
            assertEqual "Should get stored" (Just messages5'') $
                updateTimeoutMessages (Present messages1) $
                    dummyTimeoutMessage 3 3
        it "Adding message in first epoch + 3, where messages are already stored in both epochs." $
            assertEqual "Should get stored" (Just messages5'') $
                updateTimeoutMessages (Present messages3) $
                    dummyTimeoutMessage 3 3
        it "Adding message in first epoch - 1, where one message is already stored in first epoch." $
            assertEqual "Should get stored" (Just messages6) $
                updateTimeoutMessages (Present messages5) $
                    dummyTimeoutMessage 0 1
        it "Adding message in first epoch - 1, where messages are already stored in both epochs." $
            assertEqual "Should not get stored" Nothing $
                updateTimeoutMessages (Present messages5') $
                    dummyTimeoutMessage 0 0
        it "Adding message in first epoch - 2, where messages are already stored in both epochs." $
            assertEqual "Should not get stored" Nothing $
                updateTimeoutMessages (Present messages5) $
                    dummyTimeoutMessage 0 0
  where
    messages6 =
        TimeoutMessages
            { tmSecondEpochTimeouts = Map.fromList [(FinalizerIndex 1, dummyTimeoutMessage 1 2)],
              tmFirstEpochTimeouts = Map.fromList [(FinalizerIndex 0, dummyTimeoutMessage 0 1)],
              tmFirstEpoch = 1
            }
    messages5'' =
        TimeoutMessages
            { tmSecondEpochTimeouts = Map.empty,
              tmFirstEpochTimeouts = Map.fromList [(FinalizerIndex 3, dummyTimeoutMessage 3 3)],
              tmFirstEpoch = 3
            }
    messages5' =
        TimeoutMessages
            { tmSecondEpochTimeouts = Map.fromList [(FinalizerIndex 2, dummyTimeoutMessage 2 2)],
              tmFirstEpochTimeouts = Map.fromList [(FinalizerIndex 1, dummyTimeoutMessage 1 1)],
              tmFirstEpoch = 1
            }
    messages5 =
        TimeoutMessages
            { tmSecondEpochTimeouts = Map.empty,
              tmFirstEpochTimeouts = Map.fromList [(FinalizerIndex 1, dummyTimeoutMessage 1 2)],
              tmFirstEpoch = 2
            }
    messages4' =
        TimeoutMessages
            { tmSecondEpochTimeouts = Map.fromList [(FinalizerIndex 1, dummyTimeoutMessage 1 1), (FinalizerIndex 2, dummyTimeoutMessage 2 1)],
              tmFirstEpochTimeouts = Map.fromList [(FinalizerIndex 0, dummyTimeoutMessage 0 0)],
              tmFirstEpoch = 0
            }
    messages4 =
        TimeoutMessages
            { tmSecondEpochTimeouts = Map.fromList [(FinalizerIndex 1, dummyTimeoutMessage 1 1)],
              tmFirstEpochTimeouts = Map.fromList [(FinalizerIndex 0, dummyTimeoutMessage 0 0), (FinalizerIndex 2, dummyTimeoutMessage 2 0)],
              tmFirstEpoch = 0
            }
    messages3' =
        TimeoutMessages
            { tmSecondEpochTimeouts = Map.empty,
              tmFirstEpochTimeouts = Map.fromList [(FinalizerIndex 0, dummyTimeoutMessage 0 0), (FinalizerIndex 1, dummyTimeoutMessage' 1 1 0)],
              tmFirstEpoch = 0
            }
    messages3 =
        TimeoutMessages
            { tmSecondEpochTimeouts = Map.fromList [(FinalizerIndex 1, dummyTimeoutMessage 1 1)],
              tmFirstEpochTimeouts = Map.fromList [(FinalizerIndex 0, dummyTimeoutMessage 0 0)],
              tmFirstEpoch = 0
            }
    messages2 =
        TimeoutMessages
            { tmSecondEpochTimeouts = Map.empty,
              tmFirstEpochTimeouts = Map.fromList [(FinalizerIndex 0, dummyTimeoutMessage 0 0), (FinalizerIndex 1, dummyTimeoutMessage 1 0)],
              tmFirstEpoch = 0
            }
    messages1 = TimeoutMessages 0 (Map.singleton (FinalizerIndex 0) $ dummyTimeoutMessage 0 0) Map.empty

-- |Test the 'receiveTimeoutMessage' function which partially verifies
-- a 'TimeoutMessage'.
testReceiveTimeoutMessage :: Spec
testReceiveTimeoutMessage = describe "Receive timeout message" $ do
    it "rejects obsolete round" $ receiveAndCheck sd obsoleteRoundMessage $ Rejected ObsoleteRound
    it "rejects obsolete qc" $ receiveAndCheck sd obsoleteQCMessage $ Rejected ObsoleteQC
    it "initializes catch-up upon future epoch" $ receiveAndCheck sd futureEpochTM CatchupRequired
    it "rejects from a non finalizer" $ receiveAndCheck sd notAFinalizerQCMessage $ Rejected NotAFinalizer
    it "rejects on unknown finalization committee" $ receiveAndCheck sd unknownFinalizationCommittee $ Rejected ObsoleteQC
    it "rejects on an invalid signature" $ receiveAndCheck sd invalidSignatureMessage $ Rejected InvalidSignature
    it "initializes catch-up upon a future round" $ receiveAndCheck sd futureRoundTM CatchupRequired
    it "rejects when the qc points to an old finalized block" $ receiveAndCheck sd obsoleteQCPointer $ Rejected ObsoleteQCPointer
    it "initializes catch-up when the qc pointer is unknown" $ receiveAndCheck sd unknownQCPointer CatchupRequired
    it "rejects when the qc points to a dead block" $ receiveAndCheck sd qcPointerIsDead $ Rejected DeadQCPointer
    it "initializes catch-up when qc pointer is pending" $ receiveAndCheck sd qcPointerIsPending CatchupRequired
    it "returns duplicate upon a duplicate timeout message" $ receiveAndCheck sd duplicateMessage $ Rejected Duplicate
    it "rejects double signing" $ receiveAndCheck sd doubleSignMessage $ Rejected DoubleSigning
    it "received a valid timeout message" $
        receiveAndCheck sd validTimeoutMessage $
            Received $
                PartiallyVerifiedTimeoutMessage validTimeoutMessage finalizers True (Present $ someBlockPointer liveBlockHash 1 0)
  where
    -- A valid timeout message that should pass the initial verification.
    -- This is sent from finalizer with index 2.
    validTimeoutMessage = mkTimeoutMessage $! mkTimeoutMessageBody 2 2 0 $ aQC liveBlockHash (Round 1) 0
    -- A message that will be rejected as double signing as the finalizer already have @duplicateMessage@
    -- in the tree state.
    doubleSignMessage = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 $ aQC anotherLiveBlock (Round 1) 0
    -- A message that is intended to return @Duplicate@, hence it is also insert into the
    -- tree state before running the test.
    duplicateMessage = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 $ aQC liveBlockHash (Round 1) 0
    -- A message where the qc pointer is pending
    qcPointerIsPending = mkTimeoutMessage $! mkTimeoutMessageBody 2 2 0 qcWithPendingPointer
    -- A message where the qc pointer is pointing to a dead block
    qcPointerIsDead = mkTimeoutMessage $! mkTimeoutMessageBody 2 2 0 qcWithDeadPointer
    -- A message where the qc pointer is unknown
    unknownQCPointer = mkTimeoutMessage $! mkTimeoutMessageBody 2 2 0 qcWithUnknownPointer
    -- A message where the qc pointer is to a block prior to the last finalized block.
    obsoleteQCPointer = mkTimeoutMessage $! mkTimeoutMessageBody 2 2 0 someQCPointingToAndOldFinalizedBlock
    -- A message where the epoch is in the future.
    futureEpochTM = mkTimeoutMessage $! mkTimeoutMessageBody 1 11 1 $ someQC (Round 10) 1
    -- A message where the round is in the future but the finalizer is present in the epoch.
    futureRoundTM = mkTimeoutMessage $! mkTimeoutMessageBody 0 5 0 $ someQC (Round 3) 0
    -- A message where the signature is invalid
    invalidSignatureMessage = TimeoutMessage (mkTimeoutMessageBody 1 2 0 (someQC (Round 1) 0)) (Sig.Signature "invalid signature")
    -- A message where the round is obsolete.
    obsoleteRoundMessage = mkTimeoutMessage $! mkTimeoutMessageBody 1 1 0 $ someQC (Round 0) 0
    -- A message where the qc is obsolete.
    obsoleteQCMessage = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 $ someQC (Round 0) 0
    -- A message with an unknown finalization committee associated.
    unknownFinalizationCommittee = mkTimeoutMessage $! mkTimeoutMessageBody 2 2 0 $ someQC (Round 1) 42
    -- A message from a non finalizer (42)
    notAFinalizerQCMessage = mkTimeoutMessage $! mkTimeoutMessageBody 42 2 0 $ someQC (Round 1) 0
    -- Some quorum certificate for the specified round and epoch
    someQC = aQC someBlockHash
    -- a quorum certificate pointing to the provided block hash.
    -- The quorum certificate is signed off by finalizer 1 and 2.
    aQC bh r e = QuorumCertificate bh r e qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- a qc pointing to a pending block
    qcWithPendingPointer = QuorumCertificate pendingBlockHash 1 0 qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- a qc pointing to a dead block
    qcWithDeadPointer = QuorumCertificate deadBlockHash 1 0 qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- a qc pointing to an unknown block
    qcWithUnknownPointer = QuorumCertificate (BlockHash $ Hash.hash "Some unknown block") 1 0 qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- A qc pointing to an old finalized block.
    someQCPointingToAndOldFinalizedBlock = QuorumCertificate someOldFinalizedBlockHash 1 0 qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- Some unsigned quorum certificate message
    someQCMessage finalizerIndex = QuorumMessage (QuorumSignature Bls.emptySignature) someBlockHash (FinalizerIndex finalizerIndex) 1 1
    -- A quorum certificate message signature for some quorum certificate message.
    someQCMessageSignature finalizerIndex = signQuorumSignatureMessage (quorumSignatureMessageFor (someQCMessage finalizerIndex) genesisBlkHash) $ blsPrivateKey finalizerIndex
    -- A private bls key shared by the finalizers in this test.
    blsPrivateKey fidx = fst $ Dummy.randomBlsSecretKey $ mkStdGen (fromIntegral fidx)
    -- A public bls key shared by the finalizers in this test.
    blsPublicKey fidx = Bls.derivePublicKey (blsPrivateKey fidx)
    -- The aggregate signature for the quorum certificate signed off by finalizer 1 and 2.
    qcSignature = someQCMessageSignature 1 <> someQCMessageSignature 1
    --- A VRF public key shared by the finalizers in this test.
    vrfPublicKey = VRF.publicKey someVRFKeyPair
    -- make a timeout message body suitable for signing.
    mkTimeoutMessageBody fidx r e qc = TimeoutMessageBody (FinalizerIndex fidx) (Round r) e qc $ validTimeoutSignature fidx r (qcRound qc) (qcEpoch qc)
    -- Create a valid timeout signature.
    validTimeoutSignature fidx r qr qe = signTimeoutSignatureMessage (TimeoutSignatureMessage genesisBlkHash (Round r) qr qe) (blsPrivateKey fidx)
    -- sign and create a timeout message.
    mkTimeoutMessage body = signTimeoutMessage body genesisBlkHash $ sigKeyPair' (fromIntegral $ theFinalizerIndex $ tmFinalizerIndex body)
    -- A block hash for a block marked as pending.
    pendingBlockHash = BlockHash $ Hash.hash "A pending block"
    -- A block hash for a block marked as dead.
    deadBlockHash = BlockHash $ Hash.hash "A dead block"
    -- Some genesis block hash.
    genesisBlkHash = BlockHash $ Hash.hash "My genesis block"
    -- Some other block hash
    someBlockHash = BlockHash $ Hash.hash "Some other block"
    -- A hash for an old finalized block.
    someOldFinalizedBlockHash = BlockHash $ Hash.hash "Some old finalized block"
    -- Some baked block for the provided round and epoch
    bakedBlock r e = BakedBlock r e 0 0 (someQC r e) Absent Absent dummyBlockNonce Vec.empty emptyTransactionOutcomesHashV1 (StateHashV0 $ Hash.hash "empty state hash")
    -- Some pending block with no meaningful content.
    -- It is inserted into the tree state before running tests.
    pendingBlock = MemBlockPending $ PendingBlock signedBlock $ timestampToUTCTime 0
    -- a block hash for a block that is alive.
    -- A block with this hash is put into the live blocks table and
    -- likewise the @duplicateMessage@ has this block hash (which is
    -- present in the @receivedTimeoutMessages@.
    liveBlockHash = BlockHash $ Hash.hash "live block"
    -- Another live block. This is just present in the live block table,
    -- but no one has already sent a timeout message for it.
    -- It is used for triggering the double signing case together with the
    -- @liveBlockHash@.
    anotherLiveBlock = BlockHash $ Hash.hash "another live block"
    -- A block that is recorded as live in the tree state
    liveBlock = MemBlockAlive $ someBlockPointer liveBlockHash 3 0
    -- A block signed by finalizer 1.
    signedBlock = SignedBlock (bakedBlock 4 0) pendingBlockHash $ Sig.sign (sigKeyPair' 1) "foo"
    -- FinalizerInfo for the finalizer index provided.
    -- All finalizers has the same keys attached.
    fi :: Word32 -> FinalizerInfo
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 (sigPublicKey' (fromIntegral fIdx)) vrfPublicKey (blsPublicKey fIdx) (BakerId $ AccountIndex $ fromIntegral fIdx)
    -- COnstruct the finalization committee
    finalizers = FinalizationCommittee (Vec.fromList [fi 0, fi 1, fi 2]) 3
    -- Construct a set of 0 bakers and 3 finalisers with indices 0,1,2.
    bakersAndFinalizers = BakersAndFinalizers (FullBakers Vec.empty 0) finalizers
    -- A tree state where the following applies:
    -- - Current round is 2
    -- - Current epoch is 0
    -- - There is a finalization committee consisting of the finalizers with indices [0,1,2]
    -- - There is a last finalized block for round 1, epoch 0.
    sd =
        dummyInitialSkovData
            & roundStatus . rsCurrentRound .~ Round 2
            & roundStatus . rsCurrentEpoch .~ 0
            & genesisMetadata %~ (\existingGenesis -> existingGenesis{gmCurrentGenesisHash = genesisBlkHash, gmFirstGenesisHash = genesisBlkHash})
            & lastFinalized .~ myBlockPointer (Round 1) 0
            & skovEpochBakers . currentEpochBakers .~ bakersAndFinalizers
            & skovEpochBakers . previousEpochBakers .~ bakersAndFinalizers
            & blockTable . deadBlocks %~ insertDeadCache deadBlockHash
            & blockTable
                . liveMap
                %~ HM.insert pendingBlockHash pendingBlock
                . HM.insert liveBlockHash liveBlock
                . HM.insert anotherLiveBlock liveBlock
            & currentTimeoutMessages .~ Present (TimeoutMessages 0 (Map.singleton (FinalizerIndex 1) duplicateMessage) Map.empty)
    -- A low level database which consists of a finalized block for height 0 otherwise empty.
    lldb =
        let myLLDB = lldbWithGenesis @'P6
        in  myLLDB{lldbBlocks = HM.singleton someOldFinalizedBlockHash $ toStoredBlock (dummyBlock 20)}
    -- receive the timeout message in the provided tree state context and
    -- check that the result is as expected.
    receiveAndCheck skovData tm expect = do
        resultCode <- runTestLLDB lldb $ receiveTimeoutMessage tm skovData
        resultCode `shouldBe` expect

-- |Tests for executing timeout messages.
-- The @executeTimeoutMessage@ executes a 'TimeoutMessage' which is partially verified
-- by @receiveTimeoutMessage@.
testExecuteTimeoutMessages :: Spec
testExecuteTimeoutMessages = describe "execute timeout messages" $ do
    it "rejects message with invalid bls signature" $ execute invalidAggregateSignature InvalidAggregateSignature
    it "accepts message where there is already checked a valid qc for the round" $ execute validMessageAbsentQCPointer ExecutionSuccess
    it "rejects message with invalid qc signature (qc round is better than recorded highest qc)" $ execute invalidQCTimeoutMessage $ InvalidQC $ someInvalidQC 2 0
    it "accepts message where qc is ok (qc round is better than recorded highest qc)" $ execute newValidQCTimeoutMessage ExecutionSuccess
    it "rejects message with qc round no greater than highest qc and invalic qc" $ execute wrongEpochMessage $ InvalidQC $ someInvalidQC 0 0
    it "accepts message with qc round no greather than highest qc and valid qc" $ execute oldValidQCTimeoutMessage ExecutionSuccess
    it "accepts message with qc already checked for that round and qc checks out (qc round <= higest qc)" $ execute oldRoundValidTimeoutMessage ExecutionSuccess
  where
    -- action that runs @executeTimeoutMessage@ on the provided
    -- timeout message and checks that it matches the expectation.
    --
    -- In particular the context we're executing the action within
    -- - current round is 2
    -- - highest QC seen is round 1 and epoch 0
    -- - a qc for round 1 epoch 0 has been checked (witnessed)
    execute timeoutMessage expect = runTestMonad @'P6 noBaker time myGenesisData $ do
        -- Set the highest qc to round 1 epoch 0.
        roundStatus . rsHighestCertifiedBlock
            .= CertifiedBlock
                { cbQuorumCertificate = someQC (Round 1) 0,
                  cbQuorumBlock = someBlockPointer (BlockHash $ Hash.hash "quorum block hash") 0 1
                }
        -- Set the current round to round 2.
        roundStatus . rsCurrentRound .= Round 2
        -- Insert the witness for an already received qc for round 2 with epoch 0
        roundExistingQCs %= Map.insert (Round 1) (QuorumCertificateCheckedWitness 0)
        -- Execute the timeout message
        resultCode <- executeTimeoutMessage timeoutMessage
        -- Check that the result matches the expected one.
        liftIO $ expect @=? resultCode
    -- the finalizer with the provided finalizer index.
    -- Note that all finalizers use the same keys.
    fi :: Word32 -> FinalizerInfo
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 sigPublicKey (VRF.publicKey someVRFKeyPair) (Bls.derivePublicKey $ blsSk fIdx) (BakerId $ AccountIndex $ fromIntegral fIdx)
    -- a qc has already been checked for the round and the @pvtmBlock@ is absent
    validMessageAbsentQCPointer = PartiallyVerifiedTimeoutMessage (validTimeoutMessage 1 1 0) finalizers True Absent
    -- The bls signature will be rejected.
    invalidAggregateSignature = PartiallyVerifiedTimeoutMessage (validTimeoutMessage 1 1 0) finalizers False Absent
    -- round is already checked
    oldRoundValidTimeoutMessage = PartiallyVerifiedTimeoutMessage (validTimeoutMessage 1 1 0) finalizers True Absent
    -- a valid timeout message pointing to an "old qc" (round 1 epoch 0).
    oldValidQCTimeoutMessage = PartiallyVerifiedTimeoutMessage (validTimeoutMessage 3 0 0) finalizers True Absent
    -- a new valid timeout message for round 3 with a valid qc for round 2 (epoch 0).
    newValidQCTimeoutMessage = PartiallyVerifiedTimeoutMessage (validTimeoutMessage 3 2 0) finalizers True Absent
    -- a timeout message where the qc signature does not check out.
    invalidQCTimeoutMessage =
        PartiallyVerifiedTimeoutMessage
            { pvtmTimeoutMessage = mkTimeoutMessage $! mkTimeoutMessageBody 2 2 0 (someInvalidQC 2 0),
              pvtmQuorumFinalizers = finalizers,
              pvtmAggregateSignatureValid = True,
              pvtmBlock = Present $ myBlockPointer 1 0
            }
    -- wrong epoch
    wrongEpochMessage =
        PartiallyVerifiedTimeoutMessage
            { pvtmTimeoutMessage = mkTimeoutMessage $! mkTimeoutMessageBody 2 0 0 (someInvalidQC 0 0),
              pvtmQuorumFinalizers = finalizers,
              pvtmAggregateSignatureValid = True,
              pvtmBlock = Present $ myBlockPointer 1 0
            }
    -- the finalization committee.
    finalizers = FinalizationCommittee (Vec.fromList [fi 0, fi 1, fi 2]) 3
    -- the time that we run our test computation with respect to.
    time = timestampToUTCTime 1
    -- The @executeTimeoutMessage@ runs in a no baker context.
    noBaker = BakerContext Nothing
    -- the genesis block hash
    genesisBlkHash = GD.genesisBlockHash myGenesisData
    -- a timeout message body by the finalizer, round epoch and qc provided.
    mkTimeoutMessageBody fidx r e qc = TimeoutMessageBody (FinalizerIndex fidx) (Round r) e qc $ validTimeoutSignature fidx r (qcRound qc) (qcEpoch qc)
    -- a valid timeout signature.
    validTimeoutSignature fidx r qr qe = signTimeoutSignatureMessage (TimeoutSignatureMessage genesisBlkHash (Round r) qr qe) $ blsSk fidx
    -- A bls key for the finalizer.
    blsSk fidx = fst <$> Dummy.randomBlsSecretKey $ mkStdGen $ fromIntegral fidx
    -- a valid timeout message for the given round and qc round. Epoch 0 is used.
    validTimeoutMessage r qcr e = mkTimeoutMessage $! mkTimeoutMessageBody 0 r 0 $ someQC (Round qcr) e
    -- some valid qc message by the provided finalizer.
    someQCMessage finalizerIndex r = QuorumMessage (QuorumSignature Bls.emptySignature) someBlockHash (FinalizerIndex finalizerIndex) r 0
    -- some valid qc message signature signed by the provide
    someQCMessageSignature finalizerIndex r = signQuorumSignatureMessage (quorumSignatureMessageFor (someQCMessage finalizerIndex r) genesisBlkHash) (blsSk finalizerIndex)
    -- just a block hash for testing purposes.
    someBlockHash = BlockHash $ Hash.hash "a block hash"
    -- a valid signature.
    qcSignature = someQCMessageSignature 0 <> someQCMessageSignature 1 <> someQCMessageSignature 2
    -- a valid qc for round 0 i.e. the genesis quorum certificate.
    someQC 0 _ = genesisQuorumCertificate genesisBlkHash
    -- a qc with a valid signature created by finalizers 0,1,2
    someQC r e = QuorumCertificate someBlockHash r e (qcSignature r) $ finalizerSet [FinalizerIndex 0, FinalizerIndex 1, FinalizerIndex 2]
    -- a qc with the empty signature.
    someInvalidQC r e = QuorumCertificate someBlockHash r e (QuorumSignature Bls.emptySignature) $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2, FinalizerIndex 3]
    -- a signed timeout message with the provided body.
    mkTimeoutMessage body = signTimeoutMessage body genesisBlkHash sigKeyPair
    -- the foundation account for the genesis data.
    foundationAcct =
        Dummy.createCustomAccount
            1_000_000_000_000
            (Dummy.deterministicKP 0)
            (Dummy.accountAddressFrom 0)
    -- the genesis data for composing the monad that the computations are run within.
    -- it consists of 3 finalizers with indices 0,1,2
    (myGenesisData, _, _) =
        makeGenesisDataV1 @'P6
            (Timestamp 0)
            3
            3_600_000
            Dummy.dummyCryptographicParameters
            Dummy.dummyIdentityProviders
            Dummy.dummyArs
            [ foundationAcct
            ]
            Dummy.dummyKeyCollection
            Dummy.dummyChainParameters

-- |Tests the 'checkTimeoutCertificate' function.
testCheckTimeoutCertificate :: Spec
testCheckTimeoutCertificate = describe "check timeout certificate" $ do
    it "accepts timeout certificate" checkOkTC
    it "rejects with wrong genesis" wrongGenesis
    it "rejects when there is not enough weight" insufficientWeight
    it "rejects when the signature is invalid" invalidSignature
  where
    checkOkTC = runTest $ do
        finComm <- use $ skovEpochBakers . currentEpochBakers . bfFinalizers
        qc <- use $ roundStatus . rsHighestCertifiedBlock . to cbQuorumCertificate
        checkOk $ checkTimeoutCertificate okGenesisHash sigThreshold finComm finComm finComm $ validTCFor qc
    wrongGenesis = runTest $ do
        finComm <- use $ skovEpochBakers . currentEpochBakers . bfFinalizers
        qc <- use $ roundStatus . rsHighestCertifiedBlock . to cbQuorumCertificate
        checkNotOk $ checkTimeoutCertificate invalidGenesisHash sigThreshold finComm finComm finComm $ validTCFor qc
    insufficientWeight = runTest $ do
        finComm <- use $ skovEpochBakers . currentEpochBakers . bfFinalizers
        qc <- use $ roundStatus . rsHighestCertifiedBlock . to cbQuorumCertificate
        let finComm' = finComm{committeeFinalizers = Vec.tail $ committeeFinalizers finComm}
        checkNotOk $ checkTimeoutCertificate okGenesisHash sigThreshold finComm finComm finComm' $ validTCFor qc
    invalidSignature = runTest $ do
        finComm <- use $ skovEpochBakers . currentEpochBakers . bfFinalizers
        qc <- use $ roundStatus . rsHighestCertifiedBlock . to cbQuorumCertificate
        checkNotOk $ checkTimeoutCertificate okGenesisHash sigThreshold finComm finComm finComm $ invalidSignatureInTC qc
    checkOk = liftIO . assertBool "Check failed"
    checkNotOk b = liftIO . assertBool "Check failed" $ not b
    runTest = runTestMonad (BakerContext Nothing) (timestampToUTCTime 1_000) TestBlocks.genesisData
    okGenesisHash = TestBlocks.genesisHash
    invalidGenesisHash = genesisHash
    validTCFor qc = TestBlocks.validTimeoutFor qc (Round 1)
    invalidSignatureInTC qc =
        let tc = validTCFor qc
        in  tc{tcAggregateSignature = TimeoutSignature Bls.emptySignature}

tests :: Spec
tests = describe "KonsensusV1.Timeout" $ do
    testReceiveTimeoutMessage
    testExecuteTimeoutMessages
    it "Test updateCurrentTimeout" $ do
        testUpdateCurrentTimeout 10_000 (3 % 2) 15_000
        testUpdateCurrentTimeout 10_000 (4 % 3) 13_333
        testUpdateCurrentTimeout 10_000 (5 % 3) 16_666
        testUpdateCurrentTimeout 3_000 (4 % 3) 4_000
        testUpdateCurrentTimeout 80_000 (10 % 9) 88_888
        testUpdateCurrentTimeout 8_000 (8 % 7) 9_142
    it "Test processTimeout" testProcessTimeout
    it "Test uponTimeoutEvent" testUponTimeoutEvent
    testUpdateTimeoutMessages
    testCheckTimeoutCertificate
