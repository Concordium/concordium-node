{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |A module that tests the in-band catch-up mechanism of KonsensusV1.
module ConcordiumTests.KonsensusV1.CatchUp where

import Control.Monad.State.Strict

import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import Concordium.Types
import Concordium.Types.BakerIdentity
import Concordium.Types.HashableTo
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec

import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.CatchUp
import qualified Concordium.KonsensusV1.Consensus.Quorum as Quorum
import qualified Concordium.KonsensusV1.Consensus.Timeout as Timeout
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import ConcordiumTests.KonsensusV1.Consensus.Blocks

import qualified ConcordiumTests.KonsensusV1.Consensus.Blocks as TestBlocks

-- |Checking that the @CatchupPartialResponse m@ is as expected.
assertCatchupResponse ::
    MonadIO m =>
    -- |The expected terminal data.
    CatchUpTerminalData ->
    -- |The expected blocks to be served.
    [SignedBlock] ->
    -- |The response.
    CatchUpPartialResponse m ->
    m ()
assertCatchupResponse term [] resp = case resp of
    CatchUpPartialResponseDone actualTerm -> liftIO $ assertEqual "Unexpected terminal data" term actualTerm
    CatchUpPartialResponseBlock{..} ->
        cuprFinish >>= \case
            Absent -> liftIO $ assertFailure "Expected a terminal data as all expected blocks has been served."
            Present actualTerm -> liftIO $ assertEqual "Unexpected terminal data" term actualTerm
assertCatchupResponse term (x : xs) resp = case resp of
    CatchUpPartialResponseDone{} -> liftIO $ assertFailure "Unexpected done result. There is still expected blocks to be served."
    CatchUpPartialResponseBlock{..} -> do
        liftIO $ assertEqual "Unexpected block served" x cuprNextBlock
        assertCatchupResponse term xs =<< cuprContinue

-- |Receive and execute blocks served as part of the @CatchUpPartialResponse@.
-- Return the terminal data when available.
consumeResponse :: (MonadIO m, m ~ TestMonad 'P6) => CatchUpPartialResponse m -> TestMonad 'P6 CatchUpTerminalData
consumeResponse CatchUpPartialResponseBlock{..} = do
    succeedReceiveBlock
        PendingBlock
            { pbBlock = cuprNextBlock,
              pbReceiveTime = timestampToUTCTime $ blockTimestamp cuprNextBlock
            }
    cuprFinish >>= \case
        Absent -> consumeResponse =<< cuprContinue
        Present termData -> return termData
consumeResponse CatchUpPartialResponseDone{..} = return cuprFinished

-- |Receive and execute a timeout message.
succeedReceiveExecuteTimeoutMessage :: TimeoutMessage -> TestMonad 'P6 ()
succeedReceiveExecuteTimeoutMessage tm = do
    recvResult <- Timeout.receiveTimeoutMessage tm =<< get
    case recvResult of
        Timeout.Received verifiedTm ->
            Timeout.executeTimeoutMessage verifiedTm >>= \case
                Timeout.ExecutionSuccess -> return ()
                execStatus -> liftIO . assertFailure $ "Expected timeout message was executed, but was " <> show execStatus
        recvStatus -> liftIO . assertFailure $ "Expected timeout message was received, but was " <> show recvStatus

-- |Receive and process a quorum message.
succeedReceiveProcessQuorumMessage :: QuorumMessage -> TestMonad 'P6 ()
succeedReceiveProcessQuorumMessage qm = do
    recvResult <- Quorum.receiveQuorumMessage qm =<< get
    case recvResult of
        Quorum.Received verifiedQM -> Quorum.processQuorumMessage verifiedQM $ return () -- There is no need to make a block.
        recvStatus -> liftIO . assertFailure $ "Expected quorum message was reveived, but was " <> show recvStatus

-- |Create a valid timeout message with the provided data.
testTimeoutMessage :: Int -> Round -> QuorumCertificate -> TimeoutMessage
testTimeoutMessage finIndex rnd qc = signTimeoutMessage body TestBlocks.genesisHash (bakerSignKey . fst $ TestBlocks.bakers !! finIndex)
  where
    body =
        TimeoutMessageBody
            { tmFinalizerIndex = FinalizerIndex $ fromIntegral finIndex,
              tmRound = rnd,
              tmEpoch = qcEpoch qc,
              tmQuorumCertificate = qc,
              tmAggregateSignature = timeoutSig
            }
    timeoutSig = signTimeoutSignatureMessage timeoutSigMessage (bakerAggregationKey . fst $ TestBlocks.bakers !! finIndex)
    timeoutSigMessage =
        TimeoutSignatureMessage
            { tsmGenesis = TestBlocks.genesisHash,
              tsmRound = rnd,
              tsmQCRound = qcRound qc,
              tsmQCEpoch = qcEpoch qc
            }

-- |Create a valid quorum message with the provided data.
testQuorumMessage :: Int -> Round -> Epoch -> BlockHash -> QuorumMessage
testQuorumMessage finIndex rnd e ptr =
    let qsm =
            QuorumSignatureMessage
                { qsmGenesis = genesisHash,
                  qsmBlock = ptr,
                  qsmRound = rnd,
                  qsmEpoch = e
                }
        qsig = signQuorumSignatureMessage qsm (bakerAggregationKey . fst $ bakers !! finIndex)
    in  buildQuorumMessage qsm qsig (FinalizerIndex $ fromIntegral finIndex)

-- |Run a TestMonad pv a action with a no-baker context,
-- fixed 'genesisData' and fixed time.
runTest :: TestMonad 'P6 a -> IO a
runTest = runTestMonad @'P6 (BakerContext Nothing) (timestampToUTCTime 1_000) TestBlocks.genesisData

-- |Testing a basic test scenario where the node N knows
-- of the block in round 1 and 2 and catches up the third block and a tm + qm for round 3.
basicCatchupResponse :: Assertion
basicCatchupResponse = runTest $ do
    let b1 = TestBlocks.signedPB TestBlocks.testBB1
    TestBlocks.succeedReceiveBlock b1
    let b2 = TestBlocks.signedPB TestBlocks.testBB2
    TestBlocks.succeedReceiveBlock b2
    let b3 = TestBlocks.signedPB TestBlocks.testBB3
    TestBlocks.succeedReceiveBlock b3
    -- b2 has a qc for b1, b3 has a qc for b2 and
    -- b1 and b2 is in consecutive rounds so b1 is finalized.
    let b3QC = bbQuorumCertificate TestBlocks.testBB3
        qsmR4 = QuorumSignatureMessage TestBlocks.genesisHash (getHash b3) (Round 3) 0
        qmR4Sig = signQuorumSignatureMessage qsmR4 (bakerAggregationKey . fst $ TestBlocks.bakers !! 1)
        qmR4 = buildQuorumMessage qsmR4 qmR4Sig (FinalizerIndex 1)
        tmR4 = head $ timeoutMessagesFor b3QC (Round 3) 0
        request =
            CatchUpStatus
                { cusLastFinalizedBlock = getHash b1,
                  cusLastFinalizedRound = Round 1,
                  cusLeaves = [getHash b2],
                  cusBranches = [],
                  cusCurrentRound = Round 3,
                  cusCurrentRoundQuorum = Map.empty,
                  cusCurrentRoundTimeouts = Absent
                }
        expectedTerminalData =
            CatchUpTerminalData
                { cutdQuorumCertificates = [b3QC],
                  cutdTimeoutCertificate = Absent,
                  cutdCurrentRoundQuorumMessages = [qmR4],
                  cutdCurrentRoundTimeoutMessages = [tmR4]
                }
        expectedBlocksServed = pbBlock <$> [b3]
        finToQMsgMap = Map.insert (FinalizerIndex 1) qmR4 Map.empty
        finToTMMap = Map.insert (FinalizerIndex 0) tmR4 Map.empty
    -- Setting the current quorum and timeout message.
    currentQuorumMessages .= QuorumMessages finToQMsgMap Map.empty
    currentTimeoutMessages .= Present (TimeoutMessages 1 finToTMMap Map.empty)
    assertCatchupResponse expectedTerminalData expectedBlocksServed =<< handleCatchUpRequest request =<< get

-- |A test where the response covers an epoch transition.
catchupWithEpochTransitionResponse :: Assertion
catchupWithEpochTransitionResponse = runTest $ do
    let b1 = TestBlocks.signedPB TestBlocks.testBB1E
    TestBlocks.succeedReceiveBlock b1
    let b2 = TestBlocks.signedPB TestBlocks.testBB2E
    TestBlocks.succeedReceiveBlock b2
    let b3 = TestBlocks.signedPB TestBlocks.testBB3E
    TestBlocks.succeedReceiveBlock b3
    -- b3 contains an epoch finalization entry and is in epoch 1.
    let b3QC = bbQuorumCertificate TestBlocks.testBB3E
        qsmR4 = QuorumSignatureMessage TestBlocks.genesisHash (getHash b3) (Round 4) 0
        qmR4Sig = signQuorumSignatureMessage qsmR4 (bakerAggregationKey . fst $ TestBlocks.bakers !! 1)
        qmR4 = buildQuorumMessage qsmR4 qmR4Sig (FinalizerIndex 1)
        tmR4 = head $ timeoutMessagesFor b3QC (Round 4) 0
        request =
            CatchUpStatus
                { cusLastFinalizedBlock = getHash b1,
                  cusLastFinalizedRound = Round 1,
                  cusLeaves = [],
                  cusBranches = [],
                  cusCurrentRound = Round 3,
                  cusCurrentRoundQuorum = Map.empty,
                  cusCurrentRoundTimeouts = Absent
                }
        expectedTerminalData =
            CatchUpTerminalData
                { cutdQuorumCertificates = [b3QC],
                  cutdTimeoutCertificate = Absent,
                  cutdCurrentRoundQuorumMessages = [qmR4],
                  cutdCurrentRoundTimeoutMessages = [tmR4]
                }
        expectedBlocksServed = pbBlock <$> [b2, b3]
        finToQMsgMap = Map.insert (FinalizerIndex 1) qmR4 Map.empty
        finToTMMap = Map.insert (FinalizerIndex 0) tmR4 Map.empty
    -- Setting the current quorum and timeout message.
    currentQuorumMessages .= QuorumMessages finToQMsgMap Map.empty
    currentTimeoutMessages .= Present (TimeoutMessages 1 finToTMMap Map.empty)
    assertCatchupResponse expectedTerminalData expectedBlocksServed =<< handleCatchUpRequest request =<< get

-- |A response where there is a timeout in round 4,
-- a quorum message for round 5 and a timeout message for round 5.
catchupWithTimeoutsResponse :: Assertion
catchupWithTimeoutsResponse = runTest $ do
    let b1 = TestBlocks.signedPB TestBlocks.testBB1E
    TestBlocks.succeedReceiveBlock b1
    let b2 = TestBlocks.signedPB TestBlocks.testBB2E
    TestBlocks.succeedReceiveBlock b2
    -- b3 contains an epoch finalization entry and is in epoch 1.
    let b3 = TestBlocks.signedPB TestBlocks.testBB3E
    TestBlocks.succeedReceiveBlock b3
    -- round 4 timeouts
    let b5 = TestBlocks.signedPB TestBlocks.testBB5E'
    TestBlocks.succeedReceiveBlock b5
    let b3QC = bbQuorumCertificate TestBlocks.testBB3E
        qsmR5 = QuorumSignatureMessage TestBlocks.genesisHash (getHash b3) (Round 5) 0
        qmR5Sig = signQuorumSignatureMessage qsmR5 (bakerAggregationKey . fst $ TestBlocks.bakers !! 1)
        qmR5 = buildQuorumMessage qsmR5 qmR5Sig (FinalizerIndex 1)
        tmR5 = head $ timeoutMessagesFor b3QC (Round 4) 0
        request =
            CatchUpStatus
                { cusLastFinalizedBlock = getHash b1,
                  cusLastFinalizedRound = Round 1,
                  cusLeaves = [],
                  cusBranches = [],
                  cusCurrentRound = Round 3,
                  cusCurrentRoundQuorum = Map.empty,
                  cusCurrentRoundTimeouts = Absent
                }
        expectedTerminalData =
            CatchUpTerminalData
                { cutdQuorumCertificates = [blockQuorumCertificate $ pbBlock b5],
                  cutdTimeoutCertificate = blockTimeoutCertificate $ pbBlock b5,
                  cutdCurrentRoundQuorumMessages = [qmR5],
                  cutdCurrentRoundTimeoutMessages = [tmR5]
                }
        expectedBlocksServed = pbBlock <$> [b2, b3, b5]
        finToQMsgMap = Map.insert (FinalizerIndex 1) qmR5 Map.empty
        finToTMMap = Map.insert (FinalizerIndex 0) tmR5 Map.empty
    -- Setting the current quorum and timeout message.
    currentQuorumMessages .= QuorumMessages finToQMsgMap Map.empty
    currentTimeoutMessages .= Present (TimeoutMessages 1 finToTMMap Map.empty)
    assertCatchupResponse expectedTerminalData expectedBlocksServed =<< handleCatchUpRequest request =<< get

-- |There is a TC generated for the last round (round 3).
catchupWithOneTimeoutAtEndResponse :: Assertion
catchupWithOneTimeoutAtEndResponse = runTest $ do
    let b1 = TestBlocks.signedPB TestBlocks.testBB1
    TestBlocks.succeedReceiveBlock b1
    let b2 = TestBlocks.signedPB TestBlocks.testBB2
    TestBlocks.succeedReceiveBlock b2
    let b3 = TestBlocks.signedPB TestBlocks.testBB3
    TestBlocks.succeedReceiveBlock b3
    -- Generate a TC for round 3.
    let tm0_3 = testTimeoutMessage 0 (Round 3) $ blockQuorumCertificate $ pbBlock b3
        tm1_3 = testTimeoutMessage 1 (Round 3) $ blockQuorumCertificate $ pbBlock b3
        tm2_3 = testTimeoutMessage 2 (Round 3) $ blockQuorumCertificate $ pbBlock b3
        tm3_3 = testTimeoutMessage 3 (Round 3) $ blockQuorumCertificate $ pbBlock b3
    succeedReceiveExecuteTimeoutMessage tm0_3
    succeedReceiveExecuteTimeoutMessage tm1_3
    succeedReceiveExecuteTimeoutMessage tm2_3
    succeedReceiveExecuteTimeoutMessage tm3_3
    -- b2 has a qc for b1, b3 has a qc for b2 and
    -- b1 and b2 is in consecutive rounds so b1 is finalized.
    let request =
            CatchUpStatus
                { cusLastFinalizedBlock = getHash b1,
                  cusLastFinalizedRound = Round 1,
                  cusLeaves = [],
                  cusBranches = [],
                  cusCurrentRound = Round 3,
                  cusCurrentRoundQuorum = Map.empty,
                  cusCurrentRoundTimeouts = Absent
                }
        expectedBlocksServed = pbBlock <$> [b2, b3]
    expectedTerminalData <- do
        sd <- get
        case sd ^. roundStatus . rsPreviousRoundTimeout of
            Absent -> liftIO . assertFailure $ "Expected timeout messages, but they were absent."
            Present (RoundTimeout tc _) ->
                return $
                    CatchUpTerminalData
                        { cutdQuorumCertificates = [blockQuorumCertificate $ pbBlock b3],
                          cutdTimeoutCertificate = Present tc,
                          cutdCurrentRoundQuorumMessages = [],
                          cutdCurrentRoundTimeoutMessages = []
                        }
    assertCatchupResponse expectedTerminalData expectedBlocksServed =<< handleCatchUpRequest request =<< get

-- |There is a timeout for round 3 and round 4.
catchupWithTwoTimeoutsAtEndResponse :: Assertion
catchupWithTwoTimeoutsAtEndResponse = runTest $ do
    let b1 = TestBlocks.signedPB TestBlocks.testBB1
    TestBlocks.succeedReceiveBlock b1
    let b2 = TestBlocks.signedPB TestBlocks.testBB2
    TestBlocks.succeedReceiveBlock b2
    -- b3 does not get a qc as the round times out.
    let b3 = TestBlocks.signedPB TestBlocks.testBB3
    TestBlocks.succeedReceiveBlock b3
    -- b2 has a qc for b1, b3 has a qc for b2 and
    -- b1 and b2 is in consecutive rounds so b1 is finalized.
    -- Generate a TC for round 3.
    let b2QC = blockQuorumCertificate $ pbBlock b3 -- qc for b2
        tm0_3 = testTimeoutMessage 0 (Round 3) b2QC
        tm1_3 = testTimeoutMessage 1 (Round 3) b2QC
        tm2_3 = testTimeoutMessage 2 (Round 3) b2QC
        tm3_3 = testTimeoutMessage 3 (Round 3) b2QC
    succeedReceiveExecuteTimeoutMessage tm0_3
    succeedReceiveExecuteTimeoutMessage tm1_3
    succeedReceiveExecuteTimeoutMessage tm2_3
    succeedReceiveExecuteTimeoutMessage tm3_3
    -- Generate a TC for round 4.
    let tm0_4 = testTimeoutMessage 0 (Round 4) b2QC
        tm1_4 = testTimeoutMessage 1 (Round 4) b2QC
        tm2_4 = testTimeoutMessage 2 (Round 4) b2QC
        tm3_4 = testTimeoutMessage 3 (Round 4) b2QC
    succeedReceiveExecuteTimeoutMessage tm0_4
    succeedReceiveExecuteTimeoutMessage tm1_4
    succeedReceiveExecuteTimeoutMessage tm2_4
    succeedReceiveExecuteTimeoutMessage tm3_4
    -- b2 has a qc for b1, b3 has a qc for b2 and
    -- b1 and b2 is in consecutive rounds so b1 is finalized.
    let request =
            CatchUpStatus
                { cusLastFinalizedBlock = getHash b1,
                  cusLastFinalizedRound = Round 1,
                  cusLeaves = [],
                  cusBranches = [],
                  cusCurrentRound = Round 3,
                  cusCurrentRoundQuorum = Map.empty,
                  cusCurrentRoundTimeouts = Absent
                }
        expectedBlocksServed = pbBlock <$> [b2, b3]
    expectedTerminalData <- do
        sd <- get
        case sd ^. roundStatus . rsPreviousRoundTimeout of
            Absent -> liftIO . assertFailure $ "Expected timeout messages, but they were absent."
            -- todo: perhaps spell this tc out.
            Present (RoundTimeout tc _) ->
                return $
                    CatchUpTerminalData
                        { cutdQuorumCertificates = [blockQuorumCertificate $ pbBlock b3],
                          cutdTimeoutCertificate = Present tc,
                          cutdCurrentRoundQuorumMessages = [],
                          cutdCurrentRoundTimeoutMessages = []
                        }
    assertCatchupResponse expectedTerminalData expectedBlocksServed =<< handleCatchUpRequest request =<< get

catchupWithTwoBranchesResponse :: Assertion
catchupWithTwoBranchesResponse = runTest $ do
    let b1 = TestBlocks.signedPB TestBlocks.testBB1
    TestBlocks.succeedReceiveBlock b1
    let b2 = TestBlocks.signedPB TestBlocks.testBB2
    TestBlocks.succeedReceiveBlock b2
    let b3 = TestBlocks.signedPB TestBlocks.testBB3
    TestBlocks.succeedReceiveBlock b3
    -- block 1 is finalized as b2 has a qc for b1 and b3 has a qc for b2.

    -- Now we time out round 3 with a reference to b1.
    let b2QC = blockQuorumCertificate $ pbBlock b2 -- qc for b1
        tm0_3 = testTimeoutMessage 0 (Round 3) b2QC
        tm1_3 = testTimeoutMessage 1 (Round 3) b2QC
        tm2_3 = testTimeoutMessage 2 (Round 3) b2QC
        tm3_3 = testTimeoutMessage 3 (Round 3) b2QC
    succeedReceiveExecuteTimeoutMessage tm0_3
    succeedReceiveExecuteTimeoutMessage tm1_3
    succeedReceiveExecuteTimeoutMessage tm2_3
    succeedReceiveExecuteTimeoutMessage tm3_3

    -- we grab the timeout certificate for round 3 in the round status
    -- so we can create a b4 with this tc.
    -- todo: maybe just spell out this tc.
    r3rt <- use $ roundStatus . rsPreviousRoundTimeout
    b4 <- case r3rt of
        Absent -> liftIO . assertFailure $ "There should be a previous round timeout"
        Present rt ->
            return $
                TestBlocks.signedPB
                    BakedBlock
                        { bbRound = 4,
                          bbEpoch = 0,
                          bbTimestamp = 3_000,
                          bbBaker = 3,
                          bbQuorumCertificate = validQCFor testBB2,
                          bbTimeoutCertificate = Present $ rtTimeoutCertificate rt,
                          bbEpochFinalizationEntry = Absent,
                          bbNonce = computeBlockNonce genesisLEN 4 (TestBlocks.bakerVRFKey (3 :: Int)),
                          bbTransactions = Vec.empty,
                          bbTransactionOutcomesHash = emptyBlockTOH 3,
                          bbStateHash = read "3c7aaf3f231da62001d6b6dc651da0af08d72eca5d7b3a8970ee1b33367874f6"
                        }
    TestBlocks.succeedReceiveBlock b4
    -- There is one current timeout message and one current quorum message
    let tm0_4 = testTimeoutMessage 0 (Round 4) b2QC
        qm1_4 = testQuorumMessage 1 (Round 4) 0 (getHash b4)
    succeedReceiveExecuteTimeoutMessage tm0_4
    succeedReceiveProcessQuorumMessage qm1_4
    let request =
            CatchUpStatus
                { cusLastFinalizedBlock = getHash b1,
                  cusLastFinalizedRound = Round 1,
                  cusLeaves = [],
                  cusBranches = [],
                  cusCurrentRound = Round 3,
                  cusCurrentRoundQuorum = Map.empty,
                  cusCurrentRoundTimeouts = Absent
                }
        -- The expected blocks. (b3 timed out)
        expectedBlocksServed = pbBlock <$> [b2, b4]
    expectedTerminalData <- do
        sd <- get
        case sd ^. roundStatus . rsPreviousRoundTimeout of
            Absent -> liftIO . assertFailure $ "Expected timeout messages, but they were absent."
            -- todo: perhaps spell this tc out.
            Present (RoundTimeout tc _) ->
                return $
                    CatchUpTerminalData
                        { cutdQuorumCertificates = [blockQuorumCertificate $ pbBlock b3],
                          cutdTimeoutCertificate = Present tc,
                          cutdCurrentRoundQuorumMessages = [qm1_4],
                          cutdCurrentRoundTimeoutMessages = [tm0_4]
                        }
    assertCatchupResponse expectedTerminalData expectedBlocksServed =<< handleCatchUpRequest request =<< get

-- |Checking that the 'CatchUpStatus' is correctly generated from a state where:
-- there are 3 blocks for round 1,2 and 3, where the first block is finalized.
-- The block in round 3 never gets a 'QuorumCertificate' and hence round 3 times out.
-- There is a block 4 for round 4
testMakeCatchupStatus :: Assertion
testMakeCatchupStatus = runTest $ do
    let b1 = TestBlocks.signedPB TestBlocks.testBB1
    TestBlocks.succeedReceiveBlock b1
    let b2 = TestBlocks.signedPB TestBlocks.testBB2
    TestBlocks.succeedReceiveBlock b2
    let b3 = TestBlocks.signedPB TestBlocks.testBB3
    TestBlocks.succeedReceiveBlock b3
    -- block 1 is finalized as b2 has a qc for b1 and b3 has a qc for b2.

    -- Now we time out round 3 with a reference to b1.
    let b2QC = blockQuorumCertificate $ pbBlock b2 -- qc for b1
        tm0_3 = testTimeoutMessage 0 (Round 3) b2QC
        tm1_3 = testTimeoutMessage 1 (Round 3) b2QC
        tm2_3 = testTimeoutMessage 2 (Round 3) b2QC
        tm3_3 = testTimeoutMessage 3 (Round 3) b2QC
    succeedReceiveExecuteTimeoutMessage tm0_3
    succeedReceiveExecuteTimeoutMessage tm1_3
    succeedReceiveExecuteTimeoutMessage tm2_3
    succeedReceiveExecuteTimeoutMessage tm3_3

    -- we grab the timeout certificate for round 3 in the round status
    -- so we can create a b4 with this tc.
    -- todo: maybe just spell out this tc.
    r3rt <- use $ roundStatus . rsPreviousRoundTimeout
    b4 <- case r3rt of
        Absent -> liftIO . assertFailure $ "There should be a previous round timeout"
        Present rt ->
            return $
                TestBlocks.signedPB
                    BakedBlock
                        { bbRound = 4,
                          bbEpoch = 0,
                          bbTimestamp = 3_000,
                          bbBaker = 3,
                          bbQuorumCertificate = validQCFor testBB2,
                          bbTimeoutCertificate = Present $ rtTimeoutCertificate rt,
                          bbEpochFinalizationEntry = Absent,
                          bbNonce = computeBlockNonce genesisLEN 4 (TestBlocks.bakerVRFKey (3 :: Int)),
                          bbTransactions = Vec.empty,
                          bbTransactionOutcomesHash = emptyBlockTOH 3,
                          bbStateHash = read "3c7aaf3f231da62001d6b6dc651da0af08d72eca5d7b3a8970ee1b33367874f6"
                        }
    TestBlocks.succeedReceiveBlock b4
    -- There is one current timeout message and one current quorum message
    let tm0_4 = testTimeoutMessage 0 (Round 4) b2QC
        qm1_4 = testQuorumMessage 1 (Round 4) 0 (getHash b4)
    succeedReceiveExecuteTimeoutMessage tm0_4
    succeedReceiveProcessQuorumMessage qm1_4

    let expectedCatchupStatus =
            CatchUpStatus
                { cusLastFinalizedBlock = getHash b1,
                  cusLastFinalizedRound = Round 1,
                  cusLeaves = [getHash b3, getHash b4],
                  cusBranches = [getHash b2],
                  cusCurrentRound = Round 4,
                  cusCurrentRoundQuorum = Map.insert (getHash b4) (finalizerSet [FinalizerIndex 1]) Map.empty,
                  cusCurrentRoundTimeouts = Present (TimeoutSet 0 (finalizerSet [FinalizerIndex 0]) emptyFinalizerSet)
                }
    actualCatchupStatus <- makeCatchUpStatus <$> get
    liftIO $ assertEqual "Unexpected catchup status" expectedCatchupStatus actualCatchupStatus

-- |Test of a basic catch up.
testBasicCatchup :: Assertion
testBasicCatchup = do
    -- Responder to catchup
    (rStatus, responderState) <- runTest $ do
        let b1 = TestBlocks.signedPB TestBlocks.testBB1
        TestBlocks.succeedReceiveBlock b1
        let b2 = TestBlocks.signedPB TestBlocks.testBB2
        TestBlocks.succeedReceiveBlock b2
        let b3 = TestBlocks.signedPB TestBlocks.testBB3
        TestBlocks.succeedReceiveBlock b3
        -- b2 has a qc for b1, b3 has a qc for b2 and
        -- b1 and b2 is in consecutive rounds so b1 is finalized.
        let b3QC = bbQuorumCertificate TestBlocks.testBB3
            qsmR4 = QuorumSignatureMessage TestBlocks.genesisHash (getHash b3) (Round 3) 0
            qmR4Sig = signQuorumSignatureMessage qsmR4 (bakerAggregationKey . fst $ TestBlocks.bakers !! 1)
            qmR4 = buildQuorumMessage qsmR4 qmR4Sig (FinalizerIndex 1)
            tmR4 = head $ timeoutMessagesFor b3QC (Round 3) 0
            finToQMsgMap = Map.insert (FinalizerIndex 1) qmR4 Map.empty
            finToTMMap = Map.insert (FinalizerIndex 0) tmR4 Map.empty
        -- Setting the current quorum and timeout message.
        currentQuorumMessages .= QuorumMessages finToQMsgMap Map.empty
        currentTimeoutMessages .= Present (TimeoutMessages 1 finToTMMap Map.empty)
        sd <- get
        let statusMessage = makeCatchUpStatusMessage sd
        return (statusMessage, sd)
    -- Initiator of catchup
    (req, initatorState) <- runTest $ do
        let b1 = TestBlocks.signedPB TestBlocks.testBB1
        TestBlocks.succeedReceiveBlock b1
        let b2 = TestBlocks.signedPB TestBlocks.testBB2
        TestBlocks.succeedReceiveBlock b2
        sd <- get
        catchupRequired <- isCatchUpRequired (cumStatus rStatus) sd
        liftIO $ assertBool "Catch-up should be required." catchupRequired
        return (makeCatchUpStatus sd, sd)
    resp <- runTest $ do
        put responderState
        handleCatchUpRequest req =<< get
    -- Let the initiator catchup.
    runTest $ do
        -- Set the initiator state
        put initatorState
        termData <- consumeResponse resp
        processCatchUpTerminalData termData >>= \case
            TerminalDataResultValid stateProgessed -> liftIO $ assertBool "State should have progessed" stateProgessed
            TerminalDataResultInvalid _ -> liftIO $ assertFailure "The terminal data should be valid."

tests :: Spec
tests = describe "KonsensusV1.CatchUp" $ do
    it "Basic catch-up response" basicCatchupResponse
    it "Catch-up with epoch transition" catchupWithEpochTransitionResponse
    it "Catch-up with timeout in the middle of the chain" catchupWithTimeoutsResponse
    it "Catch-up with one timeout certificate at end" catchupWithOneTimeoutAtEndResponse
    it "Catch-up with two timeout certificates at end" catchupWithTwoTimeoutsAtEndResponse
    it "Catch-up with two branches" catchupWithTwoBranchesResponse
    it "Make catchup status" testMakeCatchupStatus
    it "Basic catch-up integration test" testBasicCatchup
