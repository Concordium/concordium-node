{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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

import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.CatchUp
import Concordium.KonsensusV1.Consensus.Timeout
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import ConcordiumTests.KonsensusV1.Consensus.Blocks

import qualified ConcordiumTests.KonsensusV1.Consensus.Blocks as TestBlocks
import qualified ConcordiumTests.KonsensusV1.Consensus.Blocks as Testblocks

assertCatchupResponse :: MonadIO m => CatchUpTerminalData -> [SignedBlock] -> CatchUpPartialResponse m -> m ()
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

-- |Receive and execute a timeout message.
succeedReceiveExecuteTimeoutMessage :: TimeoutMessage -> TestMonad 'P6 ()
succeedReceiveExecuteTimeoutMessage tm = do
    recvResult <- receiveTimeoutMessage tm =<< get
    case recvResult of
        Received verifiedTm ->
            executeTimeoutMessage verifiedTm >>= \case
                ExecutionSuccess -> return ()
                execStatus -> liftIO . assertFailure $ "Expected timeout message was executed, but was " <> show execStatus
        recvStatus -> liftIO . assertFailure $ "Expected timeout message was received, but was " <> show recvStatus

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

-- |Run a TestMonad pv a action with a no-baker context,
-- fixed 'genesisData' and fixed time.
runTest :: TestMonad 'P6 a -> IO a
runTest = runTestMonad @'P6 (BakerContext Nothing) (timestampToUTCTime 1_000) TestBlocks.genesisData

basicCatchup :: Assertion
basicCatchup = runTest $ do
    let b1 = TestBlocks.signedPB TestBlocks.testBB1
    TestBlocks.succeedReceiveBlock b1
    let b2 = TestBlocks.signedPB TestBlocks.testBB2
    TestBlocks.succeedReceiveBlock b2
    let b3 = TestBlocks.signedPB TestBlocks.testBB3
    TestBlocks.succeedReceiveBlock b3
    -- b2 has a qc for b1, b3 has a qc for b2 and
    -- b1 and b2 is in consecutive rounds so b1 is finalized.
    let b3QC = bbQuorumCertificate TestBlocks.testBB3
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
                  cusCurrentRound = Round 2,
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

catchupWithEpochTransition :: Assertion
catchupWithEpochTransition = runTest $ do
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
                  cusCurrentRound = Round 2,
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

catchupWithTimeouts :: Assertion
catchupWithTimeouts = runTest $ do
    let b1 = TestBlocks.signedPB TestBlocks.testBB1E
    TestBlocks.succeedReceiveBlock b1
    let b2 = TestBlocks.signedPB TestBlocks.testBB2E
    TestBlocks.succeedReceiveBlock b2
    -- b3 contains an epoch finalization entry and is in epoch 1.
    let b3 = TestBlocks.signedPB TestBlocks.testBB3E
    Testblocks.succeedReceiveBlock b3
    -- timeout
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
                  cusCurrentRound = Round 2,
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

catchupWithOneTimeoutAtEnd :: Assertion
catchupWithOneTimeoutAtEnd = runTest $ do
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
                  cusCurrentRound = Round 2,
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

catchupWithTwoTimeoutsAtEnd :: Assertion
catchupWithTwoTimeoutsAtEnd = runTest $ do
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
                  cusCurrentRound = Round 2,
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

tests :: Spec
tests = describe "KonsensusV1.CatchUp" $ do
    it "Basic catchup" basicCatchup
    it "Catch-up with epoch transition" catchupWithEpochTransition
    it "Catch-up with timeout in the middle of the chain" catchupWithTimeouts
    it "Catch-up with one timeout certificate at end" catchupWithOneTimeoutAtEnd
    it "Catch-up with two timeout certificates at end" catchupWithTwoTimeoutsAtEnd
