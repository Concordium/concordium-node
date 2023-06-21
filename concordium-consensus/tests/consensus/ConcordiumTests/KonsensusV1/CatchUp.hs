{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |A module that tests the in-band catch-up mechanism of KonsensusV1.
module ConcordiumTests.KonsensusV1.CatchUp where

import Control.Monad.State.Strict

import Test.HUnit
import Test.Hspec
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.BakerIdentity
import Concordium.Types.HashableTo
import qualified Data.Map.Strict as Map

import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.CatchUp
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import ConcordiumTests.KonsensusV1.Consensus.Blocks

import qualified ConcordiumTests.KonsensusV1.Consensus.Blocks as TestBlocks

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
                  cusLastFinalizedRound = Round 0,
                  cusLeaves = [],
                  cusBranches = [],
                  cusCurrentRound = Round 1,
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

tests :: Spec
tests = describe "KonsensusV1.CatchUp" $ do
    it "Basic catchup" basicCatchup
