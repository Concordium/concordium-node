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

bakerKey :: Integral a => a -> BakerSignPrivateKey
bakerKey i = bakerSignKey $ fst (bakers !! fromIntegral i)

bakerVRFKey :: Integral a => a -> BakerElectionPrivateKey
bakerVRFKey i = bakerElectionKey $ fst (bakers !! fromIntegral i)

-- makeTestSignedBlock :: Int -> BlockHash -> Round -> Timestamp -> SignedBlock

testUponReceivingBlock :: Assertion
testUponReceivingBlock = do
    runTestMonad noBaker testTime genesisData $ do
        genBP <- use lastFinalized
        genSeedState <- getSeedState (bpState genBP)
        let len = genSeedState ^. currentLeadershipElectionNonce
        let blockForBaker i =
                PendingBlock
                    { pbReceiveTime = testTime,
                      pbBlock =
                        signBlock (bakerKey i) genesisHash $
                            BakedBlock
                                { bbRound = 1,
                                  bbEpoch = 0,
                                  bbTimestamp = 1_000,
                                  bbBaker = i,
                                  bbQuorumCertificate = genesisQuorumCertificate genesisHash,
                                  bbTimeoutCertificate = Absent,
                                  bbEpochFinalizationEntry = Absent,
                                  bbNonce = computeBlockNonce len 0 (bakerVRFKey i),
                                  bbTransactions = Vec.empty,
                                  bbTransactionOutcomesHash = emptyTransactionOutcomesHashV1,
                                  bbStateHash = getHash (bpState genBP)
                                }
                    }
        forM_ [0 .. 4] $ \bid -> do
            res <- uponReceivingBlock $ blockForBaker bid
            liftIO $ print res
            case res of
                BlockResultSuccess vb -> do
                    executeBlock vb
                    brs <- use branches
                    liftIO $ print brs
                _ -> return ()
  where
    noBaker = BakerContext Nothing
    testTime = timestampToUTCTime 1_000

tests :: Spec
tests = describe "KonsensusV1.Consensus.Blocks" $ do
    it "uponReceivingBlock" testUponReceivingBlock
    return ()
