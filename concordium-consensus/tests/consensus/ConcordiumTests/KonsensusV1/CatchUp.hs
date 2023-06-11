{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- |A module that tests the in-band catch-up mechanism of KonsensusV1.
module ConcordiumTests.KonsensusV1.CatchUp where

import Test.HUnit
import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Lens.Micro.Platform
import qualified Data.Vector as Vec

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Types.DummyData as Dummy
import qualified Concordium.GlobalState.DummyData as Dummy
import qualified Concordium.Crypto.DummyData as Dummy
import Concordium.Types.BakerIdentity
import Concordium.GlobalState.BakerInfo
import Concordium.Types
import Concordium.Genesis.Data
import qualified  Concordium.Genesis.Data.BaseV1 as BaseV1
import Concordium.Startup
import Concordium.Types.HashableTo
import qualified Data.Map.Strict as Map
import Concordium.Utils
import qualified Concordium.Genesis.Data.P6 as P6

import Concordium.Types.Transactions
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.Types
import Concordium.KonsensusV1.Consensus.CatchUp
import Concordium.KonsensusV1.TreeState.Types

import ConcordiumTests.KonsensusV1.LMDB
import ConcordiumTests.KonsensusV1.TreeStateTest

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
validSignBlock :: BakedBlock -> SignedBlock
validSignBlock bb = signBlock (bakerKey (bbBaker bb)) genesisHash bb

-- |Hash of the genesis block.
genesisHash :: BlockHash
genesisHash = BlockHash $ Hash.hash "My dummy genesis hash"

genesisBlockPointer :: BlockPointer 'P6
genesisBlockPointer =
    BlockPointer
    { bpInfo = genesisBlockMetadata,
      bpBlock = GenesisBlock metadata,
      bpState = dummyBlockState
    }
  where
    genesisBlockMetadata =
      BlockMetadata
      { bmHeight = 0,
        bmReceiveTime = timestampToUTCTime $ BaseV1.genesisTime $ P6.genesisCore $ unGDP6 genesisData,
        bmArriveTime = timestampToUTCTime $ BaseV1.genesisTime $ P6.genesisCore $ unGDP6 genesisData,
        bmEnergyCost = 0,
        bmTransactionsSize = 0
      }
    metadata =
      GenesisMetadata
      { gmParameters = P6.genesisCore $ unGDP6 genesisData,
        gmCurrentGenesisHash = genesisHash,
        gmFirstGenesisHash = genesisHash,
        gmStateHash = StateHashV0 $ Hash.hash "empty state hash"
      }

-- |A signed block.
-- The block has no meaningful state.
signedBlock :: Round -> QuorumCertificate -> Option TimeoutCertificate -> SignedBlock
signedBlock r qc oTC = validSignBlock (BakedBlock r 0 0 0 qc oTC Absent dummyBlockNonce Vec.empty emptyTransactionOutcomesHashV1 (StateHashV0 $ Hash.hash "empty state hash"))

-- |A type that informs 'makeDummyBlockPointer' of how it should
-- create the new block pointer.
data BlockMake pv
  -- Make a block pointer for the provided 'Round' with the last certified block
  -- being the block pointer.
  = Timeout !Round !(BlockPointer pv)
  -- Make a block that extends the provided block pointer and
  -- contains a QC for it.
  | Quorum !(BlockPointer pv)

blockMakeRound :: BlockMake pv -> Round
blockMakeRound (Timeout r _) = r
blockMakeRound (Quorum bp) = blockRound bp

blockMakeBp :: BlockMake pv -> BlockPointer pv
blockMakeBp (Timeout _ bp) = bp
blockMakeBp (Quorum bp) = bp

-- |Make a dummy 'BlockPointer' that is a continuation of either a timeout
-- or a block.
makeDummyBlockPointer :: BlockMake pv -> BlockPointer pv
makeDummyBlockPointer bMake = makeBp
  where
    makeBp = BlockPointer
        {
            bpInfo = metadata,
            bpBlock = theBlock,
            bpState = dummyBlockState
        }
    makeQC bp =
          QuorumCertificate
              {
                qcBlock = getHash bp,
                qcRound = 1 + blockRound bp,
                qcEpoch = blockEpoch bp,
                qcAggregateSignature = mempty,
                qcSignatories = FinalizerSet 0
              }
    makeTC bp r = 
      TimeoutCertificate
          {
            tcRound = r,
            tcMinEpoch = blockEpoch bp,
            tcFinalizerQCRoundsFirstEpoch = FinalizerRounds Map.empty,
            tcFinalizerQCRoundsSecondEpoch = FinalizerRounds Map.empty,
            tcAggregateSignature = mempty
          }
    metadata =
        BlockMetadata
            {
                bmHeight = (BlockHeight 1) + (blockHeight $ blockMakeBp bMake),
                bmReceiveTime = timestampToUTCTime 0,
                bmArriveTime = timestampToUTCTime 0,
                bmEnergyCost = 0,
                bmTransactionsSize = 0
            }
    theBlock = case bMake of
        (Timeout r bp) -> NormalBlock $ signedBlock theRound (makeQC bp) (Present $ makeTC bp r)
        (Quorum bp) -> NormalBlock $ signedBlock theRound (makeQC bp) Absent
      where
        theRound = Round 1 + blockMakeRound bMake

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
-- * Block 1 is the highest certified block.
-- * N is in round 2
--
-- The state of P is as follows:
-- * Blocks 0,1,2 are finalized
-- * Highest certified block is block 3.
-- * P also has a block 4.
-- * P is in round 5.
catchupNoBranches :: Assertion
catchupNoBranches = runTest $ do
    -- set current round to 5
    roundStatus . rsCurrentRound .= Round 5
    -- Blocks 0,1 and 2 are finalized
    writeBlocks [storedBlockRound0, storedBlockRound1, storedBlockRound2] dummyFinalizationEntry
    -- block in round 3 is the highest certified block.
    addToBranches block3
    blockTable . liveMap . at' (getHash block3) ?=! MemBlockAlive block3
--    roundStatus . rsHighestCertifiedBlock .= block3 todo certify the block
    -- block in round 4 with a TC pointing to block2.
    addToBranches block4
    blockTable . liveMap . at' (getHash block4) ?=! MemBlockAlive block4
    -- Handle catchup status from a peer that is in round 2 and knows about the two first blocks.
    let request = CatchUpStatus {
          cusLastFinalizedBlock = (getHash . stbBlock) storedBlockRound0,
          cusLastFinalizedRound = Round 0,
          cusLeaves = [],
          cusBranches = [],
          cusCurrentRound = Round 1,
          cusCurrentRoundQuorum = Map.empty
          }
          
    handleCatchUpRequest request >>= \case
        CatchUpPartialResponseBlock{..} -> liftIO $ do
                -- todo insert asserts
            return ()
        _ -> liftIO $ assertFailure "Should not be terminal data just yet as we need a block first."
  where
    storedBlockRound0 = dummyStoredBlockEmpty 0 0
    storedBlockRound1 = dummyStoredBlockEmpty 1 1
    storedBlockRound2 = dummyStoredBlockEmpty 2 2
    block3 = makeDummyBlockPointer $ Quorum genesisBlockPointer -- todo: not very nice. maybe just add an initial variant.
    block4 = makeDummyBlockPointer $ Quorum block3

-- |A test where node N tries to catch up with peer P.
-- 
-- The state of N is as follows:
-- * Block 1 is the highest certified block.
-- * N is in round 2
--
-- The state of P is as follows:
-- * Blocks 0,1,2 are finalized
-- * Block 3 is on one branch
-- * Block 4 contains a TC for round 3, and QC for block 2.
-- * Block 4 is the highest certified block.
-- * Block 5 is extending block 4 in round 5.
-- * P is in round 6.
catchupTwoBranches :: Assertion
catchupTwoBranches = runTest $ do
    -- set current round to 6
    roundStatus . rsCurrentRound .= Round 6
    -- Blocks 0,1 and 2 are finalized
    writeBlocks [storedBlockRound0, storedBlockRound1, storedBlockRound2] dummyFinalizationEntry
    -- block in round 3 is the highest certified block.
    addToBranches block3
    blockTable . liveMap . at' (getHash block3) ?=! MemBlockAlive block3
--    roundStatus . rsHighestCertifiedBlock .= block3 todo certify the block
    -- block for round 4.
    addToBranches block4
    blockTable . liveMap . at' (getHash block4) ?=! MemBlockAlive block4
    -- block for round 5
    addToBranches block5
    blockTable . liveMap . at' (getHash block5) ?=! MemBlockAlive block5
    -- Handle catchup status from a peer that is in round 2 and knows about the two first blocks.
    let request = CatchUpStatus {
          cusLastFinalizedBlock = (getHash . stbBlock) storedBlockRound0,
          cusLastFinalizedRound = Round 0,
          cusLeaves = [],
          cusBranches = [],
          cusCurrentRound = Round 1,
          cusCurrentRoundQuorum = Map.empty
          }
          
    handleCatchUpRequest request >>= \case
        CatchUpPartialResponseBlock{..} -> liftIO $ do
                -- todo insert asserts
            return ()
        _ -> liftIO $ assertFailure "Should not be terminal data just yet as we need a block first."
  where
    storedBlockRound0 = dummyStoredBlockEmpty 0 0
    storedBlockRound1 = dummyStoredBlockEmpty 1 1
    storedBlockRound2 = dummyStoredBlockEmpty 2 2
    block3 = makeDummyBlockPointer $ Quorum genesisBlockPointer -- todo this is not very nice. Maybe just add a dummy bp initially.
    block4 = makeDummyBlockPointer $ Quorum block3
    block4FromTimeout = makeDummyBlockPointer $ Timeout (Round 4) block3
    block5 = makeDummyBlockPointer $ Quorum block4


-- |A test where node N tries to catch up with peer P.
-- 
-- The state of N is as follows:
-- * Block 1 is the highest certified block.
-- * N is in round 2
--
-- The state of P is as follows:
-- * Blocks 0,1,2 are finalized
-- * Block 3 is on one branch
-- * Block5TC contains a TC for round 3, and QC for block 2.
-- * Block 4 is the highest certified block.
-- * Block 6 is extending block 4 in round 5.
-- * P is in round 6.
catchupThreeBranches :: Assertion
catchupThreeBranches = runTest $ do
    -- set current round to 6
    roundStatus . rsCurrentRound .= Round 6
    -- Blocks 0,1 and 2 are finalized
    writeBlocks [storedBlockRound0, storedBlockRound1, storedBlockRound2] dummyFinalizationEntry
    -- block in round 3 is the highest certified block.
    addToBranches block3
    blockTable . liveMap . at' (getHash block3) ?=! MemBlockAlive block3
--    roundStatus . rsHighestCertifiedBlock .= block3 TODO: certify the block.
    -- block for round 4.
    addToBranches block4
    blockTable . liveMap . at' (getHash block4) ?=! MemBlockAlive block4
    -- block for round 5
    addToBranches block5TC
    blockTable . liveMap . at' (getHash block5TC) ?=! MemBlockAlive block5TC
    -- block for round 5
    addToBranches block6
    blockTable . liveMap . at' (getHash block6) ?=! MemBlockAlive block6
    -- block for round 5
    addToBranches block7TC
    blockTable . liveMap . at' (getHash block7TC) ?=! MemBlockAlive block7TC
    -- Handle catchup status from a peer that is in round 2 and knows about the two first blocks.
    let request = CatchUpStatus {
          cusLastFinalizedBlock = (getHash . stbBlock) storedBlockRound0,
          cusLastFinalizedRound = Round 0,
          cusLeaves = [],
          cusBranches = [],
          cusCurrentRound = Round 1,
          cusCurrentRoundQuorum = Map.empty
          }
          
    handleCatchUpRequest request >>= \case
        CatchUpPartialResponseBlock{..} -> liftIO $ do
                -- todo insert asserts
            return ()
        _ -> liftIO $ assertFailure "Should not be terminal data just yet as we need a block first."
  where
    storedBlockRound0 = dummyStoredBlockEmpty 0 0
    storedBlockRound1 = dummyStoredBlockEmpty 1 1
    storedBlockRound2 = dummyStoredBlockEmpty 2 2
    block3 = makeDummyBlockPointer $ Quorum genesisBlockPointer -- todo this is not very nice. Maybe just add a dummy bp initially.
    block4 = makeDummyBlockPointer $ Quorum block3
    block5TC = makeDummyBlockPointer $ Timeout (Round 4) block3
    block6 = makeDummyBlockPointer $ Quorum block5TC
    block7TC = makeDummyBlockPointer $ Timeout (Round 5) block5TC

tests :: Spec
tests = describe "KonsensusV1.CatchUp" $ do
    it "Catch up no branches" catchupNoBranches
    it "Catch up with two branches" catchupTwoBranches
    it "Catch up with 3 branches - requester knows some of one of the branches." catchupThreeBranches
