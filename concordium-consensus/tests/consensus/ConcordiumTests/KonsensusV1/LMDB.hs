{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ConcordiumTests.KonsensusV1.LMDB (tests) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as Map
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vector
import System.IO.Temp
import System.Random
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.BlockSignature as Block
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.DummyData (randomBlockKeyPair, randomBlsSecretKey)
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Crypto.SignatureScheme
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.Persistent.BlobStore (BlobRef (..))
import Concordium.ID.Types (randomAccountAddress)
import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.TreeState.LowLevel.LMDB
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Logger
import Concordium.Types
import qualified Concordium.Types.Conditionally as Cond
import Concordium.Types.HashableTo
import Concordium.Types.Option
import Concordium.Types.TransactionOutcomes
import Concordium.Types.Transactions

import qualified ConcordiumTests.KonsensusV1.Common as Common

-- | A dummy UTCTime used for tests where the actual value is not significant.
dummyTime :: UTCTime
dummyTime = posixSecondsToUTCTime 0

-- | A 'TransactionTime' needed for constructing a 'TransactionHeader', and for the metadata of
--  block items.
dummyTransactionTime :: TransactionTime
dummyTransactionTime = utcTimeToTransactionTime $ posixSecondsToUTCTime 0

-- | A BLS secret key needed for creating a 'QuorumSignature'.
dummyBlsSK :: Bls.SecretKey
dummyBlsSK = fst $ randomBlsSecretKey (mkStdGen 17)

-- | A 'Hash' used for constructing a 'BlockHash', a 'TransactionOutcomesHash',
--  a 'StateHashV0', a 'TransactionSignHashV0' and a 'BlockQuasiHash'.
dummyHash :: Hash.Hash
dummyHash = Hash.hash "test"

-- | A 'QuorumCertificate' used in both 'dummyFinalizationEntry' and for constructing a 'BakedBlock'.
dummyQC :: QuorumCertificate
dummyQC =
    QuorumCertificate
        { qcBlock = BlockHash dummyHash,
          qcRound = 1,
          qcEpoch = 1,
          qcAggregateSignature = QuorumSignature $ Bls.sign "someMessage" dummyBlsSK,
          qcSignatories = FinalizerSet 0
        }

-- | A block signature keypair used to construct a block signature.
--  The choice of the keypair is not significant.
dummyKP :: Block.KeyPair
dummyKP = fst $ randomBlockKeyPair (mkStdGen 17)

-- | A VRF keypair used to construct a VRF proof
--  The choice of the keypair is not significant.
dummyVrfKP :: VRF.KeyPair
dummyVrfKP = fst $ VRF.randomKeyPair (mkStdGen 18)

-- | A concrete VRF proof used to construct a 'BakedBlock'.
--  The proof is well-formed, but there should be no other expectation regarding its validity.
dummyProof :: VRF.Proof
dummyProof = VRF.prove dummyVrfKP "foo"

-- | A block signature used in 'dummyBlock'.
--  The signature is well-formed, but there should be no expectation as to its validity.
dummyBlockSig :: Block.Signature
dummyBlockSig = Block.sign dummyKP "someMessage"

-- | A helper function for creating a 'BakedBlock' given a round. Used by 'dummyBlock' to create blocks.
--  The block is well-formed and contains the supplied transactions and is for the specified round.
--  Beyond that, there should be no expectation on the data in the block.
dummyBakedBlock :: forall pv. (IsProtocolVersion pv) => Round -> Vector.Vector BlockItem -> BakedBlock pv
dummyBakedBlock n ts =
    BakedBlock
        { bbRound = n,
          bbEpoch = 1,
          bbTimestamp = 1000,
          bbBaker = 1,
          bbQuorumCertificate = dummyQC,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = dummyProof,
          bbTransactions = ts,
          bbDerivableHashes = case sBlockHashVersionFor (protocolVersion @pv) of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = TransactionOutcomesHash dummyHash,
                      dbhv0BlockStateHash = StateHashV0 dummyHash
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = BlockResultHash dummyHash
                    }
        }

-- | A helper function for creating an account address given a seed.
--  The address is well-formed, and different seeds should give different values.
--  Beyond that, there should be no expectation on the addresses produced.
dummyAccountAddress :: Int -> AccountAddress
dummyAccountAddress seed = fst $ randomAccountAddress (mkStdGen seed)

-- | The transaction header used in 'dummyBlockItem'.
--  This is a well-formed transaction header, but there should be no other expectation on the data.
dummyTransactionHeader :: TransactionHeader
dummyTransactionHeader =
    TransactionHeader
        { thSender = dummyAccountAddress 1,
          thNonce = 1,
          thEnergyAmount = 1234,
          thPayloadSize = 1,
          thExpiry = dummyTransactionTime
        }

-- | A BlockItem used by 'dummyStoredBlockOneTransaction' to create a 'StoredBlock' with this block item in it.
--  This is a well-formed normal transaction block item. There should be no other expectation on the
--  data.
dummyBlockItem :: BlockItem
dummyBlockItem =
    makeBlockItem dummyTransactionTime $
        AccountTransaction
            { atrSignature = TransactionSignature $ Map.fromList [(1, Map.fromList [(1, Signature "bla")])],
              atrHeader = dummyTransactionHeader,
              atrPayload = EncodedPayload "bla",
              atrSignHash = TransactionSignHashV0 dummyHash
            }

-- | A helper function for creating a block with the given round and block items.
--  Blocks with different hashes can then be constructed by calling this function with different rounds.
--  The blocks are derived from 'dummyBakedBlock' with the supplied round and block items.
dummyBlock :: forall pv. (IsProtocolVersion pv) => Round -> Vector.Vector BlockItem -> Block pv
dummyBlock n ts = NormalBlock $ SignedBlock b h dummyBlockSig
  where
    b = dummyBakedBlock n ts
    h = getHash b

-- | A helper function for creating a StoredBlock with the given block height and round, and with no transactions.
--  Empty 'StoredBlock's with different hashes can then be constructed by calling this function with different rounds.
--  The blocks are derived from 'dummyBlock' with the supplied height and round, but no block items.
dummyStoredBlockEmpty :: forall pv. (IsProtocolVersion pv) => BlockHeight -> Round -> StoredBlock pv
dummyStoredBlockEmpty h n = StoredBlock blockMeta (dummyBlock n Vector.empty) (BlobRef 0)
  where
    blockMeta =
        BlockMetadata
            { bmHeight = h,
              bmReceiveTime = dummyTime,
              bmArriveTime = dummyTime,
              bmEnergyCost = 0,
              bmTransactionsSize = 0,
              bmBlockStateHash =
                Cond.conditionally
                    (sBlockStateHashInMetadata (sBlockHashVersionFor (protocolVersion @pv)))
                    (StateHashV0 dummyHash)
            }

-- | A helper function for creating a StoredBlock with the given block height and round, and with one transaction.
--  'StoredBlock's (with one transaction) with different hashes can then be constructed by calling this function with different rounds.
--  The blocks are derived from 'dummyBlock' with the supplied height and round, and a singular 'dummyBlockItem'.
dummyStoredBlockOneTransaction :: forall pv. (IsProtocolVersion pv) => BlockHeight -> Round -> StoredBlock pv
dummyStoredBlockOneTransaction height n = StoredBlock blockMeta (dummyBlock n $ Vector.singleton dummyBlockItem) (BlobRef 0)
  where
    blockMeta =
        BlockMetadata
            { bmHeight = height,
              bmReceiveTime = dummyTime,
              bmArriveTime = dummyTime,
              bmEnergyCost = 200,
              bmTransactionsSize = 200,
              bmBlockStateHash =
                Cond.conditionally
                    (sBlockStateHashInMetadata (sBlockHashVersionFor (protocolVersion @pv)))
                    (StateHashV0 dummyHash)
            }

-- | List of stored blocks used for testing. The heights are chosen so it is tested that the endianness of the stored block heights are correct.
dummyStoredBlocks :: (IsProtocolVersion pv) => [StoredBlock pv]
dummyStoredBlocks =
    [ dummyStoredBlockEmpty 1 1,
      dummyStoredBlockEmpty 0x100 2,
      dummyStoredBlockEmpty 0x10000 3,
      dummyStoredBlockEmpty 0x1000000 4,
      dummyStoredBlockEmpty 0x100000000 5,
      dummyStoredBlockEmpty 0x10000000000 6,
      dummyStoredBlockEmpty 0x1000000000000 7,
      dummyStoredBlockEmpty 0x100000000000000 8,
      dummyStoredBlockEmpty 0 9,
      dummyStoredBlockOneTransaction 5 10
    ]

-- | A FinalizationEntry. Both used by 'writeBlocks' and used when testing 'lookupLatestFinalizationEntry'.
dummyFinalizationEntry :: forall pv. (IsProtocolVersion pv) => FinalizationEntry pv
dummyFinalizationEntry =
    let feSuccessorProof = SuccessorProof dummyHash
        feFinalizedQuorumCertificate = dummyQC
        succRound = qcRound feFinalizedQuorumCertificate + 1
        feSuccessorQuorumCertificate =
            dummyQC
                { qcRound = succRound,
                  qcBlock =
                    successorBlockHash
                        (protocolVersion @pv)
                        (BlockHeader succRound (qcEpoch dummyQC) (qcBlock feFinalizedQuorumCertificate))
                        feSuccessorProof
                }
    in  FinalizationEntry{..}

-- In the following we test all of the functions of 'MonadTreeStateStore'.

-- | Helper function for running a test in the context of 'DiskLLDBM' over
--  some 'Reader'.
--  Running an action within this context creates a temporary file which
--  serves the disk based lmdb implementation the computation.
runLLMDBTest ::
    String ->
    DiskLLDBM pv (ReaderT (DatabaseHandlers pv) (LoggerT IO)) a ->
    IO a
runLLMDBTest name action = withTempDirectory "" name $ \path ->
    bracket
        (openDatabase path :: IO (DatabaseHandlers pv))
        closeDatabase
        (\dbhandlers -> runSilentLogger $ runReaderT (runDiskLLDBM action) dbhandlers)

-- | Set up the database with the 'dummyStoredBlocks' finalized.
setupDummy :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> DiskLLDBM pv (ReaderT (DatabaseHandlers pv) (LoggerT IO)) ()
setupDummy _ = do
    forM_ (dummyStoredBlocks @pv) $ \sb ->
        writeCertifiedBlock
            sb
            dummyQC
                { qcBlock = getHash sb,
                  qcRound = blockRound sb,
                  qcEpoch = blockEpoch sb
                }
    writeFinalizedBlocks (dummyStoredBlocks @pv) dummyFinalizationEntry

-- | Test that 'lookupLastBlock' returns the block with the greatest height among the dummy blocks.
--  The dummy blocks are chosen to have a wide range of blockheights to catch possible endianness
--  errors.
testLookupLastBlock :: (IsProtocolVersion pv) => SProtocolVersion pv -> Assertion
testLookupLastBlock sProtocolVersion = runLLMDBTest "lookupLastBlockTest" $ do
    setupDummy sProtocolVersion
    lastBlock <- lookupLastFinalizedBlock
    case lastBlock of
        Nothing -> liftIO $ assertFailure "Block should be Just"
        Just sb -> liftIO $ assertEqual "BlockHeight should be 0x100000000000000" 0x100000000000000 (blockHeight sb)

-- | Test that the function 'LookupFirstBlock' returns the block with height '0' from the dummy blocks.
testLookupFirstBlock :: (IsProtocolVersion pv) => SProtocolVersion pv -> Assertion
testLookupFirstBlock sProtocolVersion = runLLMDBTest "lookupFirstBlockTest" $ do
    setupDummy sProtocolVersion
    lastBlock <- lookupFirstBlock
    case lastBlock of
        Nothing -> liftIO $ assertFailure "Block should be Just"
        Just sb -> liftIO $ assertEqual "BlockHeight should be 0" 0 (blockHeight sb)

-- | Test that the function 'LookupBlockByHeight' retrieves the correct block at height 0x10000.
testLookupBlockByHeight :: (IsProtocolVersion pv) => SProtocolVersion pv -> Assertion
testLookupBlockByHeight sProtocolVersion = runLLMDBTest "lookupBlockByHeightTest" $ do
    setupDummy sProtocolVersion
    lastBlock <- lookupBlockByHeight 0x10000
    case lastBlock of
        Nothing -> liftIO $ assertFailure "Block should be Just"
        Just sb -> liftIO $ assertEqual "BlockHeight should be 0x10000" 0x10000 (blockHeight sb)

-- | Test that the function 'memberBlock' returns 'True' for a selected block.
testMemberBlock :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> Assertion
testMemberBlock sProtocolVersion = runLLMDBTest "memberBlockTest" $ do
    setupDummy sProtocolVersion
    isMember <- memberBlock $ getHash $ dummyBakedBlock @pv 1 Vector.empty
    liftIO $ assertBool "isMember should be True" isMember

-- | Test that the function 'lookupBlock' retrieves a selected block.
testLookupBlock :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> Assertion
testLookupBlock sProtocolVersion = runLLMDBTest "lookupBlockTest" $ do
    setupDummy sProtocolVersion
    block <- lookupBlock $ getHash $ dummyBakedBlock @pv 5 Vector.empty
    case block of
        Nothing -> liftIO $ assertFailure "Block should be Just"
        Just sb -> liftIO $ assertEqual "BlockHeight should be 0x100000000" 0x100000000 (blockHeight sb)

-- | Test that the function 'lookupFinalizationEntry' retrieves a written expected finalization entry.
testLookupLatestFinalizationEntry :: (IsProtocolVersion pv) => SProtocolVersion pv -> Assertion
testLookupLatestFinalizationEntry sProtocolVersion = runLLMDBTest "lookupFinalizationEntryTest" $ do
    setupDummy sProtocolVersion
    fe <- lookupLatestFinalizationEntry
    case fe of
        Nothing -> liftIO $ assertFailure "Finalization entry should be Just"
        Just f -> liftIO $ assertEqual "Finalization entry should match" dummyFinalizationEntry f

-- | Test that the function 'lookupTransaction' retrieves the expected transaction status.
testLookupTransaction :: (IsProtocolVersion pv) => SProtocolVersion pv -> Assertion
testLookupTransaction sProtocolVersion = runLLMDBTest "lookupTransactionTest" $ do
    setupDummy sProtocolVersion
    ft <- lookupTransaction $ wmdHash $ dummyBlockItem
    case ft of
        Nothing -> liftIO $ assertFailure "Finalized transaction status should be Just"
        Just FinalizedTransactionStatus{..} -> do
            liftIO $ assertEqual "Block height of transaction should be 5" 5 ftsBlockHeight
            liftIO $ assertEqual "Transaction index should be 0" 0 ftsIndex

-- | Test that the function 'memberTransaction' identifies the presence of a known transaction.
testMemberTransaction :: (IsProtocolVersion pv) => SProtocolVersion pv -> Assertion
testMemberTransaction sProtocolVersion = runLLMDBTest "memberTransactionTest" $ do
    setupDummy sProtocolVersion
    isMember <- memberTransaction $ wmdHash $ dummyBlockItem
    liftIO $ assertBool "memberTransaction should be True" isMember

tests :: Spec
tests = describe "KonsensusV2.LMDB" $ do
    Common.forEveryProtocolVersionConsensusV1 $ \spv pvString ->
        describe pvString $ do
            it "Test lookupLastBlock" $ testLookupLastBlock spv
            it "Test lookupFirstBlock" $ testLookupFirstBlock spv
            it "Test lookupBlockByHeight" $ testLookupBlockByHeight spv
            it "Test memberBlock" $ testMemberBlock spv
            it "Test lookupBlock" $ testLookupBlock spv
            it "Test lookupLatestFinalizationEntry" $ testLookupLatestFinalizationEntry spv
            it "Test lookupTransaction" $ testLookupTransaction spv
            it "Test memberTransaction" $ testMemberTransaction spv
