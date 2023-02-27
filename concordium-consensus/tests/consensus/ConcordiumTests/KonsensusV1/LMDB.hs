{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module ConcordiumTests.KonsensusV1.LMDB (tests) where

import Control.Exception
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
import Concordium.Types.HashableTo
import Concordium.Types.Transactions

-- |A dummy UTCTime
dummyTime :: UTCTime
dummyTime = posixSecondsToUTCTime 0

-- |A dummy UTCTime
dummyTransactionTime :: TransactionTime
dummyTransactionTime = utcTimeToTransactionTime dummyTime

-- |A dummy BLS secret key
dummyBlsSK :: Bls.SecretKey
dummyBlsSK = fst $ randomBlsSecretKey (mkStdGen 17)

-- |A dummy Hash
dummyHash :: Hash.Hash
dummyHash = Hash.hash "test"

-- |A dummy BlockHash
dummyBlockHash :: BlockHash
dummyBlockHash = BlockHash dummyHash

-- |A dummy QuorumCertificate
dummyQC :: QuorumCertificate
dummyQC =
    QuorumCertificate
        { qcBlock = dummyBlockHash, -- :: !BlockHash,
          qcRound = 1, -- :: !Round,
          qcEpoch = 1, -- :: !Epoch,
          qcAggregateSignature = QuorumSignature $ Bls.sign "someMessage" dummyBlsSK, -- :: !QuorumSignature,
          qcSignatories = FinalizerSet 0 -- :: !FinalizerSet
        }

-- |A dummy KeyPair
dummyKP :: Block.KeyPair
dummyKP = fst $ randomBlockKeyPair (mkStdGen 17)

-- |A dummy VRF KeyPair
dummyVrfKP :: VRF.KeyPair
dummyVrfKP = fst $ VRF.randomKeyPair (mkStdGen 17)

-- |A dummy VRF proof
dummyProof :: VRF.Proof
dummyProof = VRF.prove dummyVrfKP "foo"

-- |A dummy block signature
dummyBlockSig :: Block.Signature
dummyBlockSig = Block.sign dummyKP "someMessage"

-- |A dummy BakedBlock given a round, and a vector of block items
dummyBakedBlock :: Round -> Vector.Vector BlockItem -> BakedBlock
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
          bbTransactionOutcomesHash = TransactionOutcomesHash dummyHash,
          bbStateHash = StateHashV0 dummyHash
        }

-- |A dummy account address given a seed
dummyAccountAddress :: Int -> AccountAddress
dummyAccountAddress seed = fst $ randomAccountAddress (mkStdGen seed)

-- |A dummy TransactionHeader
dummyTransactionHeader :: TransactionHeader
dummyTransactionHeader =
    TransactionHeader
        { thSender = dummyAccountAddress 1,
          thNonce = 1,
          thEnergyAmount = 1234,
          thPayloadSize = 1,
          thExpiry = dummyTransactionTime
        }

-- |A dummy AccountTransaction
dummyAccountTransaction :: AccountTransaction
dummyAccountTransaction =
    AccountTransaction
        { -- \|Signature
          atrSignature = TransactionSignature $ Map.fromList [(1, Map.fromList [(1, Signature "bla")])], -- :: !TransactionSignature,
          -- \|Header
          atrHeader = dummyTransactionHeader, -- :: !TransactionHeader,
          -- \|Serialized payload
          atrPayload = EncodedPayload "bla", -- :: !EncodedPayload,
          -- \|Hash used for signing
          atrSignHash = TransactionSignHashV0 dummyHash -- :: !TransactionSignHashV0
        }

-- |A dummy BareBlockItem
dummyBareBlockItem :: BareBlockItem
dummyBareBlockItem = NormalTransaction dummyAccountTransaction

-- |A dummy BlockItem
dummyBlockItem :: BlockItem
dummyBlockItem = addMetadata id dummyTransactionTime dummyBareBlockItem

-- |A dummy SignedBlock
dummySignedBlock :: Round -> Vector.Vector BlockItem -> SignedBlock
dummySignedBlock n ts = SignedBlock b h dummyBlockSig
  where
    b = dummyBakedBlock n ts
    h = getHash b

-- |A dummy Block with the given round and block items
dummyBlock :: Round -> Vector.Vector BlockItem -> Block 'P6
dummyBlock n ts = NormalBlock $ dummySignedBlock n ts

-- |A dummy StoredBlock with the given block height and round. No transactions.
dummyStoredBlockEmpty :: BlockHeight -> Round -> StoredBlock 'P6
dummyStoredBlockEmpty h n = StoredBlock (BlockMetadata h dummyTime dummyTime) (dummyBlock n Vector.empty) (BlobRef 0)

-- |A dummy StoredBlock with the given block height and round. Contains one transaction.
dummyStoredBlockOneTransaction :: BlockHeight -> Round -> StoredBlock 'P6
dummyStoredBlockOneTransaction h n = StoredBlock (BlockMetadata h dummyTime dummyTime) (dummyBlock n $ Vector.singleton dummyBlockItem) (BlobRef 0)

-- |Dummy list of stored blocks.
dummyStoredBlocks :: [StoredBlock 'P6]
dummyStoredBlocks =
    [ dummyStoredBlockEmpty 0 9,
      dummyStoredBlockEmpty 1 1,
      dummyStoredBlockEmpty 0x100 2,
      dummyStoredBlockEmpty 0x10000 3,
      dummyStoredBlockEmpty 0x1000000 4,
      dummyStoredBlockEmpty 0x100000000 5,
      dummyStoredBlockEmpty 0x10000000000 6,
      dummyStoredBlockEmpty 0x1000000000000 7,
      dummyStoredBlockEmpty 0x100000000000000 8,
      dummyStoredBlockOneTransaction 5 10
    ]

-- |Dummy list of stored blocks of sequential height.
dummyStoredBlocksSequentialHeights :: [StoredBlock 'P6]
dummyStoredBlocksSequentialHeights =
    [ dummyStoredBlockEmpty 0 0,
      dummyStoredBlockEmpty 1 1,
      dummyStoredBlockEmpty 2 2,
      dummyStoredBlockEmpty 3 3,
      dummyStoredBlockEmpty 4 4,
      dummyStoredBlockOneTransaction 5 5
    ]

-- |Dummy FinalizationEntry.
dummyFinalizationEntry :: FinalizationEntry
dummyFinalizationEntry =
    let feSuccessorProof = BlockQuasiHash dummyHash
        feFinalizedQuorumCertificate = dummyQC
        succRound = qcRound feFinalizedQuorumCertificate + 1
        feSuccessorQuorumCertificate =
            dummyQC
                { qcRound = succRound,
                  qcBlock = successorBlockHash (BlockHeader succRound (qcEpoch dummyQC) (qcBlock feFinalizedQuorumCertificate)) feSuccessorProof
                }
    in  FinalizationEntry{..}

-- In the following we test all of the functions of 'MonadTreeStateStore'.

-- |Test of the function 'lookupLastBlock'
testLookupLastBlock :: Assertion
testLookupLastBlock = do
    let action = do
            writeBlocks dummyStoredBlocks dummyFinalizationEntry
            lastBlock <- lookupLastBlock
            case lastBlock of
                Nothing -> liftIO $ assertBool "Block should be Just" False
                Just sb -> liftIO $ assertEqual "BlockHeight should be 0x100000000000000" 0x100000000000000 (bmHeight $ stbInfo sb)
    withTempDirectory "" "lookupLastBlockTest" $ \path ->
        bracket
            (makeDatabaseHandlers path False 1000 :: IO (DatabaseHandlers 'P6))
            closeDatabase
            (\dbhandlers -> runSilentLogger $ runReaderT (runDiskLLDBM action) dbhandlers)

-- |Test of the function 'LookupFirstBlock'
testLookupFirstBlock :: Assertion
testLookupFirstBlock = do
    let action = do
            writeBlocks dummyStoredBlocks dummyFinalizationEntry
            lastBlock <- lookupFirstBlock
            case lastBlock of
                Nothing -> liftIO $ assertBool "Block should be Just" False
                Just sb -> liftIO $ assertEqual "BlockHeight should be 0" 0 (bmHeight $ stbInfo sb)
    withTempDirectory "" "lookupFirstBlockTest" $ \path ->
        bracket
            (makeDatabaseHandlers path False 1000 :: IO (DatabaseHandlers 'P6))
            closeDatabase
            (\dbhandlers -> runSilentLogger $ runReaderT (runDiskLLDBM action) dbhandlers)

-- |Test of the function 'LookupBlockByHeight'
testLookupBlockByHeight :: Assertion
testLookupBlockByHeight = do
    let action = do
            writeBlocks dummyStoredBlocks dummyFinalizationEntry
            lastBlock <- lookupBlockByHeight 0x10000
            case lastBlock of
                Nothing -> liftIO $ assertBool "Block should be Just" False
                Just sb -> liftIO $ assertEqual "BlockHeight should be 0x10000" 0x10000 (bmHeight $ stbInfo sb)
    withTempDirectory "" "lookupBlockByHeightTest" $ \path ->
        bracket
            (makeDatabaseHandlers path False 1000 :: IO (DatabaseHandlers 'P6))
            closeDatabase
            (\dbhandlers -> runSilentLogger $ runReaderT (runDiskLLDBM action) dbhandlers)

-- |Test of the function 'memberBlock'
testMemberBlock :: Assertion
testMemberBlock = do
    let action = do
            writeBlocks dummyStoredBlocks dummyFinalizationEntry
            isMember <- memberBlock $ getHash $ dummyBakedBlock 1 Vector.empty
            liftIO $ assertBool "isMember should be True" isMember
    withTempDirectory "" "memberBlockTest" $ \path ->
        bracket
            (makeDatabaseHandlers path False 1000 :: IO (DatabaseHandlers 'P6))
            closeDatabase
            (\dbhandlers -> runSilentLogger $ runReaderT (runDiskLLDBM action) dbhandlers)

-- |Test of the function 'lookupBlock'
testLookupBlock :: Assertion
testLookupBlock = do
    let action = do
            writeBlocks dummyStoredBlocks dummyFinalizationEntry
            block <- lookupBlock $ getHash $ dummyBakedBlock 5 Vector.empty
            case block of
                Nothing -> liftIO $ assertBool "Block should be Just" False
                Just sb -> liftIO $ assertEqual "BlockHeight should be 0x100000000" 0x100000000 (bmHeight $ stbInfo sb)
    withTempDirectory "" "lookupBlockTest" $ \path ->
        bracket
            (makeDatabaseHandlers path False 1000 :: IO (DatabaseHandlers 'P6))
            closeDatabase
            (\dbhandlers -> runSilentLogger $ runReaderT (runDiskLLDBM action) dbhandlers)

-- |Test of the function 'lookupFinalizationEntry'
testLookupLatestFinalizationEntry :: Assertion
testLookupLatestFinalizationEntry = do
    let action = do
            writeBlocks dummyStoredBlocks dummyFinalizationEntry
            fe <- lookupLatestFinalizationEntry
            case fe of
                Nothing -> liftIO $ assertBool "Finalization entry should be Just" False
                Just f -> liftIO $ assertEqual "Finalization entry should match" dummyFinalizationEntry f
    withTempDirectory "" "lookupFinalizationEntryTest" $ \path ->
        bracket
            (makeDatabaseHandlers path False 1000 :: IO (DatabaseHandlers 'P6))
            closeDatabase
            (\dbhandlers -> runSilentLogger $ runReaderT (runDiskLLDBM action) dbhandlers)

-- |Test of the function 'lookupTransaction'
testLookupTransaction :: Assertion
testLookupTransaction = do
    let action = do
            writeBlocks dummyStoredBlocks dummyFinalizationEntry
            ft <- lookupTransaction $ wmdHash $ dummyBlockItem
            case ft of
                Nothing -> liftIO $ assertBool "Finalized transaction status should be Just" False
                Just FinalizedTransactionStatus{..} -> do
                    liftIO $ assertEqual "Block height of transaction should be 5" 5 ftsBlockHeight
                    liftIO $ assertEqual "Transaction index should be 0" 0 ftsIndex
    withTempDirectory "" "lookupTransactionTest" $ \path ->
        bracket
            (makeDatabaseHandlers path False 1000 :: IO (DatabaseHandlers 'P6))
            closeDatabase
            (\dbhandlers -> runSilentLogger $ runReaderT (runDiskLLDBM action) dbhandlers)

-- |Test of the function 'memberTransaction'
testMemberTransaction :: Assertion
testMemberTransaction = do
    let action = do
            writeBlocks dummyStoredBlocks dummyFinalizationEntry
            isMember <- memberTransaction $ wmdHash $ dummyBlockItem
            liftIO $ assertBool "memberTransaction should be True" isMember
    withTempDirectory "" "memberTransactionTest" $ \path ->
        bracket
            (makeDatabaseHandlers path False 1000 :: IO (DatabaseHandlers 'P6))
            closeDatabase
            (\dbhandlers -> runSilentLogger $ runReaderT (runDiskLLDBM action) dbhandlers)

-- |Test of the function 'rollBackBlocksUntil'
testRollBackBlocksUntil :: Assertion
testRollBackBlocksUntil = do
    let action = do
            writeBlocks dummyStoredBlocksSequentialHeights dummyFinalizationEntry
            eb <- rollBackBlocksUntil $ \sb -> return ((bmHeight $ stbInfo sb) == 1)
            case eb of
                Left s -> liftIO $ assertBool ("Roll back failed: " ++ s) False
                Right rollbackCount -> do
                    -- Note that the `dummyStoredBlocksSequentialHeights` has 6 blocks starting from index 0, 1,..
                    -- We are rolling back to index 1 so 4 blocks should be rolled back.
                    liftIO $ assertEqual "Roll-back should have happended and 4 blocks should have been rolled back" 4 rollbackCount
            lastBlock <- lookupLastBlock
            case lastBlock of
                Nothing -> liftIO $ assertBool "Block should be Just" False
                Just sb -> liftIO $ assertEqual "BlockHeight should be 1" 1 (bmHeight $ stbInfo sb)
    withTempDirectory "" "lookupTransactionTest" $ \path ->
        bracket
            (makeDatabaseHandlers path False 1000 :: IO (DatabaseHandlers 'P6))
            closeDatabase
            (\dbhandlers -> runSilentLogger $ runReaderT (runDiskLLDBM action) dbhandlers)

tests :: Spec
tests = describe "KonsensusV2.LMDB" $ do
    it "Test lookupLastBlock" testLookupLastBlock
    it "Test lookupFirstBlock" testLookupFirstBlock
    it "Test lookupBlockByHeight" testLookupBlockByHeight
    it "Test memberBlock" testMemberBlock
    it "Test lookupBlock" testLookupBlock
    it "Test lookupLatestFinalizationEntry" testLookupLatestFinalizationEntry
    it "Test lookupTransaction" testLookupTransaction
    it "Test memberTransaction" testMemberTransaction
    it "Test rollBackBlocksUntil" testRollBackBlocksUntil
