{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ConcordiumTests.KonsensusV1.TreeStateTest where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import System.IO.Unsafe
import System.Random
import Test.HUnit
import Test.Hspec

-- base types.
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Genesis.Data.BaseV1
import Concordium.Scheduler.DummyData
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.Transactions

-- konsensus v1 related imports.
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Parameters (defaultRuntimeParameters)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.TreeState (insertDeadCache, memberDeadCache)
import Concordium.GlobalState.TransactionTable
import Concordium.ID.Types
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.TreeState.LowLevel.Memory
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types.Updates

-- |A dummy block state that is just a @BlobRef 0@.
dummyPersistentBlockState :: PersistentBlockState pv
{-# NOINLINE dummyPersistentBlockState #-}
dummyPersistentBlockState = unsafePerformIO $ newIORef $ blobRefToBufferedRef (BlobRef 0)

dummyStateHash :: StateHash
dummyStateHash = StateHashV0 $ Hash.hash "DummyPersistentBlockState"

-- |A dummy block state that has no meaningful content.
dummyBlockState :: HashedPersistentBlockState pv
dummyBlockState = HashedPersistentBlockState{..}
  where
    hpbsPointers = dummyPersistentBlockState
    hpbsHash = dummyStateHash

dummyQuorumCertificate :: BlockHash -> QuorumCertificate
dummyQuorumCertificate bh =
    QuorumCertificate
        { qcBlock = bh,
          qcRound = 0,
          qcEpoch = 0,
          qcAggregateSignature = mempty,
          qcSignatories = FinalizerSet 0
        }

dummyVRFKeys :: VRF.KeyPair
dummyVRFKeys = fst $ VRF.randomKeyPair (mkStdGen 0)

dummySignKeys :: BakerSignPrivateKey
{-# NOINLINE dummySignKeys #-}
dummySignKeys = unsafePerformIO Sig.newKeyPair

dummyBlockNonce :: BlockNonce
dummyBlockNonce = VRF.prove dummyVRFKeys ""

dummyBakedBlock :: BlockHash -> Round -> BakedBlock
dummyBakedBlock parentHash bbRound = BakedBlock{..}
  where
    bbEpoch = 0
    bbTimestamp = 0
    bbBaker = 0
    bbQuorumCertificate = dummyQuorumCertificate parentHash
    bbTimeoutCertificate = Absent
    bbEpochFinalizationEntry = Absent
    bbNonce = dummyBlockNonce
    bbTransactions = mempty
    bbTransactionOutcomesHash = TransactionOutcomesHash minBound
    bbStateHash = dummyStateHash

dummySignedBlock :: BlockHash -> Round -> SignedBlock
dummySignedBlock parentHash = signBlock dummySignKeys dummyGenesisBlockHash . dummyBakedBlock parentHash

dummyPendingBlock :: BlockHash -> Round -> PendingBlock
dummyPendingBlock parentHash r =
    PendingBlock
        { pbBlock = dummySignedBlock parentHash r,
          pbReceiveTime = timestampToUTCTime 0
        }

dummyBlock :: Round -> BlockPointer pv
dummyBlock rnd = BlockPointer{..}
  where
    bpInfo =
        BlockMetadata
            { bmHeight = fromIntegral rnd,
              bmReceiveTime = timestampToUTCTime 0,
              bmArriveTime = timestampToUTCTime 0
            }
    bpBlock = NormalBlock $ dummySignedBlock (BlockHash minBound) rnd
    bpState = dummyBlockState

dummyGenesisBlockHash :: BlockHash
dummyGenesisBlockHash = BlockHash (Hash.hash "DummyGenesis")

dummyGenesisConfiguration :: GenesisConfiguration
dummyGenesisConfiguration =
    GenesisConfiguration
        { gcParameters =
            CoreGenesisParametersV1
                { genesisTime = 0,
                  genesisEpochDuration = 3_600_000
                },
          gcCurrentGenesisHash = dummyGenesisBlockHash,
          gcFirstGenesisHash = dummyGenesisBlockHash,
          gcStateHash = getHash dummyBlockState
        }

dummyLeadershipElectionNonce :: LeadershipElectionNonce
dummyLeadershipElectionNonce = Hash.hash "LeadershipElectionNonce"

-- |Dummy bakers and finalizers with no bakers or finalizers.
-- This is only suitable for when the value is not meaningfully used.
dummyBakersAndFinalizers :: BakersAndFinalizers
dummyBakersAndFinalizers =
    BakersAndFinalizers
        { _bfBakers = FullBakers Vec.empty 0,
          _bfFinalizers = FinalizationCommittee Vec.empty 0
        }

-- |Dummy epoch bakers. This is only suitable for when the actual value is not meaningfully used.
dummyEpochBakers :: EpochBakers
dummyEpochBakers = EpochBakers 0 dummyBakersAndFinalizers dummyBakersAndFinalizers 1

dummyInitialSkovData :: SkovData pv
dummyInitialSkovData =
    mkInitialSkovData
        defaultRuntimeParameters
        dummyGenesisConfiguration
        dummyBlockState
        10_000
        dummyLeadershipElectionNonce
        dummyEpochBakers

newtype TestLLDB pv = TestLLDB {theTestLLDB :: IORef (LowLevelDB pv)}

instance HasMemoryLLDB pv (TestLLDB pv) where
    theMemoryLLDB = theTestLLDB

runTestLLDB :: LowLevelDB pv -> MemoryLLDBM pv (ReaderT (TestLLDB pv) IO) b -> IO b
runTestLLDB initDB a = do
    tdb <- TestLLDB <$> newIORef initDB
    runReaderT (runMemoryLLDBM a) tdb

-- Test values

genB :: BlockPointer pv
genB = dummyBlock 0
lastFin :: BlockPointer pv
lastFin = dummyBlock 20
testB :: BlockPointer pv
testB = dummyBlock 21
focusB :: BlockPointer pv
focusB = dummyBlock 22
pendingB :: PendingBlock
pendingB = dummyPendingBlock (BlockHash minBound) 33
deadH :: BlockHash
deadH = BlockHash (Hash.hash "DeadBlock")
unknownH :: BlockHash
unknownH = BlockHash (Hash.hash "Unknown")

-- |Convert a block pointer to a stored block, using `BlobRef 0` as the state pointer.
toStoredBlock :: BlockPointer pv -> StoredBlock pv
toStoredBlock BlockPointer{..} =
    StoredBlock
        { stbStatePointer = BlobRef 0,
          stbInfo = bpInfo,
          stbBlock = bpBlock
        }

skovDataWithTestBlocks :: SkovData pv
skovDataWithTestBlocks =
    dummyInitialSkovData
        & lastFinalized .~ lastFin
        & focusBlock .~ focusB
        & blockTable
            %~ ( ( liveMap
                    %~ ( (at (getHash testB) ?~ MemBlockAlive testB)
                            . (at (getHash focusB) ?~ MemBlockAlive focusB)
                            . (at (getHash pendingB) ?~ MemBlockPending pendingB)
                       )
                 )
                    . ( deadBlocks %~ insertDeadCache deadH
                      )
               )

-- |A test 'LowLevelDB' with the genesis block.
lldbWithGenesis :: LowLevelDB pv
lldbWithGenesis =
    initialLowLevelDB
        (toStoredBlock genB)
        (initialRoundStatus 10_000 dummyLeadershipElectionNonce)

testDoGetMemoryBlockStatus :: Spec
testDoGetMemoryBlockStatus = describe "doGetMemoryBlockStatus" $ do
    it "last finalized" $ doGetMemoryBlockStatus (getHash lastFin) sd `shouldBe` Just (BlockFinalized lastFin)
    it "live" $ doGetMemoryBlockStatus (getHash testB) sd `shouldBe` Just (BlockAlive testB)
    it "focus block" $ doGetMemoryBlockStatus (getHash focusB) sd `shouldBe` Just (BlockAlive focusB)
    it "pending block" $ doGetMemoryBlockStatus (getHash pendingB) sd `shouldBe` Just (BlockPending pendingB)
    it "dead block" $ doGetMemoryBlockStatus deadH sd `shouldBe` Just BlockDead
    it "unknown block" $ doGetMemoryBlockStatus unknownH sd `shouldBe` Nothing
  where
    sd = skovDataWithTestBlocks

testDoGetBlockStatus :: Spec
testDoGetBlockStatus = describe "doGetBlockStatus" $ do
    it "last finalized" $ getStatus (getHash lastFin) $ BlockFinalized lastFin
    it "live" $ getStatus (getHash testB) $ BlockAlive testB
    it "focus block" $ getStatus (getHash focusB) $ BlockAlive focusB
    it "pending block" $ getStatus (getHash pendingB) $ BlockPending pendingB
    it "dead block" $ getStatus deadH BlockDead
    it "genesis block" $ getStatus (getHash genB) $ BlockFinalized genB
    it "unknown block" $ getStatus unknownH BlockUnknown
  where
    getStatus bh expect = do
        s <- runTestLLDB (lldbWithGenesis @'P6) $ doGetBlockStatus bh sd
        s `shouldBe` expect
    sd = skovDataWithTestBlocks

testDoGetRecentBlockStatus :: Spec
testDoGetRecentBlockStatus = describe "doGetRecentBlockStatus" $ do
    it "last finalized" $ getStatus (getHash lastFin) $ RecentBlock $ BlockFinalized lastFin
    it "live" $ getStatus (getHash testB) $ RecentBlock $ BlockAlive testB
    it "focus block" $ getStatus (getHash focusB) $ RecentBlock $ BlockAlive focusB
    it "pending block" $ getStatus (getHash pendingB) $ RecentBlock $ BlockPending pendingB
    it "dead block" $ getStatus deadH $ RecentBlock BlockDead
    it "genesis block" $ getStatus (getHash genB) OldFinalized
    it "unknown block" $ getStatus unknownH Unknown
  where
    getStatus bh expect = do
        s <- runTestLLDB (lldbWithGenesis @'P6) $ doGetRecentBlockStatus bh sd
        s `shouldBe` expect
    sd = skovDataWithTestBlocks

testDoMakeLiveBlock :: Spec
testDoMakeLiveBlock = it "doMakeLiveBlock" $ do
    let arrTime = timestampToUTCTime 5
        hgt = 23
    let (res, sd) = runState (doMakeLiveBlock pendingB dummyBlockState hgt arrTime) skovDataWithTestBlocks
    res
        `shouldBe` BlockPointer
            { bpState = dummyBlockState,
              bpInfo =
                BlockMetadata
                    { bmReceiveTime = pbReceiveTime pendingB,
                      bmHeight = 23,
                      bmArriveTime = arrTime
                    },
              bpBlock = NormalBlock (pbBlock pendingB)
            }
    (sd ^. blockTable . liveMap . at (getHash pendingB))
        `shouldBe` Just (MemBlockAlive res)

testDoMarkBlockDead :: Spec
testDoMarkBlockDead = describe "doMarkBlockDead" $ do
    it "live" $ mbd (getHash testB)
    it "focus block" $ mbd (getHash focusB)
    it "pending block" $ mbd (getHash pendingB)
    it "dead block" $ mbd deadH
    it "unknown block" $ mbd unknownH
  where
    mbd h = do
        let ((), sd) = runState (doMarkBlockDead h) skovDataWithTestBlocks
        assertEqual "block should not be in block table" Nothing (sd ^. blockTable . liveMap . at h)
        assertBool "block should be in the dead cache" $
            sd ^. blockTable . deadBlocks . to (memberDeadCache h)

testDoMarkPending :: Spec
testDoMarkPending = it "doMarkPending" $ do
    let pb = dummyPendingBlock (BlockHash minBound) 37
    let ((), sd) = runState (doMarkPending pb) skovDataWithTestBlocks
    assertEqual
        "block should be pending in block table"
        (Just (MemBlockPending pb))
        (sd ^. blockTable . liveMap . at (getHash pb))

testDoAddPendingBlock :: Spec
testDoAddPendingBlock = it "doAddPendingBlock" $ do
    let sd0 = dummyInitialSkovData
    let ((), sd1) = runState (doAddPendingBlock pb0) sd0
    assertEqual
        "pending block queue"
        (MPQ.fromList [(40, (getHash pb0, h0))])
        (sd1 ^. pendingBlocksQueue)
    assertEqual
        "pending block table"
        (HM.fromList [(h0, [pb0])])
        (sd1 ^. pendingBlocksTable)
    let ((), sd2) = runState (doAddPendingBlock pb1) sd1
    assertEqual
        "pending block queue"
        (MPQ.fromList [(40, (getHash pb0, h0)), (41, (getHash pb1, h0))])
        (sd2 ^. pendingBlocksQueue)
    assertEqual
        "pending block table"
        (HM.fromList [(h0, [pb1, pb0])])
        (sd2 ^. pendingBlocksTable)
    let ((), sd3) = runState (doAddPendingBlock pb2) sd2
    assertEqual
        "pending block queue"
        (MPQ.fromList [(40, (getHash pb0, h0)), (41, (getHash pb1, h0)), (42, (getHash pb2, getHash pb0))])
        (sd3 ^. pendingBlocksQueue)
    assertEqual
        "pending block table"
        (HM.fromList [(h0, [pb1, pb0]), (getHash pb0, [pb2])])
        (sd3 ^. pendingBlocksTable)
  where
    h0 = BlockHash minBound
    pb0 = dummyPendingBlock h0 40
    pb1 = dummyPendingBlock h0 41
    pb2 = dummyPendingBlock (getHash pb0) 42

testDoTakePendingChildren :: Spec
testDoTakePendingChildren = it "doTakePendingChildren" $ do
    let (l, sd1) = runState (doTakePendingChildren h0) sd0
    assertEqual
        "pending children"
        [pb1, pb0]
        l
    assertEqual
        "pending block table"
        (HM.fromList [(getHash pb0, [pb2])])
        (sd1 ^. pendingBlocksTable)
    assertEqual "pending block queue" (sd0 ^. pendingBlocksQueue) (sd1 ^. pendingBlocksQueue)
    let (l', sd1') = runState (doTakePendingChildren (getHash pb0)) sd0
    assertEqual
        "pending children"
        [pb2]
        l'
    assertEqual
        "pending block table"
        (HM.fromList [(h0, [pb1, pb0])])
        (sd1' ^. pendingBlocksTable)
    assertEqual "pending block queue" (sd0 ^. pendingBlocksQueue) (sd1' ^. pendingBlocksQueue)
    let (l'', sd1'') = runState (doTakePendingChildren dummyGenesisBlockHash) sd0
    assertEqual "pending children" [] l''
    assertEqual "pending block table" (sd0 ^. pendingBlocksTable) (sd1'' ^. pendingBlocksTable)
    assertEqual "pending block queue" (sd0 ^. pendingBlocksQueue) (sd1'' ^. pendingBlocksQueue)
  where
    h0 = BlockHash minBound
    pb0 = dummyPendingBlock h0 40
    pb1 = dummyPendingBlock h0 41
    pb2 = dummyPendingBlock (getHash pb0) 42
    sd0 =
        dummyInitialSkovData
            & pendingBlocksQueue
                .~ MPQ.fromList
                    [ (40, (getHash pb0, h0)),
                      (41, (getHash pb1, h0)),
                      (42, (getHash pb2, getHash pb0))
                    ]
            & pendingBlocksTable
                .~ HM.fromList
                    [ (h0, [pb1, pb0]),
                      (getHash pb0, [pb2])
                    ]

testDoTakeNextPendingUntil :: Spec
testDoTakeNextPendingUntil = it "doTakeNextPendingUntil" $ do
    let (mpb1, pending1) = runState (doTakeNextPendingUntil 40) pending0
    assertEqual "get to round 40" Nothing mpb1
    assertEqual
        "pending block table after get to round 40"
        (_pendingBlocksTable pending0)
        (_pendingBlocksTable pending1)
    assertEqual
        "pending block queue after get to round 40"
        (MPQ.fromList [(41, (getHash pb1, h0)), (42, (getHash pb2, getHash pb0))])
        (_pendingBlocksQueue pending1)
    let (mpb2, pending2) = runState (doTakeNextPendingUntil 42) pending0
    assertEqual "get to round 42" (Just pb1) mpb2
    assertEqual
        "pending block table after get to round 42"
        (HM.fromList [(getHash pb0, [pb2])])
        (_pendingBlocksTable pending2)
    assertEqual
        "pending block queue after get to round 42"
        (MPQ.fromList [(42, (getHash pb2, getHash pb0))])
        (_pendingBlocksQueue pending2)
    let (mpb3, pending3) = runState (doTakeNextPendingUntil 42) pending2
    assertEqual "get to round 42 twice" (Just pb2) mpb3
    assertEqual
        "pending block table after get to round 42 twice"
        HM.empty
        (_pendingBlocksTable pending3)
    assertEqual
        "pending block queue after get to round 42 twice"
        MPQ.empty
        (_pendingBlocksQueue pending3)
  where
    h0 = BlockHash minBound
    pb0 = dummyPendingBlock h0 40
    pb1 = dummyPendingBlock h0 41
    pb2 = dummyPendingBlock (getHash pb0) 42
    -- Note: pb0 is present in the queue, but not in the table.
    pending0 =
        PendingBlocks
            { _pendingBlocksQueue =
                MPQ.fromList
                    [ (40, (getHash pb0, h0)),
                      (41, (getHash pb1, h0)),
                      (42, (getHash pb2, getHash pb0))
                    ],
              _pendingBlocksTable =
                HM.fromList
                    [ (h0, [pb1]),
                      (getHash pb0, [pb2])
                    ]
            }

dummySigSchemeKeys :: SigScheme.KeyPair
{-# NOINLINE dummySigSchemeKeys #-}
dummySigSchemeKeys = unsafePerformIO $ SigScheme.newKeyPair SigScheme.Ed25519

dummyTransactionSignature :: TransactionSignature
dummyTransactionSignature = TransactionSignature $ Map.singleton 0 (Map.singleton 0 sig)
  where
    sig = SigScheme.sign dummySigSchemeKeys "transaction"

dummyAccountAddressN :: Int -> AccountAddress
dummyAccountAddressN = fst . randomAccountAddress . mkStdGen

dummyAccountAddress :: AccountAddress
dummyAccountAddress = dummyAccountAddressN 0

dummyTransactionWithPayload :: Nonce -> EncodedPayload -> Transaction
dummyTransactionWithPayload n payload =
    addMetadata NormalTransaction 0 $
        makeAccountTransaction
            dummyTransactionSignature
            hdr
            payload
  where
    hdr =
        TransactionHeader
            { thSender = dummyAccountAddress,
              thPayloadSize = payloadSize payload,
              thNonce = n,
              thExpiry = 500,
              thEnergyAmount = 5_000_000
            }

dummyTransaction :: Nonce -> Transaction
dummyTransaction n = dummyTransactionWithPayload n (EncodedPayload "01234567890123456789")

dummyTransactionBI :: Nonce -> BlockItem
dummyTransactionBI = normalTransaction . dummyTransaction

dummySuccessTransactionResult :: Nonce -> TVer.VerificationResult
dummySuccessTransactionResult n =
    TVer.Ok
        TVer.NormalTransactionSuccess
            { keysHash = Hash.hash "keys",
              nonce = n
            }

dummySuccessCredentialDeployment :: TVer.VerificationResult
dummySuccessCredentialDeployment = TVer.Ok TVer.CredentialDeploymentSuccess

dummyRawUpdateInstruction :: UpdateSequenceNumber -> RawUpdateInstruction
dummyRawUpdateInstruction usn =
    RawUpdateInstruction
        { ruiSeqNumber = usn,
          ruiEffectiveTime = 0,
          ruiTimeout = 0,
          ruiPayload = MicroGTUPerEuroUpdatePayload 2
        }

dummyUpdateInstruction :: UpdateSequenceNumber -> UpdateInstruction
dummyUpdateInstruction usn =
    makeUpdateInstruction
        (dummyRawUpdateInstruction usn)
        (Map.fromList [(0, dummySigSchemeKeys)])

dummyUpdateInstructionWM :: UpdateSequenceNumber -> WithMetadata UpdateInstruction
dummyUpdateInstructionWM usn =
    addMetadata ChainUpdate 0 $ dummyUpdateInstruction usn

dummyChainUpdate :: UpdateSequenceNumber -> BlockItem
dummyChainUpdate usn = chainUpdate $ dummyUpdateInstructionWM usn

-- |A valid 'AccountCreation' with expiry 1596409020
dummyAccountCreation :: AccountCreation
dummyAccountCreation = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-credential.json" >>= embedFile)

credentialDeploymentWM :: WithMetadata AccountCreation
credentialDeploymentWM = addMetadata CredentialDeployment 0 dummyAccountCreation

dummyCredentialDeployment :: BlockItem
dummyCredentialDeployment = credentialDeployment credentialDeploymentWM

testDoLookupLiveTransaction :: Spec
testDoLookupLiveTransaction = describe "doLookupLiveTransaction" $ do
    it "present" $ do
        assertEqual
            "status transaction 1"
            (Just $ Received 0 (dummySuccessTransactionResult 1))
            $ doLookupLiveTransaction (th 1) sd
        assertEqual
            "status transaction 2"
            (Just $ Received 0 (dummySuccessTransactionResult 2))
            $ doLookupLiveTransaction (th 2) sd
        assertEqual
            "status transaction 3"
            (Just $ Received 0 (dummySuccessTransactionResult 3))
            $ doLookupLiveTransaction (th 3) sd
    it "absent"
        $ assertEqual
            "status transaction 4"
            Nothing
        $ doLookupLiveTransaction (th 4) sd
  where
    th n = getHash (dummyTransactionBI n)
    addTrans n = snd . addTransaction (dummyTransactionBI n) 0 (dummySuccessTransactionResult n)
    sd =
        dummyInitialSkovData
            & transactionTable
                %~ addTrans 1
                . addTrans 2
                . addTrans 3

testDoLookupTransaction :: Spec
testDoLookupTransaction = describe "doLookupTransaction" $ do
    it "finalized" $ lu (th 1) (Just $ Finalized $ FinalizedTransactionStatus 1 0)
    it "live" $ lu (th 2) (Just $ Live $ Received 0 $ dummySuccessTransactionResult 2)
    it "absent" $ lu (th 5) Nothing
  where
    th n = getHash (dummyTransactionBI n)
    addTrans n = snd . addTransaction (dummyTransactionBI n) 0 (dummySuccessTransactionResult n)
    sd =
        dummyInitialSkovData
            & transactionTable
                %~ addTrans 2
                . addTrans 3
    db =
        (lldbWithGenesis @'P6)
            { lldbTransactions = HM.fromList [(th 1, FinalizedTransactionStatus 1 0)]
            }
    lu hsh expect = do
        s <- runTestLLDB db $ doLookupTransaction hsh sd
        s `shouldBe` expect

testDoGetNonFinalizedAccountTransactions :: Spec
testDoGetNonFinalizedAccountTransactions = describe "doGetNonFinalizedAccountTransactions" $ do
    it "present" $ do
        assertEqual
            "transactions for dummy account 0 from 1"
            [ (2, Map.singleton (dummyTransaction 2) (dummySuccessTransactionResult 2)),
              (3, Map.singleton (dummyTransaction 3) (dummySuccessTransactionResult 3))
            ]
            $ doGetNonFinalizedAccountTransactions
                (accountAddressEmbed dummyAccountAddress)
                1
                sd
        assertEqual
            "transactions for dummy account 0 from 3"
            [ (3, Map.singleton (dummyTransaction 3) (dummySuccessTransactionResult 3))
            ]
            $ doGetNonFinalizedAccountTransactions
                (accountAddressEmbed dummyAccountAddress)
                3
                sd
    it "absent"
        $ assertEqual
            "transactions for dummy account 1"
            []
        $ doGetNonFinalizedAccountTransactions
            (accountAddressEmbed (dummyAccountAddressN 1))
            1
            sd
  where
    addTrans n = snd . addTransaction (dummyTransactionBI n) 0 (dummySuccessTransactionResult n)
    sd =
        dummyInitialSkovData
            & transactionTable
                %~ addTrans 2
                . addTrans 3

testDoGetNonFinalizedChainUpdates ::
    Spec
testDoGetNonFinalizedChainUpdates = describe "doGetNonFinalizedChainUpdates" $ do
    it "present" $ do
        assertEqual
            "chain updates for UpdateMicroGTUPerEuro are present"
            [ (2, Map.insert (dummyUpdateInstructionWM 2) (dummySuccessTransactionResult 2) Map.empty),
              (3, Map.insert (dummyUpdateInstructionWM 3) (dummySuccessTransactionResult 3) Map.empty)
            ]
            $ doGetNonFinalizedChainUpdates UpdateMicroGTUPerEuro 1 sd
        assertEqual
            "one chain update for UpdateMicroGTUPerEuro are present from usn 3"
            [(3, Map.insert (dummyUpdateInstructionWM 3) (dummySuccessTransactionResult 3) Map.empty)]
            $ doGetNonFinalizedChainUpdates UpdateMicroGTUPerEuro 3 sd
    it "absent" $ do
        assertEqual
            "no chain updates for ProtocolUpdate are present"
            []
            $ doGetNonFinalizedChainUpdates UpdateProtocol 1 sd
  where
    addChainUpdate n = snd . addTransaction (dummyChainUpdate n) 0 (dummySuccessTransactionResult n)
    sd =
        dummyInitialSkovData
            & transactionTable
                %~ addChainUpdate 2
                . addChainUpdate 3

testDoGetNonFinalizedCredential :: Spec
testDoGetNonFinalizedCredential = describe "doGetNonFinalizedCredential" $ do
    it "present" $ do
        assertEqual
            "non-finalized credential deployment is present"
            (Just (credentialDeploymentWM, dummySuccessCredentialDeployment))
            $ doGetNonFinalizedCredential credDeploymentHash sd
    it "absent" $ do
        assertEqual "non-finalized credential deployment is absent" Nothing $ doGetNonFinalizedCredential nonExistingHash sd
  where
    addCredential = snd . addTransaction dummyCredentialDeployment 0 dummySuccessCredentialDeployment
    credDeploymentHash = getHash dummyCredentialDeployment
    nonExistingHash :: TransactionHash
    nonExistingHash = TransactionHashV0 $! Hash.hash $! Hash.hashToByteString $! v0TransactionHash credDeploymentHash
    sd =
        dummyInitialSkovData
            & transactionTable
                %~ addCredential
                . addCredential

testDoGetNextAccountNonce :: Spec
testDoGetNextAccountNonce = describe "doGetNextAccountNonce" $ do
    it "with non-finalized" $
        doGetNextAccountNonce (accountAddressEmbed dummyAccountAddress) sd
            `shouldBe` (4, False)
    it "with no transactions" $
        doGetNextAccountNonce (accountAddressEmbed (dummyAccountAddressN 1)) sd
            `shouldBe` (minNonce, True)
    it "with finalized transactions" $
        doGetNextAccountNonce (accountAddressEmbed (dummyAccountAddressN 2)) sd
            `shouldBe` (7, True)
  where
    addTrans n = snd . addTransaction (dummyTransactionBI n) 0 (dummySuccessTransactionResult n)
    sd =
        dummyInitialSkovData
            & transactionTable
                %~ addTrans 2
                . addTrans 3
                . ( ttNonFinalizedTransactions . at (accountAddressEmbed (dummyAccountAddressN 2))
                        ?~ emptyANFTWithNonce 7
                  )

testDoFinalizeTransactions :: Spec
testDoFinalizeTransactions = describe "doFinalizeTransactions" $ do
    it "normal transactions" $ do
        sd' <- execStateT (doFinalizeTransactions [normalTransaction tr0]) sd
        assertEqual
            "Account non-finalized transactions"
            (Just AccountNonFinalizedTransactions{_anftNextNonce = 2, _anftMap = Map.singleton 2 (Map.singleton tr1 (dummySuccessTransactionResult 2))})
            (sd' ^. transactionTable . ttNonFinalizedTransactions . at sender)
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr1, (normalTransaction tr1, Received 0 (dummySuccessTransactionResult 2)))])
            (sd' ^. transactionTable . ttHashMap)
    it "chain updates" $ do
        sd' <- execStateT (doFinalizeTransactions [chainUpdate cu0]) sd1
        assertEqual
            "Chain update non-finalized transactions"
            (Just NonFinalizedChainUpdates{_nfcuNextSequenceNumber = 2, _nfcuMap = Map.singleton 2 (Map.singleton cu1 (dummySuccessTransactionResult 2))})
            (sd' ^. transactionTable . ttNonFinalizedChainUpdates . at UpdateMicroGTUPerEuro)
        assertEqual
            "transaction hash map"
            ( HM.fromList
                [ (getHash cu1, (chainUpdate cu1, Received 0 (dummySuccessTransactionResult 2))),
                  (getHash tr1, (normalTransaction tr1, Received 0 (dummySuccessTransactionResult 2)))
                ]
            )
            (sd' ^. transactionTable . ttHashMap)
    it "credential deployments" $ do
        sd' <- execStateT (doFinalizeTransactions [credentialDeployment cred0]) sd2
        assertEqual
            "Non-finalized credential deployments"
            (sd' ^. transactionTable . ttHashMap . at credDeploymentHash)
            Nothing
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr1, (normalTransaction tr1, Received 0 (dummySuccessTransactionResult 2)))])
            (sd' ^. transactionTable . ttHashMap)
  where
    sender = accountAddressEmbed dummyAccountAddress
    tr0 = dummyTransaction 1
    tr1 = dummyTransaction 2
    tr2 = dummyTransactionWithPayload 1 (EncodedPayload "a")
    cu0 = dummyUpdateInstructionWM 1
    cu1 = dummyUpdateInstructionWM 2
    cred0 = credentialDeploymentWM
    addTrans t = snd . addTransaction (normalTransaction t) 0 (dummySuccessTransactionResult (transactionNonce t))
    addChainUpdate u = snd . addTransaction (chainUpdate u) 0 (dummySuccessTransactionResult (updateSeqNumber $ uiHeader $ wmdData u))
    addCredential = snd . addTransaction dummyCredentialDeployment 0 dummySuccessCredentialDeployment
    credDeploymentHash = getHash dummyCredentialDeployment
    sd =
        dummyInitialSkovData
            & transactionTable
                %~ addTrans tr0
                . addTrans tr1
                . addTrans tr2
    sd1 =
        dummyInitialSkovData
            & transactionTable
                %~ addChainUpdate cu0
                . addChainUpdate cu1
                . addTrans tr1
    sd2 =
        dummyInitialSkovData
            & transactionTable
                %~ addCredential
                . addTrans tr1

testDoAddTransaction :: Spec
testDoAddTransaction = describe "doAddTransaction" $ do
    it "add transaction" $ do
        sd' <- execStateT (doAddTransaction tr0Round (normalTransaction tr0) (dummySuccessTransactionResult 1)) dummyInitialSkovData
        assertEqual
            "Account non-finalized transactions"
            (Just AccountNonFinalizedTransactions{_anftNextNonce = 1, _anftMap = Map.singleton 1 (Map.singleton tr0 (dummySuccessTransactionResult 1))})
            (sd' ^. transactionTable . ttNonFinalizedTransactions . at sender)
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr0, (normalTransaction tr0, Received (commitPoint tr0Round) (dummySuccessTransactionResult 1)))])
            (sd' ^. transactionTable . ttHashMap)
        assertEqual
            "transaction table purge counter is incremented"
            (1 + dummyInitialSkovData ^. transactionTablePurgeCounter)
            (sd' ^. transactionTablePurgeCounter)
        sd'' <- execStateT (doFinalizeTransactions [normalTransaction tr0]) sd'
        added <- evalStateT (doAddTransaction tr0Round (normalTransaction tr0) (dummySuccessTransactionResult 1)) sd''
        assertEqual "tx should not be added" False added
  where
    tr0Round = 1
    tr0 = dummyTransaction 1
    sender = accountAddressEmbed dummyAccountAddress

testDoCommitTransaction :: Spec
testDoCommitTransaction = describe "doCommitTransaction" $ do
    it "commit transaction" $ do
        sd' <- execStateT (doCommitTransaction 1 bh 0 (normalTransaction tr0)) sd
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr0, (normalTransaction tr0, Committed 1 (dummySuccessTransactionResult (transactionNonce tr0)) $ HM.fromList [(bh, TransactionIndex 0)]))])
            (sd' ^. transactionTable . ttHashMap)
  where
    tr0 = dummyTransaction 1
    addTrans t = snd . addTransaction (normalTransaction t) 0 (dummySuccessTransactionResult (transactionNonce t))
    sd =
        dummyInitialSkovData
            & transactionTable
                %~ addTrans tr0
    bh = BlockHash minBound

testDoMarkTransactionDead :: Spec
testDoMarkTransactionDead = describe "doMarkTransactionDead" $ do
    it "mark committed transaction dead" $ do
        sd' <- execStateT (doCommitTransaction 1 bh 0 (normalTransaction tr0)) sd
        sd'' <- execStateT (doMarkTransactionDead bh (normalTransaction tr0)) sd'
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr0, (normalTransaction tr0, Received 1 (dummySuccessTransactionResult (transactionNonce tr0))))])
            (sd'' ^. transactionTable . ttHashMap)
    it "mark received transaction dead" $ do
        sd' <- execStateT (doMarkTransactionDead bh (normalTransaction tr0)) sd
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr0, (normalTransaction tr0, Received 0 (dummySuccessTransactionResult (transactionNonce tr0))))])
            (sd' ^. transactionTable . ttHashMap)
  where
    tr0 = dummyTransaction 1
    addTrans t = snd . addTransaction (normalTransaction t) 0 (dummySuccessTransactionResult (transactionNonce t))
    sd =
        dummyInitialSkovData
            & transactionTable
                %~ addTrans tr0
    bh = BlockHash minBound

testDoPurgeTransactionTable :: Spec
testDoPurgeTransactionTable = describe "doPurgeTransactionTable" $ do
    it "force purge the transaction table" $ do
        -- increment the purge counter.
        sd' <- execStateT (doAddTransaction 0 (normalTransaction tr0) (dummySuccessTransactionResult 1)) sd
        sd'' <- execStateT (doPurgeTransactionTable True theTime) sd'
        assertEqual
            "purge counter should be reset"
            0
            (sd'' ^. transactionTablePurgeCounter)
        assertEqual
            "Account non-finalized transactions"
            (Just $ AccountNonFinalizedTransactions{_anftMap = Map.empty, _anftNextNonce = 1})
            (sd'' ^. transactionTable . ttNonFinalizedTransactions . at sender)
        assertEqual
            "Chain update non-finalized transactions"
            (Just $ NonFinalizedChainUpdates{_nfcuMap = Map.empty, _nfcuNextSequenceNumber = 1})
            (sd'' ^. transactionTable . ttNonFinalizedChainUpdates . at UpdateMicroGTUPerEuro)
        assertEqual
            "Non-finalized credential deployments"
            Nothing
            (sd'' ^. transactionTable . ttHashMap . at credDeploymentHash)
  where
    addChainUpdate u = snd . addTransaction (chainUpdate u) 0 (dummySuccessTransactionResult (updateSeqNumber $ uiHeader $ wmdData u))
    addCredential = snd . addTransaction dummyCredentialDeployment 0 dummySuccessCredentialDeployment
    tr0 = dummyTransaction 1
    cu0 = dummyUpdateInstructionWM 1
    theTime = timestampToUTCTime $! Timestamp 1596409021
    sender = accountAddressEmbed dummyAccountAddress
    credDeploymentHash = getHash dummyCredentialDeployment
    -- Set the round for the last finalized block pointer.
    sd =
        dummyInitialSkovData
            & transactionTable
                %~ addChainUpdate cu0
                . addCredential
            & pendingTransactionTable
                %~ addPendingDeployCredential credDeploymentHash

testDoClearOnProtocolUpdate :: Spec
testDoClearOnProtocolUpdate = describe "doClearOnProtocolUpdate" $
    it "clears on protocol update" $ do
        sd' <- execStateT (doCommitTransaction 1 bh 0 (normalTransaction tr0)) sd
        sd'' <- execStateT doClearOnProtocolUpdate sd'
        assertEqual
            "pending block table should be empty"
            HM.empty
            (sd'' ^. pendingBlocksTable)
        assertEqual
            "block table should be empty"
            emptyBlockTable
            (sd'' ^. blockTable)
        assertEqual
            "Branches should be empty"
            Seq.empty
            (sd'' ^. branches)
        assertEqual
            "Branches should be empty"
            Seq.empty
            (sd'' ^. branches)
        assertEqual
            "committed transactions should be received"
            (HM.fromList [(getHash tr0, (normalTransaction tr0, Received 1 (dummySuccessTransactionResult 1)))])
            (sd'' ^. transactionTable . ttHashMap)
  where
    tr0 = dummyTransaction 1
    bh = BlockHash minBound
    addTrans t = snd . addTransaction (normalTransaction t) 0 (dummySuccessTransactionResult (transactionNonce t))
    sd =
        skovDataWithTestBlocks
            & transactionTable
                %~ addTrans tr0

tests :: Spec
tests = describe "KonsensusV1.TreeState" $ do
    describe "BlockTable" $ do
        testDoGetMemoryBlockStatus
        testDoGetBlockStatus
        testDoGetRecentBlockStatus
        testDoMakeLiveBlock
        testDoMarkBlockDead
        testDoMarkPending
    describe "PendingBlockTable" $ do
        testDoAddPendingBlock
        testDoTakePendingChildren
        testDoTakeNextPendingUntil
    describe "TransactionTable" $ do
        testDoLookupLiveTransaction
        testDoLookupTransaction
        testDoGetNonFinalizedAccountTransactions
        testDoGetNonFinalizedChainUpdates
        testDoGetNonFinalizedCredential
        testDoGetNextAccountNonce
        testDoFinalizeTransactions
        testDoAddTransaction
        testDoCommitTransaction
        testDoMarkTransactionDead
        testDoPurgeTransactionTable
    describe "Clear on protocol update" $ do
        testDoClearOnProtocolUpdate
