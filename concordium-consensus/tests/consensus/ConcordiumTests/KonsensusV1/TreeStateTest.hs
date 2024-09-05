{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module tests the 'Concordium.KonsensusV1.TreeState.Implementation' module
--  which materializes the tree state for consensus protocol v1 (PV6).
--
--  In particular the following functions exposed in the above mentioned module are tested:
--
--  BlockTable
--  A structure that records live blocks and blocks which are marked _dead_ in
--  the tree state.
--
--  * 'getMemoryBlockStatus'
--  * 'getBlockStatus'
--  * 'getRecentBlockStatus'
--  * 'makeLiveBlock'
--  * 'markBlockDead'
--  * 'markPending'
--
--  PendingBlockTable
--  A structure that records pending blocks stored in the tree state.
--  When a pending block becomes live i.e. it has been executed and his head of the
--  chain then it will be added to the block table.
--
--  * 'takePendingBlock'
--  * 'takePendingChildren'
--  * 'takePendingChildrenUntil'
--
--  TransactionTable
--  A structure that records transactions in the tree state.
--  From a consensus perspective, then transactions can be added to
--  the tree state either individually (i.e. a single transaction sent to the
--  the consensus layer) or via a block.
--
--  * 'lookupLiveTransaction'
--  * 'lookupTransaction'
--  * 'getNonFinalizedAccountTransactions'
--  * 'getNonFinalizedChainUpdates'
--  * 'getNonFinalizedCredential'
--  * 'getNextAccountNonce'
--  * 'finalizeTransactions'
--  * 'addTransaction'
--  * 'commitTransaction'
--  * 'markTransactionDead'
--  * 'purgeTransactionTable'
--
--  Protocol update
--  Functions related to when a protocol update is
--  initiated.
--
--  * 'clearOnProtocolUpdate'
--
--  Refer to each individual test for a more detailed description of what each individual test
--  verifies.
module ConcordiumTests.KonsensusV1.TreeStateTest (
    tests,
    dummyBlock,
    dummyGenesisBlockHash,
    dummyInitialSkovData,
    lldbWithGenesis,
    toStoredBlock,
    runTestLLDB,
) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Ratio
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import Data.Word ()
import Lens.Micro.Platform
import System.Random
import Test.HUnit
import Test.Hspec

-- base types.
import Concordium.Crypto.DummyData
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.Genesis.Account
import Concordium.Genesis.Data.BaseV1
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.Scheduler.DummyData
import Concordium.Types
import qualified Concordium.Types.Conditionally as Cond
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.Parameters
import Concordium.Types.TransactionOutcomes
import Concordium.Types.Transactions

-- konsensus v1 related imports.
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Parameters (defaultRuntimeParameters)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.TreeState (insertDeadCache, memberDeadCache)
import qualified Concordium.GlobalState.TransactionTable as TT
import Concordium.ID.Types
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.LowLevel.Memory
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types.Option
import Concordium.Types.Updates

import qualified ConcordiumTests.KonsensusV1.Common as Common
import qualified ConcordiumTests.KonsensusV1.TransactionProcessingTest as Helper

-- We derive these instances here so we don't accidentally end up using them in production.
-- We have them because they are very convenient for testing purposes.
deriving instance Eq (BlockStatus pv)
deriving instance Eq (BlockTable pv)
deriving instance Eq (RecentBlockStatus pv)

dummyBlockResultHash :: BlockResultHash
dummyBlockResultHash = BlockResultHash $ Hash.hash "DummyBlockResult"

dummySignKeys :: BakerSignPrivateKey
dummySignKeys = fst $ randomBlockKeyPair $ mkStdGen 42

-- | An empty block where the parent is indicated
--  by the provided 'parentHash' and the 'Round' of the block
--  by provided 'Round'
dummyBakedBlock ::
    forall pv.
    (IsProtocolVersion pv) =>
    -- | 'BlockHash' of the parent.
    BlockHash ->
    -- | The round of the block
    Round ->
    -- | The timestamp of the block
    Timestamp ->
    -- | The empty baked block
    BakedBlock pv
dummyBakedBlock parentHash bbRound bbTimestamp = BakedBlock{..}
  where
    bbEpoch = 0
    bbBaker = 0
    bbQuorumCertificate = Common.dummyQuorumCertificate parentHash
    bbTimeoutCertificate = Absent
    bbEpochFinalizationEntry = Absent
    bbNonce = Common.dummyBlockNonce
    bbTransactions = mempty
    bbDerivableHashes = case sBlockHashVersionFor (protocolVersion @pv) of
        SBlockHashVersion0 ->
            DerivableBlockHashesV0
                { dbhv0TransactionOutcomesHash = TransactionOutcomesHash minBound,
                  dbhv0BlockStateHash = Common.dummyStateHash
                }
        SBlockHashVersion1 ->
            DerivableBlockHashesV1
                { dbhv1BlockResultHash = dummyBlockResultHash
                }

-- | Create a 'SignedBlock' by signing the
--  'dummyBakedBlock' with 'dummySignKeys'
dummySignedBlock ::
    (IsProtocolVersion pv) =>
    -- | 'BlockHash' of the parent
    BlockHash ->
    -- | 'Round' of the block
    Round ->
    -- | Timestamp of the block
    Timestamp ->
    -- | The signed block
    SignedBlock pv
dummySignedBlock parentHash rnd = signBlock dummySignKeys dummyGenesisBlockHash . dummyBakedBlock parentHash rnd

-- | Construct a 'PendingBlock' for the provided 'Round' where the
--  parent is indicated by the provided 'BlockHash'.
dummyPendingBlock ::
    (IsProtocolVersion pv) =>
    -- | Parent 'BlockHash'
    BlockHash ->
    -- | The 'Timestamp' of the block
    Timestamp ->
    -- | The resulting 'PendingBlock'
    PendingBlock pv
dummyPendingBlock parentHash ts =
    PendingBlock
        { pbBlock = dummySignedBlock parentHash 1 ts,
          pbReceiveTime = timestampToUTCTime 0
        }

-- | A 'BlockPointer' referrring to the 'dummySignedBlock' for the provided 'Round'
dummyBlock ::
    forall pv.
    (IsProtocolVersion pv) =>
    -- | 'Round' of the block that the created 'BlockPointer' should point to.
    Round ->
    -- | The 'BlockPointer'
    BlockPointer pv
dummyBlock rnd = BlockPointer{..}
  where
    bpInfo =
        BlockMetadata
            { bmHeight = fromIntegral rnd,
              bmReceiveTime = timestampToUTCTime 0,
              bmArriveTime = timestampToUTCTime 0,
              bmEnergyCost = 0,
              bmTransactionsSize = 0,
              bmBlockStateHash =
                Cond.conditionally
                    (sBlockStateHashInMetadata (sBlockHashVersionFor (protocolVersion @pv)))
                    Common.dummyStateHash
            }
    bpBlock = NormalBlock $ dummySignedBlock (BlockHash minBound) rnd 0
    bpState = Common.dummyBlockState

-- | A 'BlockHash' suitable for configuring
--  a 'GenesisMetadata' (see 'dummyGenesisMetadata') which is
--  used to create an initial 'SkovData pv' which is used for the
--  tests presented in this file.
--  The actual hash chosen plays no role for the tests carried out
--  in this module.
dummyGenesisBlockHash :: BlockHash
dummyGenesisBlockHash = BlockHash (Hash.hash "DummyGenesis")

-- | 'GenesisMetadata' configured with
--  the 'dummyGenesisBlockHash' and the 'dummyBlockState'.
--  The chosen 'CoreGenesisParametersV1' has no effect on the tests run
--  as part of this module.
dummyGenesisMetadata :: GenesisMetadata
dummyGenesisMetadata =
    GenesisMetadata
        { gmParameters =
            CoreGenesisParametersV1
                { genesisTime = 0,
                  genesisEpochDuration = 3_600_000,
                  genesisSignatureThreshold = 2 % 3
                },
          gmCurrentGenesisHash = dummyGenesisBlockHash,
          gmFirstGenesisHash = dummyGenesisBlockHash,
          gmStateHash = getHash Common.dummyBlockState
        }

-- | Dummy bakers and finalizers with no bakers or finalizers.
--  This is only suitable for when the value is not meaningfully used.
dummyBakersAndFinalizers :: BakersAndFinalizers
dummyBakersAndFinalizers =
    BakersAndFinalizers
        { _bfBakersEx = FullBakersEx Vec.empty 0,
          _bfFinalizers = finalizers,
          _bfFinalizerHash = computeFinalizationCommitteeHash finalizers
        }
  where
    finalizers = FinalizationCommittee Vec.empty 0

-- | Dummy epoch bakers. This is only suitable for when the actual value is not meaningfully used.
dummyEpochBakers :: EpochBakers
dummyEpochBakers = EpochBakers bf bf bf 1
  where
    bf = dummyBakersAndFinalizers

-- | An initial 'SkovData' suitable for testing.
--  The block state is empty ('dummyBlockState') only consisting of a
--  genesis block.
--
--  The outputted 'SkovData pv' is configured with the
--  'dummyGenesisMetada' (and so the 'dummyGenesisBlockHash') however for the
--  tests we are carrying out in this module it could be any genesis metadata
--
--  The initial 'RoundStatus' for the 'SkovData pv' is configured with
--  the initial timeout duration set to 10 seconds.
--  However this is just a dummy value and can be replaced with other values,
--  i.e. it has no effect on the tests being run with the 'dummyInitialSkovData'.
--
--  Note that as the 'SkovData pv' returned here is constructed by simple dummy values,
--  then is not suitable for carrying out block state queries or operations.
--  But this is fine as we do not require that from the tests here.
dummyInitialSkovData :: (IsConsensusV1 pv, IsProtocolVersion pv) => SkovData pv
dummyInitialSkovData =
    mkInitialSkovData
        defaultRuntimeParameters
        dummyGenesisMetadata
        GenesisBlockHeightInfo
            { gbhiAbsoluteHeight = 0,
              gbhiGenesisIndex = 0
            }
        Common.dummyBlockState
        10_000
        dummyEpochBakers
        TT.emptyTransactionTable
        TT.emptyPendingTransactionTable

-- | A 'LowLevelDB' for testing purposes.
newtype TestLLDB pv = TestLLDB {theTestLLDB :: IORef (LowLevelDB pv)}

-- | Deriving 'HasMemoryLLDB' for our 'TestLLDB' such that
--  we can run the 'MonadTreeStateStore' via the in memory
--  implementation.
instance HasMemoryLLDB pv (TestLLDB pv) where
    theMemoryLLDB = theTestLLDB

-- | Run the provided action @a@ on the provided 'LowLevelDB'.
runTestLLDB ::
    -- | The initial database
    LowLevelDB pv ->
    -- | The action
    MemoryLLDBM pv (ReaderT (TestLLDB pv) IO) b ->
    -- | The result of the action.
    IO b
runTestLLDB initDB a = do
    tdb <- TestLLDB <$> newIORef initDB
    runReaderT (runMemoryLLDBM a) tdb

-- Test values

genB :: (IsProtocolVersion pv) => BlockPointer pv
genB = dummyBlock 0
lastFin :: (IsProtocolVersion pv) => BlockPointer pv
lastFin = dummyBlock 20
testB :: (IsProtocolVersion pv) => BlockPointer pv
testB = dummyBlock 21
focusB :: (IsProtocolVersion pv) => BlockPointer pv
focusB = dummyBlock 22
pendingB :: (IsProtocolVersion pv) => PendingBlock pv
pendingB = dummyPendingBlock (BlockHash minBound) 33
deadH :: BlockHash
deadH = BlockHash (Hash.hash "DeadBlock")
unknownH :: BlockHash
unknownH = BlockHash (Hash.hash "Unknown")

-- | Convert a block pointer to a stored block, using `BlobRef 0` as the state pointer.
toStoredBlock :: BlockPointer pv -> LowLevel.StoredBlock pv
toStoredBlock BlockPointer{..} =
    LowLevel.StoredBlock
        { stbStatePointer = BlobRef 0,
          stbInfo = bpInfo,
          stbBlock = bpBlock
        }

-- | A 'SkovData' that corresponds to the
--  'dummyInitialSkovData' which have the following blocks added:
--
--  * 'testB' (alive)
--  * 'focusB' (parent is 'testB')
--  * the block indicated by 'deadH' has added to the dead cache.
skovDataWithTestBlocks :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => SkovData pv
skovDataWithTestBlocks =
    dummyInitialSkovData
        & lastFinalized .~ lastFin
        & focusBlock .~ focusB
        & blockTable
            %~ ( ( liveMap
                    %~ ( (at (getHash (testB @pv)) ?~ testB)
                            . (at (getHash (focusB @pv)) ?~ focusB)
                       )
                 )
                    . ( deadBlocks %~ insertDeadCache deadH
                      )
               )

-- | A test 'LowLevelDB' with the genesis block.
lldbWithGenesis :: (IsProtocolVersion pv) => LowLevelDB pv
lldbWithGenesis =
    initialLowLevelDB
        sb
        initialPersistentRoundStatus
  where
    sb = toStoredBlock genB

-- | Testing 'getMemoryBlockStatus' functionality.
--  In particular this test ensures that a (known) block in memory can
--  have its status retrieved.
testGetMemoryBlockStatus ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testGetMemoryBlockStatus _ =
    describe "getMemoryBlockStatus" $ do
        it "last finalized" $ getMemoryBlockStatus (getHash (lastFin @pv)) sd `shouldBe` Just (BlockFinalized lastFin)
        it "live" $ getMemoryBlockStatus (getHash (testB @pv)) sd `shouldBe` Just (BlockAlive testB)
        it "focus block" $ getMemoryBlockStatus (getHash (focusB @pv)) sd `shouldBe` Just (BlockAlive focusB)
        it "dead block" $ getMemoryBlockStatus deadH sd `shouldBe` Just BlockDead
        it "unknown block" $ getMemoryBlockStatus unknownH sd `shouldBe` Nothing
  where
    sd = skovDataWithTestBlocks @pv

-- | Testing 'getBlockStatus' functionality.
--  In particular this test ensures that a (known) block, transient or persistent
--  can have its status looked up.
testGetBlockStatus ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testGetBlockStatus _ =
    describe ": getBlockStatus" $ do
        it "last finalized" $ getStatus (getHash (lastFin @pv)) $ BlockFinalized lastFin
        it "live" $ getStatus (getHash (testB @pv)) $ BlockAlive testB
        it "focus block" $ getStatus (getHash (focusB @pv)) $ BlockAlive focusB
        it "dead block" $ getStatus deadH BlockDead
        it "genesis block" $ getStatus (getHash (genB @pv)) $ BlockFinalized genB
        it "unknown block" $ getStatus unknownH BlockUnknown
  where
    getStatus bh expect = do
        s <- runTestLLDB lldbWithGenesis $ getBlockStatus bh sd
        s `shouldBe` expect
    sd = skovDataWithTestBlocks @pv

-- | Testing 'getRecentBlockStatus' functionality.
--  In particular this test ensures that a (known) block in memory
--  can have its status looked up, or at least inform the caller
--  that it is a predecessor of the last finalized block ('OldFinalized').
testGetRecentBlockStatus ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testGetRecentBlockStatus _ = describe ": getRecentBlockStatus" $ do
    it "last finalized" $ getStatus (getHash (lastFin @pv)) $ RecentBlock $ BlockFinalized lastFin
    it "live" $ getStatus (getHash (testB @pv)) $ RecentBlock $ BlockAlive testB
    it "focus block" $ getStatus (getHash (focusB @pv)) $ RecentBlock $ BlockAlive focusB
    it "dead block" $ getStatus deadH $ RecentBlock BlockDead
    it "genesis block" $ getStatus (getHash (genB @pv)) OldFinalized
    it "unknown block" $ getStatus unknownH $ RecentBlock BlockUnknown
  where
    getStatus bh expect = do
        s <- runTestLLDB lldbWithGenesis $ getRecentBlockStatus bh sd
        s `shouldBe` expect
    sd = skovDataWithTestBlocks @pv

-- | Testing 'makeLiveBlock' function.
--  This ensures that a 'PendingBlock' can be marked live
--  and by doing so it becomes present in the map of live blocks
--  and a valid 'BlockPointer' to the block marked is returned.
testMakeLiveBlock ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testMakeLiveBlock sProtocolVersion = it "makeLiveBlock" $ do
    let arrTime = timestampToUTCTime 5
        hgt = 23
    let (res, sd) = runState (makeLiveBlock @_ @pv pendingB Common.dummyBlockState hgt arrTime 0) skovDataWithTestBlocks
    res
        `shouldBe` BlockPointer
            { bpState = Common.dummyBlockState,
              bpInfo =
                BlockMetadata
                    { bmReceiveTime = pbReceiveTime (pendingB @pv),
                      bmHeight = 23,
                      bmArriveTime = arrTime,
                      bmEnergyCost = 0,
                      bmTransactionsSize = 0,
                      bmBlockStateHash =
                        Cond.conditionally
                            (sBlockStateHashInMetadata (sBlockHashVersionFor sProtocolVersion))
                            Common.dummyStateHash
                    },
              bpBlock = NormalBlock (pbBlock pendingB)
            }
    (sd ^. blockTable . liveMap . at (getHash (pendingB @pv)))
        `shouldBe` Just res

-- | Testing 'markBlockDead' function.
--  This test ensures that whatever the state of a block referenced
--  by the provided 'BlockHash' is expunged from memory and that
--  it becomes part of the cache of dead blocks.
testMarkBlockDead ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testMarkBlockDead _ = describe "markBlockDead" $ do
    it "live" $ mbd (getHash (testB @pv))
    it "focus block" $ mbd (getHash (focusB @pv))
    it "pending block" $ mbd (getHash (pendingB @pv))
    it "dead block" $ mbd deadH
    it "unknown block" $ mbd unknownH
  where
    mbd h = do
        let ((), sd) = runState (markBlockDead h) (skovDataWithTestBlocks @pv)
        assertEqual "block should not be in block table" Nothing (sd ^. blockTable . liveMap . at h)
        assertBool "block should be in the dead cache" $
            sd ^. blockTable . deadBlocks . to (memberDeadCache h)

-- | An arbitrary chosen 'SigScheme.KeyPair'
--  suitable for testing purposes.
dummySigSchemeKeys :: SigScheme.KeyPair
dummySigSchemeKeys =
    let ((signKey, verifyKey), _) = randomEd25519KeyPair $ mkStdGen 42
    in  SigScheme.KeyPairEd25519{..}

dummyTransactionSignature :: TransactionSignature
dummyTransactionSignature = TransactionSignature $ Map.singleton 0 (Map.singleton 0 sig)
  where
    sig = SigScheme.sign dummySigSchemeKeys "transaction"

dummyAccountAddressN :: Int -> AccountAddress
dummyAccountAddressN = fst . randomAccountAddress . mkStdGen

dummyAccountAddress :: AccountAddress
dummyAccountAddress = dummyAccountAddressN 0

-- | A dummy normal transfer transaction suitable for the tests
--  in this file.
--  Note that the tests presented in this module
--  does no transaction processing i.e. verification of the transaction.
dummyTransaction' :: AccountAddress -> Nonce -> Transaction
dummyTransaction' accAddr n =
    addMetadata NormalTransaction 0 $
        makeAccountTransaction
            dummyTransactionSignature
            hdr
            payload
  where
    hdr =
        TransactionHeader
            { thSender = accAddr,
              thPayloadSize = payloadSize payload,
              thNonce = n,
              thExpiry = 500,
              thEnergyAmount = 5_000_000
            }
    payload = encodePayload $ Transfer dummyAccountAddress 10

dummyTransactionFromSender :: AccountAddress -> Nonce -> Transaction
dummyTransactionFromSender = dummyTransaction'

dummyTransactionBIFromSender :: AccountAddress -> Nonce -> BlockItem
dummyTransactionBIFromSender accAddr = normalTransaction . (dummyTransactionFromSender accAddr)

dummyTransaction :: Nonce -> Transaction
dummyTransaction = dummyTransaction' dummyAccountAddress

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

-- | A valid 'AccountCreation' with expiry 1596409020
dummyAccountCreation :: AccountCreation
dummyAccountCreation = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-credential.json" >>= embedFile)

credentialDeploymentWM :: WithMetadata AccountCreation
credentialDeploymentWM = addMetadata CredentialDeployment 0 dummyAccountCreation

dummyCredentialDeployment :: BlockItem
dummyCredentialDeployment = credentialDeployment credentialDeploymentWM

-- | Testing 'lookupLiveTransaction'
--  This test ensures that live transactions can be
--  looked up and that uknown ones cannot be.
testLookupLiveTransaction ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testLookupLiveTransaction _ = describe "lookupLiveTransaction" $ do
    it "present" $ do
        assertEqual
            "status transaction 1"
            (Just $ TT.Received 0 (dummySuccessTransactionResult 1))
            $ lookupLiveTransaction (txHash 1) sd
        assertEqual
            "status transaction 2"
            (Just $ TT.Received 0 (dummySuccessTransactionResult 2))
            $ lookupLiveTransaction (txHash 2) sd
        assertEqual
            "status transaction 3"
            (Just $ TT.Received 0 (dummySuccessTransactionResult 3))
            $ lookupLiveTransaction (txHash 3) sd
    it "absent"
        $ assertEqual
            "status transaction 4"
            Nothing
        $ lookupLiveTransaction (txHash 4) sd
  where
    txHash nonce = getHash (dummyTransactionBI nonce)
    addTrans nonce = snd . TT.addTransaction (dummyTransactionBI nonce) 0 (dummySuccessTransactionResult nonce)
    sd =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addTrans 1
                    . addTrans 2
                    . addTrans 3

-- | Testing 'lookupTransaction'
--  This test ensures that:
--  * A finalized transation can be looked up.
--  * A live transaction can be looked up.
--  * Looking up with an unknown transaction hash will result in a 'Nothing' result.
testLookupTransaction ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testLookupTransaction _ = describe "lookupTransaction" $ do
    it "finalized" $ lookupAndCheck (txHash 1) (Just $ Finalized $ FinalizedTransactionStatus 1 0)
    it "live" $ lookupAndCheck (txHash 2) (Just $ Live $ TT.Received 0 $ dummySuccessTransactionResult 2)
    it "absent" $ lookupAndCheck (txHash 5) Nothing
  where
    txHash nonce = getHash (dummyTransactionBI nonce)
    addTrans nonce = snd . TT.addTransaction (dummyTransactionBI nonce) 0 (dummySuccessTransactionResult nonce)
    sd =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addTrans 2
                    . addTrans 3
    db =
        (lldbWithGenesis @pv)
            { lldbTransactions = HM.fromList [(txHash 1, FinalizedTransactionStatus 1 0)]
            }
    lookupAndCheck hsh expectedOutcome = do
        lookupResult <- runTestLLDB db $ lookupTransaction hsh sd
        lookupResult `shouldBe` expectedOutcome

-- | Testing 'getNonFinalizedAccountTransactions'
--  This test ensures that:
--  * An existing non finalized account transaction can be looked up
--  * Looking up with an unknown transaction hash will result in a 'Nothing' result.
testGetNonFinalizedAccountTransactions ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testGetNonFinalizedAccountTransactions _ =
    describe "getNonFinalizedAccountTransactions" $ do
        it "present" $ do
            assertEqual
                "transactions for dummy account 0 from 1"
                [ (2, Map.singleton (dummyTransaction 2) (dummySuccessTransactionResult 2)),
                  (3, Map.singleton (dummyTransaction 3) (dummySuccessTransactionResult 3))
                ]
                $ getNonFinalizedAccountTransactions
                    (accountAddressEmbed dummyAccountAddress)
                    1
                    sd
            assertEqual
                "transactions for dummy account 0 from 3"
                [ (3, Map.singleton (dummyTransaction 3) (dummySuccessTransactionResult 3))
                ]
                $ getNonFinalizedAccountTransactions
                    (accountAddressEmbed dummyAccountAddress)
                    3
                    sd
        it "absent"
            $ assertEqual
                "transactions for dummy account 1"
                []
            $ getNonFinalizedAccountTransactions
                (accountAddressEmbed (dummyAccountAddressN 1))
                1
                sd
  where
    addTrans n = snd . TT.addTransaction (dummyTransactionBI n) 0 (dummySuccessTransactionResult n)
    sd =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addTrans 2
                    . addTrans 3

-- | Testing 'getNonFinalizedChainUpdates'
--  This test ensures that:
--  * An existing non finalized chain update transaction can be looked up
--  * Looking up with an unknown transaction hash will result in a 'Nothing' result.
testGetNonFinalizedChainUpdates ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testGetNonFinalizedChainUpdates _ = describe "getNonFinalizedChainUpdates" $ do
    it "present" $ do
        assertEqual
            "chain updates for UpdateMicroGTUPerEuro are present"
            [ (2, Map.insert (dummyUpdateInstructionWM 2) (dummySuccessTransactionResult 2) Map.empty),
              (3, Map.insert (dummyUpdateInstructionWM 3) (dummySuccessTransactionResult 3) Map.empty)
            ]
            $ getNonFinalizedChainUpdates UpdateMicroGTUPerEuro 1 sd
        assertEqual
            "one chain update for UpdateMicroGTUPerEuro are present from usn 3"
            [(3, Map.insert (dummyUpdateInstructionWM 3) (dummySuccessTransactionResult 3) Map.empty)]
            $ getNonFinalizedChainUpdates UpdateMicroGTUPerEuro 3 sd
    it "absent" $ do
        assertEqual
            "no chain updates for ProtocolUpdate are present"
            []
            $ getNonFinalizedChainUpdates UpdateProtocol 1 sd
  where
    addChainUpdate n = snd . TT.addTransaction (dummyChainUpdate n) 0 (dummySuccessTransactionResult n)
    sd =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addChainUpdate 2
                    . addChainUpdate 3

-- | Testing 'getNonFinalizedCredential'
--  This test ensures that:
--  * An existing non finalized credential deployment transaction can be looked up
--  * Looking up with an unknown transaction hash will result in a 'Nothing' result.
testGetNonFinalizedCredential ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testGetNonFinalizedCredential _ = describe "getNonFinalizedCredential" $ do
    it "present" $ do
        assertEqual
            "non-finalized credential deployment is present"
            (Just (credentialDeploymentWM, dummySuccessCredentialDeployment))
            $ getNonFinalizedCredential credDeploymentHash sd
    it "absent" $ do
        assertEqual "non-finalized credential deployment is absent" Nothing $ getNonFinalizedCredential nonExistingHash sd
  where
    addCredential = snd . TT.addTransaction dummyCredentialDeployment 0 dummySuccessCredentialDeployment
    credDeploymentHash = getHash dummyCredentialDeployment
    nonExistingHash :: TransactionHash
    nonExistingHash = TransactionHashV0 $! Hash.hash $! Hash.hashToByteString $! v0TransactionHash credDeploymentHash
    sd =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addCredential
                    . addCredential

-- | Testing 'getNextAccountNonce'
--  This test ensures that the function returns
--  the correct next account nonce.
--
--  Note that we do not test getting the next available account nonce
--  for accounts with finalized transactions.
-- This behaviour is tested in 'TransactionTableIntegrationTest' already.
testGetNextAccountNonce ::
    forall pv.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    SProtocolVersion pv ->
    Spec
testGetNextAccountNonce _ = describe "getNextAccountNonce" $ do
    it "with non-finalized" $ do
        void $ runTestWithBS $ do
            transactionTable %= addTrans 2 . addTrans 3
            sd <- get
            n0 <- getNextAccountNonce (accountAddressEmbed accEqAddr) sd
            liftIO $ n0 `shouldBe` (4, False)
    it "with no transactions" $ do
        void $ runTestWithBS $ do
            sd <- get
            n1 <- getNextAccountNonce (accountAddressEmbed accEqAddr) sd
            liftIO $ n1 `shouldBe` (minNonce, True)
  where
    -- An account that is present in the block state.
    accEqAddr = gaAddress $ head $ Dummy.makeFakeBakers 1
    bi = dummyTransactionBIFromSender accEqAddr
    -- Run the computation via the helper test monad.
    runTestWithBS = Helper.runMyTestMonad @pv Dummy.dummyIdentityProviders (timestampToUTCTime 1)
    -- Add a transaction from @accEqAddr@ with the provided nonce to the transaction table.
    addTrans n = snd . TT.addTransaction (bi n) 0 (dummySuccessTransactionResult n)

-- | Testing 'finalizeTransactions'.
--  This test ensures that the provided list of
--  transactions are removed from the the transaction table,
--  and if the transaction is either a normal transaction or
--  a chain update then the non finalized transaction map is
--  updated accordingly, meaning that it's removed from non finalized transaction
--  map and that the next available nonce for the sender of the transaction is set to
--  be 1 + the nonce of the removed transaction.
testRemoveTransactions ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testRemoveTransactions _ = describe "finalizeTransactions" $ do
    it "normal transactions" $ do
        sd' <- execStateT (finalizeTransactions [normalTransaction tr0]) sd
        assertEqual
            "Account non-finalized transactions"
            (Just TT.AccountNonFinalizedTransactions{_anftMap = Map.singleton 2 (Map.singleton tr1 (dummySuccessTransactionResult 2))})
            (sd' ^. transactionTable . TT.ttNonFinalizedTransactions . at sender)
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr1, (normalTransaction tr1, TT.Received 0 (dummySuccessTransactionResult 2)))])
            (sd' ^. transactionTable . TT.ttHashMap)
    it "chain updates" $ do
        sd' <- execStateT (finalizeTransactions [chainUpdate cu0]) sd1
        assertEqual
            "Chain update non-finalized transactions"
            (Just TT.NonFinalizedChainUpdates{_nfcuNextSequenceNumber = 2, _nfcuMap = Map.singleton 2 (Map.singleton cu1 (dummySuccessTransactionResult 2))})
            (sd' ^. transactionTable . TT.ttNonFinalizedChainUpdates . at UpdateMicroGTUPerEuro)
        assertEqual
            "transaction hash map"
            ( HM.fromList
                [ (getHash cu1, (chainUpdate cu1, TT.Received 0 (dummySuccessTransactionResult 2))),
                  (getHash tr1, (normalTransaction tr1, TT.Received 0 (dummySuccessTransactionResult 2)))
                ]
            )
            (sd' ^. transactionTable . TT.ttHashMap)
    it "credential deployments" $ do
        sd' <- execStateT (finalizeTransactions [credentialDeployment cred0]) sd2
        assertEqual
            "Non-finalized credential deployments"
            (sd' ^. transactionTable . TT.ttHashMap . at credDeploymentHash)
            Nothing
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr1, (normalTransaction tr1, TT.Received 0 (dummySuccessTransactionResult 2)))])
            (sd' ^. transactionTable . TT.ttHashMap)
  where
    sender = accountAddressEmbed dummyAccountAddress
    tr0 = dummyTransaction 1
    tr1 = dummyTransaction 2
    tr2 = dummyTransaction 1
    cu0 = dummyUpdateInstructionWM 1
    cu1 = dummyUpdateInstructionWM 2
    cred0 = credentialDeploymentWM
    addTrans t = snd . TT.addTransaction (normalTransaction t) 0 (dummySuccessTransactionResult (transactionNonce t))
    addChainUpdate u = snd . TT.addTransaction (chainUpdate u) 0 (dummySuccessTransactionResult (updateSeqNumber $ uiHeader $ wmdData u))
    addCredential = snd . TT.addTransaction dummyCredentialDeployment 0 dummySuccessCredentialDeployment
    credDeploymentHash = getHash dummyCredentialDeployment
    sd =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addTrans tr0
                    . addTrans tr1
                    . addTrans tr2
    sd1 =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addChainUpdate cu0
                    . addChainUpdate cu1
                    . addTrans tr1
    sd2 =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addCredential
                    . addTrans tr1

-- | Testing 'addTransaction'.
--  This test ensures that the supplied transaction is added
--  to the transaction table with the provided round.
--  This test also checks that the transaction table purge counter
--  is incremented.
testAddTransaction ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testAddTransaction _ = describe "addTransaction" $ do
    it "add transaction" $ do
        sd' <- execStateT (addTransaction tr0Round (normalTransaction tr0) (dummySuccessTransactionResult 1)) (dummyInitialSkovData @pv)
        assertEqual
            "Account non-finalized transactions"
            (Just TT.AccountNonFinalizedTransactions{_anftMap = Map.singleton 1 (Map.singleton tr0 (dummySuccessTransactionResult 1))})
            (sd' ^. transactionTable . TT.ttNonFinalizedTransactions . at sender)
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr0, (normalTransaction tr0, TT.Received (TT.commitPoint tr0Round) (dummySuccessTransactionResult 1)))])
            (sd' ^. transactionTable . TT.ttHashMap)
        assertEqual
            "transaction table purge counter is incremented"
            (1 + (dummyInitialSkovData @pv) ^. transactionTablePurgeCounter)
            (sd' ^. transactionTablePurgeCounter)
        sd'' <- execStateT (finalizeTransactions [normalTransaction tr0]) sd'
        added <- evalStateT (addTransaction tr0Round (normalTransaction tr1) (dummySuccessTransactionResult 1)) sd''
        assertEqual "tx should be added" True added
  where
    tr0Round = 1
    tr0 = dummyTransaction 1
    tr1 = dummyTransaction 2
    sender = accountAddressEmbed dummyAccountAddress

-- | Test of 'commitTransaction'.
--  The test checks that a live transaction i.e. present in the transaction table 'ttHashMap'
--  is being set to committed for the provided round with a pointer to the block provided (by the 'BlockHash').
testCommitTransaction ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testCommitTransaction _ = describe "commitTransaction" $ do
    it "commit transaction" $ do
        sd' <- execStateT (commitTransaction 1 bh 0 (normalTransaction tr0)) sd
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr0, (normalTransaction tr0, TT.Committed 1 (dummySuccessTransactionResult (transactionNonce tr0)) $ HM.fromList [(bh, TransactionIndex 0)]))])
            (sd' ^. transactionTable . TT.ttHashMap)
  where
    tr0 = dummyTransaction 1
    addTrans t = snd . TT.addTransaction (normalTransaction t) 0 (dummySuccessTransactionResult (transactionNonce t))
    sd =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addTrans tr0
    bh = BlockHash minBound

-- | Test 'markTransactionDead'
--  This test ensures that when a (committed) transaction identified
--  by the provided 'BlockHash' is marked dead, then it is removed
--  from the transaction table live map.
--  Further the test checks that marking a (received) transaction as dead has no effect,
--  as such a transaction will be freed from memory via transaction table purging
--  at some point.
testMarkTransactionDead ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testMarkTransactionDead _ = describe "markTransactionDead" $ do
    it "mark committed transaction dead" $ do
        sd' <- execStateT (commitTransaction 1 bh 0 (normalTransaction tr0)) sd
        sd'' <- execStateT (markTransactionDead bh (normalTransaction tr0)) sd'
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr0, (normalTransaction tr0, TT.Received 1 (dummySuccessTransactionResult (transactionNonce tr0))))])
            (sd'' ^. transactionTable . TT.ttHashMap)
    it "mark received transaction dead" $ do
        sd' <- execStateT (markTransactionDead bh (normalTransaction tr0)) sd
        assertEqual
            "transaction hash map"
            (HM.fromList [(getHash tr0, (normalTransaction tr0, TT.Received 0 (dummySuccessTransactionResult (transactionNonce tr0))))])
            (sd' ^. transactionTable . TT.ttHashMap)
  where
    tr0 = dummyTransaction 1
    addTrans t = snd . TT.addTransaction (normalTransaction t) 0 (dummySuccessTransactionResult (transactionNonce t))
    sd =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addTrans tr0
    bh = BlockHash minBound

-- | Test 'purgeTransactionTable' function.
--  This test ensures that transactions eligible for purging are
--  removed from the transaction table and the pending transactions.
--  This test also ensures that the transaction table purge counter is reset
--  after a purge has been carried out.
testPurgeTransactionTable ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testPurgeTransactionTable _ = describe "purgeTransactionTable" $ do
    it "force purge the transaction table" $ do
        -- increment the purge counter.
        sd' <- execStateT (addTransaction 0 (normalTransaction tr0) (dummySuccessTransactionResult 1)) sd
        sd'' <- execStateT (purgeTransactionTable True theTime) sd'
        assertEqual
            "purge counter should be reset"
            0
            (sd'' ^. transactionTablePurgeCounter)
        assertEqual
            "Account non-finalized transactions"
            Nothing
            (sd'' ^. transactionTable . TT.ttNonFinalizedTransactions . at sender)
        assertEqual
            "Chain update non-finalized transactions"
            (Just $ TT.NonFinalizedChainUpdates{_nfcuMap = Map.empty, _nfcuNextSequenceNumber = 1})
            (sd'' ^. transactionTable . TT.ttNonFinalizedChainUpdates . at UpdateMicroGTUPerEuro)
        assertEqual
            "Non-finalized credential deployments"
            Nothing
            (sd'' ^. transactionTable . TT.ttHashMap . at credDeploymentHash)
  where
    addChainUpdate u = snd . TT.addTransaction (chainUpdate u) 0 (dummySuccessTransactionResult (updateSeqNumber $ uiHeader $ wmdData u))
    addCredential = snd . TT.addTransaction dummyCredentialDeployment 0 dummySuccessCredentialDeployment
    tr0 = dummyTransaction 1
    cu0 = dummyUpdateInstructionWM 1
    theTime = timestampToUTCTime $! Timestamp 1_596_409_021
    sender = accountAddressEmbed dummyAccountAddress
    credDeploymentHash = getHash dummyCredentialDeployment
    -- Set the round for the last finalized block pointer.
    sd =
        dummyInitialSkovData @pv
            & transactionTable
                %~ addChainUpdate cu0
                    . addCredential
            & pendingTransactionTable
                %~ TT.addPendingDeployCredential credDeploymentHash

-- | Test 'clearOnProtocolUpdate' function.
--  This test checks that the following is occurring in the associated 'SkovData' when invoked:
--
--  * The pending blocks table is cleared
--  * The block table is cleared
--  * The branches is cleared
--  * All committed transactions should be rolled back into the 'Received' state.
testClearOnProtocolUpdate ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testClearOnProtocolUpdate _ = describe "clearOnProtocolUpdate" $
    it "clears on protocol update" $ do
        sd' <- execStateT (commitTransaction 1 bh 0 (normalTransaction tr0)) sd
        sd'' <- execStateT clearOnProtocolUpdate sd'
        assertEqual
            "block table should be empty"
            emptyBlockTable
            (sd'' ^. blockTable)
        assertEqual
            "Branches should be empty"
            Seq.empty
            (sd'' ^. branches)
        assertEqual
            "committed transactions should be received with commit point 0"
            (HM.fromList [(getHash tr0, (normalTransaction tr0, TT.Received 0 (dummySuccessTransactionResult 1)))])
            (sd'' ^. transactionTable . TT.ttHashMap)
  where
    tr0 = dummyTransaction 1
    bh = BlockHash minBound
    addTrans t = snd . TT.addTransaction (normalTransaction t) 0 (dummySuccessTransactionResult (transactionNonce t))
    sd =
        skovDataWithTestBlocks @pv
            & transactionTable
                %~ addTrans tr0

tests :: Spec
tests = describe "KonsensusV1.TreeState" $ do
    Common.forEveryProtocolVersionConsensusV1 $ \spv pvString ->
        describe pvString $ do
            describe "BlockTable" $ do
                testGetMemoryBlockStatus spv
                testGetBlockStatus spv
                testGetRecentBlockStatus spv
            describe "BlockTable" $ do
                testMakeLiveBlock spv
                testMarkBlockDead spv
            describe "TransactionTable" $ do
                testLookupLiveTransaction spv
                testLookupTransaction spv
                testGetNonFinalizedAccountTransactions spv
                testGetNonFinalizedChainUpdates spv
                testGetNonFinalizedCredential spv
                testGetNextAccountNonce spv
                testRemoveTransactions spv
                testAddTransaction spv
                testCommitTransaction spv
                testMarkTransactionDead spv
                testPurgeTransactionTable spv
            describe "Clear on protocol update" $ do
                testClearOnProtocolUpdate spv
