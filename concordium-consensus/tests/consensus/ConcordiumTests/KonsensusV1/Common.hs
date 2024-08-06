{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | A common module for various helper definitions used for testing purposes.
module ConcordiumTests.KonsensusV1.Common where

import qualified Data.Vector as Vec
import System.Random

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.DummyData as Dummy
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.Persistent.BlobStore (blobRefToBufferedRef, refNull)
import Concordium.GlobalState.Persistent.BlockState (HashedPersistentBlockState (..), PersistentBlockState)
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types
import qualified Concordium.Types.Conditionally as Cond
import Concordium.Types.Option
import Concordium.Types.Parameters
import Concordium.Types.TransactionOutcomes
import GHC.IO (unsafePerformIO)
import GHC.IORef (newIORef)
import Test.Hspec (Spec)

-- | Just an arbitrary chosen block hash used for testing.
myBlockHash :: BlockHash
myBlockHash = BlockHash $ Hash.hash "my block hash"

dummyStateHash :: StateHash
dummyStateHash = StateHashV0 $ Hash.hash "DummyPersistentBlockState"

dummyBlockResultHash :: BlockResultHash
dummyBlockResultHash = BlockResultHash $ Hash.hash "DummyBlockResult"

-- | A dummy block state that has no content.
dummyBlockState :: HashedPersistentBlockState pv
dummyBlockState = HashedPersistentBlockState{..}
  where
    hpbsPointers = dummyPersistentBlockState
    hpbsHash = dummyStateHash

-- | A dummy block state that is just a @BlobRef maxBound@.
dummyPersistentBlockState :: PersistentBlockState pv
{-# NOINLINE dummyPersistentBlockState #-}
dummyPersistentBlockState = unsafePerformIO $ newIORef $ blobRefToBufferedRef refNull

-- | A 'QuorumCertificate' pointing to the block provided.
--  The certificate itself is inherently invalid as it is for round and epoch 0.
--  Further no one signed it and it has an empty signature.
--  However for the tests exposed here it is sufficient.
dummyQuorumCertificate :: BlockHash -> QuorumCertificate
dummyQuorumCertificate blockHash =
    QuorumCertificate
        { qcBlock = blockHash,
          qcRound = 0,
          qcEpoch = 0,
          qcAggregateSignature = mempty,
          qcSignatories = FinalizerSet 0
        }

-- | A BlockNonce consisting
--  of a VRF proof on the empty string with the 'dummyVRFKeys'.
dummyBlockNonce :: BlockNonce
dummyBlockNonce = VRF.prove dummyVRFKeys ""

-- | An arbitrary chosen 'VRF.KeyPair'.
--  This is required to create the 'dummyBlockNonce'.
dummyVRFKeys :: VRF.KeyPair
dummyVRFKeys = fst $ VRF.randomKeyPair (mkStdGen 0)

-- | A 'BlockPointer' which refers to a block with no meaningful state.
someBlockPointer :: SProtocolVersion pv -> BlockHash -> Round -> Epoch -> BlockPointer pv
someBlockPointer sProtocolVersion bh r e =
    BlockPointer
        { bpInfo =
            BlockMetadata
                { bmHeight = 0,
                  bmReceiveTime = timestampToUTCTime 0,
                  bmArriveTime = timestampToUTCTime 0,
                  bmEnergyCost = 0,
                  bmTransactionsSize = 0,
                  bmBlockStateHash = case sBlockHashVersionFor sProtocolVersion of
                    SBlockHashVersion0 -> Cond.CFalse
                    SBlockHashVersion1 -> Cond.CTrue stateHash
                },
          bpBlock = NormalBlock $ SignedBlock bakedBlock bh (Sig.sign sigKeyPair "foo"),
          bpState = dummyBlockState
        }
  where
    stateHash = StateHashV0 $ Hash.hash "empty state hash"
    -- A dummy block pointer with no meaningful state.
    bakedBlock =
        BakedBlock
            { bbRound = r,
              bbEpoch = e,
              bbTimestamp = 0,
              bbBaker = 0,
              bbQuorumCertificate = dummyQuorumCertificate $ BlockHash minBound,
              bbTimeoutCertificate = Absent,
              bbEpochFinalizationEntry = Absent,
              bbNonce = dummyBlockNonce,
              bbTransactions = Vec.empty,
              bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
                SBlockHashVersion0 ->
                    DerivableBlockHashesV0
                        { dbhv0TransactionOutcomesHash = toTransactionOutcomesHash emptyTransactionOutcomesHashV1,
                          dbhv0BlockStateHash = stateHash
                        }
                SBlockHashVersion1 ->
                    DerivableBlockHashesV1
                        { dbhv1BlockResultHash = BlockResultHash $ Hash.hash "empty state hash"
                        }
            }

-- | A block pointer with 'myBlockHash' as block hash.
myBlockPointer :: SProtocolVersion pv -> Round -> Epoch -> BlockPointer pv
myBlockPointer sProtocolVersion = someBlockPointer sProtocolVersion myBlockHash

-- | A key pair created from the provided seed.
sigKeyPair' :: Int -> Sig.KeyPair
sigKeyPair' seed = fst $ Dummy.randomBlockKeyPair $ mkStdGen seed

-- | The public key of the 'sigKeyPair''.
sigPublicKey' :: Int -> Sig.VerifyKey
sigPublicKey' seed = Sig.verifyKey $ sigKeyPair' seed

-- | An arbitrary chosen key pair
sigKeyPair :: Sig.KeyPair
sigKeyPair = fst $ Dummy.randomBlockKeyPair $ mkStdGen 42

-- | The public key of the 'sigKeyPair'.
sigPublicKey :: Sig.VerifyKey
sigPublicKey = Sig.verifyKey sigKeyPair

-- | Run tests for each protocol version.
-- The tests are provided with the protocol version and a string representation of the protocol version.
forEveryProtocolVersion ::
    (forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> String -> Spec) ->
    Spec
forEveryProtocolVersion check =
    sequence_
        [ check SP1 "P1",
          check SP2 "P2",
          check SP3 "P3",
          check SP4 "P4",
          check SP5 "P5",
          check SP6 "P6"
          -- FIXME: check SP7 "P7"
        ]

-- | Run tests for each protocol version using consensus v1 (P6 and onwards).
-- The tests are provided with the protocol version and a string representation of the protocol version.
forEveryProtocolVersionConsensusV1 ::
    ( forall pv.
      (IsProtocolVersion pv, IsConsensusV1 pv) =>
      SProtocolVersion pv ->
      String ->
      Spec
    ) ->
    Spec
forEveryProtocolVersionConsensusV1 check =
    forEveryProtocolVersion $ \spv pvString -> case consensusVersionFor spv of
        ConsensusV0 -> return ()
        ConsensusV1 -> check spv pvString

forEveryProtocolVersionBHV1 ::
    ( forall pv.
      (IsProtocolVersion pv, IsConsensusV1 pv, BlockHashVersionFor pv ~ 'BlockHashVersion1) =>
      SProtocolVersion pv ->
      String ->
      Spec
    ) ->
    Spec
forEveryProtocolVersionBHV1 check =
    forEveryProtocolVersionConsensusV1 $ \spv pvString -> case sBlockHashVersionFor spv of
        SBlockHashVersion1 -> check spv pvString
        _ -> return ()
