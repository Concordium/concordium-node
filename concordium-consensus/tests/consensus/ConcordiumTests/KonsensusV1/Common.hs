{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A common module for various helper definitions used for testing purposes.
module ConcordiumTests.KonsensusV1.Common where

import qualified Data.Vector as Vec
import System.Random

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.DummyData as Dummy
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types
import qualified Concordium.Types.Conditionally as Cond
import Concordium.Types.Option
import Concordium.Types.TransactionOutcomes
import ConcordiumTests.KonsensusV1.TreeStateTest hiding (tests)

-- | Just an arbitrary chosen block hash used for testing.
myBlockHash :: BlockHash
myBlockHash = BlockHash $ Hash.hash "my block hash"

-- | A 'BlockPointer' which refers to a block with no meaningful state.
someBlockPointer :: BlockHash -> Round -> Epoch -> BlockPointer 'P6
someBlockPointer bh r e =
    BlockPointer
        { bpInfo =
            BlockMetadata
                { bmHeight = 0,
                  bmReceiveTime = timestampToUTCTime 0,
                  bmArriveTime = timestampToUTCTime 0,
                  bmEnergyCost = 0,
                  bmTransactionsSize = 0,
                  bmBlockStateHash = Cond.CFalse
                },
          bpBlock = NormalBlock $ SignedBlock bakedBlock bh (Sig.sign sigKeyPair "foo"),
          bpState = dummyBlockState
        }
  where
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
              bbDerivableHashes =
                DBHashesV0 $
                    BlockDerivableHashesV0
                        { bdhv0TransactionOutcomesHash = emptyTransactionOutcomesHashV1,
                          bdhv0BlockStateHash = StateHashV0 $ Hash.hash "empty state hash"
                        }
            }

-- | A block pointer with 'myBlockHash' as block hash.
myBlockPointer :: Round -> Epoch -> BlockPointer 'P6
myBlockPointer = someBlockPointer myBlockHash

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
