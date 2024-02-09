{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | End to end test that check transaction table is as expected while the tree state processes blocks.
module ConcordiumTests.EndToEnd.TransactionTableIntegrationTest (tests) where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Concordium.GlobalState.TransactionTable as TT
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.Types
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Option
import Concordium.Types.Parameters
import Concordium.Types.Transactions

import qualified ConcordiumTests.KonsensusV1.Common as Common
import ConcordiumTests.KonsensusV1.Consensus.Blocks hiding (testBB1, testBB2, testBB2', testBB3, testBB3', tests)

-- | Make a raw transfer transaction with the provided nonce.
mkTransferTransaction :: Nonce -> BareBlockItem
mkTransferTransaction nonce = NormalTransaction{biTransaction = signTransactionSingle foundationKeyPair mkHeader payload}
  where
    mkHeader =
        TransactionHeader
            { thSender = foundationAccountAddress,
              thNonce = nonce,
              thEnergyAmount = 1000000,
              thPayloadSize = payloadSize payload,
              thExpiry = 10000
            }
    payload = encodePayload $ Transfer foundationAccountAddress 10

-- | A transfer with nonce 1 for testBB1
transfer1 :: BlockItem
transfer1 = normalTransaction $ addMetadata (\x -> NormalTransaction{biTransaction = x}) 1000 (biTransaction $ mkTransferTransaction 1)

-- | A transfer with nonce 2 for testBB4
transfer2 :: BlockItem
transfer2 = normalTransaction $ addMetadata (\x -> NormalTransaction{biTransaction = x}) 1001 (biTransaction $ mkTransferTransaction 2)

-- | Valid block for round 1 with 1 normal transfer
testBB1 :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB1 =
    BakedBlock
        { bbRound = 1,
          bbEpoch = 0,
          bbTimestamp = 1_000,
          bbBaker = bakerId,
          bbQuorumCertificate = genesisQuorumCertificate (genesisHash sProtocolVersion),
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 1 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.fromList [transfer1],
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "13907ff30e010398b3438b73a55f6fd02177d653527aafb6b77360a646cb938c",
                      dbhv0BlockStateHash = read "84d5b24177c60db5fb17f62a5cc93a500afc6565977f080cbd9260a68be66925"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "c61642de9fe839abbf378f8e9e56bfd5a2ed744dae5ce82df8dcb9a7849f9ce2"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 2

-- | Valid block for round 2.
--  This block carries a QC for 'testBB1' thus certifying it.
testBB2 :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB2 =
    BakedBlock
        { bbRound = 2,
          bbEpoch = 0,
          bbTimestamp = 3_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB1,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 2 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "f840ea702e095175b8c2fceacc2377d5d2d0be867350bc0bdd8c6d56ee14797c",
                      dbhv0BlockStateHash = read "0b286c7356d7c69717e42b39fc3cabf2fd82dbc4713f2e752084b1b9e2c5bdb8"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "569e1cb324b05d744c3d3f29146c87252e0062f5654278b2e935e92e92778f49"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 4

-- | Valid block for round 3, finalizes 'testBB1' as this block
--  carries a QC for 'testBB2'.
testBB3 :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3 =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 0,
          bbTimestamp = 5_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB2,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 3 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "9bbf1ab9edd3744bc88dfc0a6aa87a89dc51765d9a4b57bc8c7c49b1fb151099",
                      dbhv0BlockStateHash = read "80d087748edeea46b7d0b8f25c8fb50bb015b498c11eeb03e8efe8b59e7d40f9"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "ce6d3023e834d92e0ac4cde15ed1f20aacab400d1083c98adca13c9ee1b9c426"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 4

-- | Valid block for round 4 with 1 normal transfer
testBB4 :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB4 =
    BakedBlock
        { bbRound = 4,
          bbEpoch = 0,
          bbTimestamp = 7_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB3,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 4 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.fromList [transfer2],
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "d46c011009b5315c7cd32bb1345bd2e73a3cd6111a7e4d06c33e863f16c8c8bd",
                      dbhv0BlockStateHash = read "a47ca3a8412ad577df94ae8ebc288f8972a499ce5315033bfc2f2c18ce00bfb8"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "e40b8cfe9cf6f2f26f1e81783c46dbd4f5de533f9f6a9a789d4849b2ece32b90"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 3

-- | Test that the @getNextAccountNonce@ returns correctly when adding a new transaction for an account A, after
--  some prior transactions for account A has been finalized (and the transaction table is fully purged).
testAccountNonce ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testAccountNonce sProtocolVersion =
    it "account nonce test" $ runTestMonad @pv noBaker testTime (genesisData sProtocolVersion) $ do
        nonce <- getNextAccountNonce sender =<< get
        liftIO $
            assertEqual
                "Transaction is not received"
                (1, True)
                nonce
        let b1 = signedPB testBB1
        succeedReceiveBlock b1
        let b2 = signedPB testBB2
        succeedReceiveBlock b2

        nonce' <- getNextAccountNonce sender =<< get
        liftIO $
            assertEqual
                "Transaction is in non-finalized transactions"
                (2, False)
                nonce'

        let b3 = signedPB testBB3
        succeedReceiveBlock b3
        -- transaction in b1 is now finalized and we force purge the table so
        -- sender is expunged from transaction table.
        purgeTransactionTable True (posixSecondsToUTCTime 1)
        sd <- get
        nonce'' <- getNextAccountNonce sender sd
        liftIO $
            assertEqual
                "first transaction should be finalized"
                (2, True)
                nonce''
        liftIO $
            assertEqual
                "transaction should not be in the anft map for the sender anymore"
                Nothing
                (sd ^? transactionTable . TT.ttNonFinalizedTransactions . ix sender)

        let b4 = signedPB testBB4
        succeedReceiveBlock b4
        nonce''' <- getNextAccountNonce sender =<< get
        liftIO $
            assertEqual
                "sender should be present in tt again and anftNextNonce is correctly set"
                (3, False)
                nonce'''
  where
    sender = accountAddressEmbed foundationAccountAddress

tests :: Spec
tests = describe "EndToEndTests.TransactionTableIntegrationTest" $ do
    Common.forEveryProtocolVersionConsensusV1 $ \spv pvString ->
        describe pvString $
            testAccountNonce spv
