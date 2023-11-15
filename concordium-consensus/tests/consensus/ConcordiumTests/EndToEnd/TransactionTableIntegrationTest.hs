{-# LANGUAGE NumericUnderscores #-}

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
import Concordium.Types.Transactions

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
testBB1 :: BakedBlock
testBB1 =
    BakedBlock
        { bbRound = 1,
          bbEpoch = 0,
          bbTimestamp = 1_000,
          bbBaker = bakerId,
          bbQuorumCertificate = genesisQuorumCertificate genesisHash,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 1 (bakerVRFKey bakerId),
          bbTransactions = Vec.fromList [transfer1],
          bbTransactionOutcomesHash = read "13907ff30e010398b3438b73a55f6fd02177d653527aafb6b77360a646cb938c",
          bbStateHash = read "84d5b24177c60db5fb17f62a5cc93a500afc6565977f080cbd9260a68be66925"
        }
  where
    bakerId = 2

-- | Valid block for round 2.
--  This block carries a QC for 'testBB1' thus certifying it.
testBB2 :: BakedBlock
testBB2 =
    BakedBlock
        { bbRound = 2,
          bbEpoch = 0,
          bbTimestamp = 3_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB1,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 2 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = read "f840ea702e095175b8c2fceacc2377d5d2d0be867350bc0bdd8c6d56ee14797c",
          bbStateHash = read "0b286c7356d7c69717e42b39fc3cabf2fd82dbc4713f2e752084b1b9e2c5bdb8"
        }
  where
    bakerId = 4

-- | Valid block for round 3, finalizes 'testBB1' as this block
--  carries a QC for 'testBB2'.
testBB3 :: BakedBlock
testBB3 =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 0,
          bbTimestamp = 5_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB2,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 3 (bakerVRFKey bakerId),
          bbTransactions = Vec.empty,
          bbTransactionOutcomesHash = read "9bbf1ab9edd3744bc88dfc0a6aa87a89dc51765d9a4b57bc8c7c49b1fb151099",
          bbStateHash = read "80d087748edeea46b7d0b8f25c8fb50bb015b498c11eeb03e8efe8b59e7d40f9"
        }
  where
    bakerId = 4

-- | Valid block for round 4 with 1 normal transfer
testBB4 :: BakedBlock
testBB4 =
    BakedBlock
        { bbRound = 4,
          bbEpoch = 0,
          bbTimestamp = 7_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor testBB3,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce genesisLEN 4 (bakerVRFKey bakerId),
          bbTransactions = Vec.fromList [transfer2],
          bbTransactionOutcomesHash = read "d46c011009b5315c7cd32bb1345bd2e73a3cd6111a7e4d06c33e863f16c8c8bd",
          bbStateHash = read "a47ca3a8412ad577df94ae8ebc288f8972a499ce5315033bfc2f2c18ce00bfb8"
        }
  where
    bakerId = 3

-- | Test that the anftNextNonce is correctly set when adding a new transaction after
--  transactions has been finalized and the transaction table is fully purged.
testAnftNonce :: Assertion
testAnftNonce = runTestMonad noBaker testTime genesisData $ do
    let b1 = signedPB testBB1
    succeedReceiveBlock b1
    let b2 = signedPB testBB2
    succeedReceiveBlock b2

    sd <- get
    liftIO $
        assertEqual
            "sender should be present in transaction table"
            (Just 1)
            (sd ^? transactionTable . TT.ttNonFinalizedTransactions . ix sender . TT.anftNextNonce)

    let b3 = signedPB testBB3
    succeedReceiveBlock b3
    -- transaction in b1 is now finalized and we force purge the table so
    -- sender is expunged from transaction table.
    purgeTransactionTable True (posixSecondsToUTCTime 1)
    sd' <- get
    liftIO $
        assertEqual
            "sender should not be present in tt"
            Nothing
            (sd' ^? transactionTable . TT.ttNonFinalizedTransactions . ix sender . TT.anftNextNonce)

    let b4 = signedPB testBB4
    succeedReceiveBlock b4
    sd'' <- get
    liftIO $
        assertEqual
            "sender should be present in tt again and anftNextNonce is correctly set"
            (Just 2)
            (sd'' ^? transactionTable . TT.ttNonFinalizedTransactions . ix sender . TT.anftNextNonce)
  where
    sender = accountAddressEmbed foundationAccountAddress

tests :: Spec
tests = describe "EndToEndTests.TransactionTableIntegrationTest" $ do
    it "anftNextNonce test" testAnftNonce
