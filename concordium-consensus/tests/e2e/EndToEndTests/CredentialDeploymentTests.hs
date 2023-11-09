{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | End to end tests for credential deployments.
module EndToEndTests.CredentialDeploymentTests (tests) where

import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import System.Random
import Test.HUnit
import Test.Hspec

import Concordium.Common.Time
import Concordium.Crypto.DummyData
import Concordium.Crypto.FFIDataTypes
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.GlobalState.BlockState
import Concordium.ID.Parameters
import Concordium.ID.Types
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types
import Concordium.Types.Option
import Concordium.Types.Transactions
import EndToEndTests.E2ETestData

-- | Make public keys for a credential deployment
mkCredentialPublicKeys :: CredentialPublicKeys
mkCredentialPublicKeys = makeCredentialPublicKeys [key] 1
  where
    key = SigScheme.correspondingVerifyKey $ dummyKeyPair 1
    dummyKeyPair :: Int -> SigScheme.KeyPair
    dummyKeyPair = uncurry SigScheme.KeyPairEd25519 . fst . randomEd25519KeyPair . mkStdGen

-- | A credential deployment transaction.
credBi :: BlockItem
credBi =
    credentialDeployment $ addMetadata (\x -> CredentialDeployment{biCred = x}) (tt + 1) accCreation
  where
    tt = utcTimeToTransactionTime testTime
    regId seed = RegIdCred $ generateGroupElementFromSeed dummyGlobalContext seed
    accCreation :: AccountCreation
    accCreation =
        AccountCreation
            { messageExpiry = tt + 1,
              credential =
                InitialACWP
                    InitialCredentialDeploymentInfo
                        { icdiValues =
                            InitialCredentialDeploymentValues
                                { icdvAccount = mkCredentialPublicKeys,
                                  icdvRegId = regId 42,
                                  icdvIpId = IP_ID 0,
                                  icdvPolicy = pol
                                },
                          icdiSig =
                            IpCdiSignature
                                { theSignature = "invalid signature"
                                }
                        }
            }

    pol =
        Policy
            { pValidTo =
                YearMonth
                    { ymYear = 2070,
                      ymMonth = 1
                    },
              pCreatedAt =
                YearMonth
                    { ymYear = 2021,
                      ymMonth = 1
                    },
              pItems = Map.empty
            }

-- | Valid block for round 1 with 1 credential deployment.
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
          bbTransactions = Vec.fromList [credBi],
          bbTransactionOutcomesHash = read "d270b085491bc5b52d7791e7364897b120002032ac8de60af8984890d02c0a03",
          bbStateHash = read "ff2aae922111fb0b95d1736e02e641753bdf4e5b10d09ec2e9d9bf5096a09e96"
        }
  where
    bakerId = 2

-- | Valid block for round 2.
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
          bbTransactionOutcomesHash = read "375fef64a251f353d608171d283d00fe00aa0bd77596ba7703c810f48056ef89",
          bbStateHash = read "22e786ad9aff80596e1b315b5d26b812693a804928c2709bd59ab6cee340b572"
        }
  where
    bakerId = 4

-- | Valid block for round 3, finalizes round 'testBB1'.
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
          bbTransactionOutcomesHash = read "375fef64a251f353d608171d283d00fe00aa0bd77596ba7703c810f48056ef89",
          bbStateHash = read "0c6e7c833e91c8d04b15dc345faa3a6d80ee9b1ed1a16437d245931f2a97744f"
        }
  where
    bakerId = 4

-- | A test that deploys a single credential.
testDeployCredential :: Assertion
testDeployCredential = runTestMonad noBaker testTime genesisData $ do
    lfbState0 <- use (lastFinalized . to bpState)
    noAccs0 <- length <$> getAccountList lfbState0
    let b1 = signedPB testBB1
    succeedReceiveBlock b1
    let b2 = signedPB testBB2
    succeedReceiveBlock b2
    -- b3 finalizes b1 as it carries a qc for b2 (which carries a qc for b1).
    let b3 = signedPB testBB3
    succeedReceiveBlock b3
    -- check that the account is now present in the last finalized block.
    lfbState1 <- use (lastFinalized . to bpState)
    noAccs1 <- length <$> getAccountList lfbState1
    liftIO $ assertEqual "there should be one extra account" (noAccs0 + 1) noAccs1

tests :: Word -> Spec
tests _ = describe "EndToEndTests.CredentialDeployments" $ do
    it "deploy and finalize one credential" testDeployCredential
