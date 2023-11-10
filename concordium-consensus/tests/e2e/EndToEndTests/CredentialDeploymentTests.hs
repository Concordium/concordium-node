{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | End to end tests for credential deployments.
module EndToEndTests.CredentialDeploymentTests (tests) where

import Control.Monad.IO.Class
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import System.Random
import Test.HUnit
import Test.Hspec

import Concordium.Common.Version
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

-- | A credential deployment transaction.
credBi :: BlockItem
credBi =
    credentialDeployment $ addMetadata (\x -> CredentialDeployment{biCred = x}) (tt + 1) accCreation
  where
    tt = utcTimeToTransactionTime testTime
    accCreation = icdi1

-- | Helper for reading an 'AccountCreation' from a 'ByteString'.
readAccountCreation :: BSL.ByteString -> AccountCreation
readAccountCreation bs =
    case AE.eitherDecode bs of
        Left err -> error $ "Cannot read account creation " ++ err
        Right d -> if vVersion d == 0 then vValue d else error "Incorrect account creation version."

-- | A valid initial credential deployment.
{-# WARNING icdi1 "Do not use in production." #-}
icdi1 :: AccountCreation
icdi1 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-1.json" >>= embedFile)

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
          bbTransactionOutcomesHash = read "b9444648bf759471276fdba1930af0c543847d22de89c27939791898d757516d",
          bbStateHash = read "b8bc96ec5f162db36784ea96ec29e3e8ad92abff341a6847e3bf524fdada28ff"
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
          bbStateHash = read "798d5089818bcc7b8873e2585fb4fbf3d4dceffca32531259f466e7c435c8817"
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
          bbStateHash = read "4da0deab5b564cd77c617a2ac7dc8a6064f87e99b09e58c87b5f9e687db2197a"
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
