{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | End to end tests for credential deployments.
-- For future maintainers: Note that the blocks below have hardcoded transaction outcome and state hashes.
-- These can be obtained by running the test and observe the program output.
-- (The monad we're running the tests within has a logger)
-- It is not expected that the hardcoded hashes change unless the protocol version changes (AND the underlying hashing scheme).
module ConcordiumTests.EndToEnd.CredentialDeploymentTests (tests) where

import Concordium.Utils
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import Concordium.Common.Version
import Concordium.GlobalState.BlockState
import Concordium.ID.Types
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Option
import Concordium.Types.Parameters
import Concordium.Types.Transactions

import qualified ConcordiumTests.KonsensusV1.Common as Common
import ConcordiumTests.KonsensusV1.Consensus.Blocks hiding (testBB1, testBB2, testBB2', testBB3, testBB3', tests)

-- | Helper for reading an 'AccountCreation' from a 'ByteString'.
readAccountCreation :: BSL.ByteString -> AccountCreation
readAccountCreation bs =
    case AE.eitherDecode bs of
        Left err -> error $ "Cannot read account creation " ++ err
        Right d -> if vVersion d == 0 then vValue d else error "Incorrect account creation version."

-- 3 valid credentials
{-# WARNING cred1 "Do not use in production." #-}
cred1 :: AccountCreation
cred1 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-1.json" >>= embedFile)

{-# WARNING cred2 "Do not use in production." #-}
cred2 :: AccountCreation
cred2 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/initial-credential-2.json" >>= embedFile)

{-# WARNING cred3 "Do not use in production." #-}
cred3 :: AccountCreation
cred3 = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/credential-1.json" >>= embedFile)

-- | A credential deployment transaction yielding cred1.
credBi1 :: BlockItem
credBi1 =
    credentialDeployment $ addMetadata (\x -> CredentialDeployment{biCred = x}) (tt + 1) cred1
  where
    tt = utcTimeToTransactionTime testTime

-- | A credential deployment transaction yielding cred2.
credBi2 :: BlockItem
credBi2 =
    credentialDeployment $ addMetadata (\x -> CredentialDeployment{biCred = x}) (tt + 1) cred2
  where
    tt = utcTimeToTransactionTime testTime

-- | A credential deployment transaction yielding cred3
credBi3 :: BlockItem
credBi3 =
    credentialDeployment $ addMetadata (\x -> CredentialDeployment{biCred = x}) (tt + 1) cred3
  where
    tt = utcTimeToTransactionTime testTime

-- | Valid block for round 1 with 1 credential deployment.
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
          bbTransactions = Vec.fromList [credBi1],
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "b9444648bf759471276fdba1930af0c543847d22de89c27939791898d757516d",
                      dbhv0BlockStateHash = read "b8bc96ec5f162db36784ea96ec29e3e8ad92abff341a6847e3bf524fdada28ff"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "26c5ed1dc59b3110601dcf22797d52ca744a332bdd425069acc77326e8adf739"
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
                    { dbhv0TransactionOutcomesHash = read "375fef64a251f353d608171d283d00fe00aa0bd77596ba7703c810f48056ef89",
                      dbhv0BlockStateHash = read "798d5089818bcc7b8873e2585fb4fbf3d4dceffca32531259f466e7c435c8817"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "b23eb835ac2c975a7e9116590d1a6e5b9b26fe65aeaa7b311760431a189fa20c"
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
                    { dbhv0TransactionOutcomesHash = read "375fef64a251f353d608171d283d00fe00aa0bd77596ba7703c810f48056ef89",
                      dbhv0BlockStateHash = read "4da0deab5b564cd77c617a2ac7dc8a6064f87e99b09e58c87b5f9e687db2197a"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "e708da0aeec770abd93544a9717e456712e699e1722fc292c424b8e8fdbbb43d"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 4

-- | A test that deploys a single credential, and it ends up in the last finalized block.
testDeployCredential ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testDeployCredential sProtocolVersion =
    it "deploy and finalize one credential" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
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
            liftIO $ assertEqual "there should be one extra account in lfb" (noAccs0 + 1) noAccs1

-- | Valid block for round 2.
--  This block has one credential deployment.
--  This block carries a QC for 'testBB1' thus certifying it.
testBB2' :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB2' =
    BakedBlock
        { bbRound = 2,
          bbEpoch = 0,
          bbTimestamp = 3_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB1,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 2 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.fromList [credBi2],
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "abc4628869bb526115226dd01ad54bf33f54609fa770d50a9242aaf009f42fa1",
                      dbhv0BlockStateHash = read "e3cf3b280159bc20645738fb1343486d16104989a524fb5feb59ac1b0b7af9ad"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "3ee65d909c11c7329694fbbfada2095b7abb8423394122e0d9e97b43cf5d1b4a"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 4

-- | Valid block for round 3, carries a TC for round 2.
--  This block has one credential deployment.
testBB3' :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB3' =
    BakedBlock
        { bbRound = 3,
          bbEpoch = 0,
          bbTimestamp = 5_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB1,
          bbTimeoutCertificate = Present (validTimeoutFor sProtocolVersion (validQCFor @pv testBB1) 2),
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 3 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.fromList [credBi3],
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "3af8504795a03353248be256f66366263f7484c814c5a26760210bbdfd609003",
                      dbhv0BlockStateHash = read "67eb8f778a4a43efa80c73a954110154ae417e21d43c33b857b962af36913e29"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "4ec24c4ad39bce3662a8ce00fc684730df8e96c7c8acda340086826ff08de708"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 4

testBB4 :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB4 =
    BakedBlock
        { bbRound = 4,
          bbEpoch = 0,
          bbTimestamp = 7_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB3',
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 4 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "b0972dd7af05ed6feaa40099fffa9c5c5e0ba9741938166cdb57584780688743",
                      dbhv0BlockStateHash = read "9e698b9c6425b382d8fda5584f530688c237ad013e8aaf848fea274e50244111"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "cdca4d101170df0990e2624dd2a1bc07b301a9a867f8e0c1d4f8dcc2a60629ee"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 3

testBB5 :: forall pv. (IsProtocolVersion pv, IsConsensusV1 pv) => BakedBlock pv
testBB5 =
    BakedBlock
        { bbRound = 5,
          bbEpoch = 0,
          bbTimestamp = 9_000,
          bbBaker = bakerId,
          bbQuorumCertificate = validQCFor @pv testBB4,
          bbTimeoutCertificate = Absent,
          bbEpochFinalizationEntry = Absent,
          bbNonce = computeBlockNonce (genesisLEN sProtocolVersion) 5 (bakerVRFKey sProtocolVersion bakerId),
          bbTransactions = Vec.empty,
          bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
            SBlockHashVersion0 ->
                DerivableBlockHashesV0
                    { dbhv0TransactionOutcomesHash = read "b0972dd7af05ed6feaa40099fffa9c5c5e0ba9741938166cdb57584780688743",
                      dbhv0BlockStateHash = read "d9dd62c227d1cbc0d42da0d90bfc11d61533d058cc54b0745d6a597039dbe0ec"
                    }
            SBlockHashVersion1 ->
                DerivableBlockHashesV1
                    { dbhv1BlockResultHash = read "984ba9867ae9574fea5c7f358fb8e365009f298d83e75136231c480b8db45530"
                    }
        }
  where
    sProtocolVersion = protocolVersion @pv
    bakerId = 3

-- | Compute the 'AccountCreation' from the provided 'AccountCreation'.
getAccAddress :: AccountCreation -> AccountAddress
getAccAddress accCreation = case credential accCreation of
    InitialACWP x -> initialCredentialAccountAddress $ icdiValues x
    NormalACWP x -> credentialAccountAddress $ cdiValues x

-- | Test that two credential deployments (each on their own branch and with same block height) does not:
--  * Alter the state of the parent block (a new child difference map and associated reference is created).
testDeployCredentialBranching ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testDeployCredentialBranching sProtocolVersion =
    it "deploy two credentials in two branches" $
        runTestMonad noBaker testTime (genesisData sProtocolVersion) $ do
            genesisState <- use (lastFinalized . to bpState)
            noGenesisAccs <- length <$> getAccountList genesisState
            let b1 = signedPB testBB1
            succeedReceiveBlock b1
            -- Branch
            let b2 = signedPB testBB2'
            succeedReceiveBlock b2
            -- Another branch.
            let b3 = signedPB testBB3'
            succeedReceiveBlock b3

            sd <- get

            -- Check that only the first credential deployed is present in block b1.
            case sd ^. blockTable . liveMap . at' (getHash b1) of
                Nothing -> liftIO $ assertFailure "failed getting bp1"
                Just bp1 -> do
                    noAccountsBp1 <- length <$> getAccountList (bpState bp1)
                    liftIO $ assertEqual "check that there is one extra account" (noGenesisAccs + 1) noAccountsBp1
                    getAccount (bpState bp1) (getAccAddress cred1) >>= \case
                        Nothing -> liftIO $ assertFailure "Should yield cred1"
                        Just (accIndex, _) -> liftIO $ assertEqual "incorrect account index" noGenesisAccs (fromIntegral accIndex)

                    getAccount (bpState bp1) (getAccAddress cred2) >>= \case
                        Nothing -> return ()
                        Just _ -> liftIO $ assertFailure "cred2 should not be present"

                    getAccount (bpState bp1) (getAccAddress cred3) >>= \case
                        Nothing -> return ()
                        Just _ -> liftIO $ assertFailure "cred3 should not be present"

            -- Check that cred1 and cred2 is present in b2 (but not cred3)
            case sd ^. blockTable . liveMap . at' (getHash b2) of
                Nothing -> liftIO $ assertFailure "failed getting bp1"
                Just bp2 -> do
                    noAccountsBp2 <- length <$> getAccountList (bpState bp2)
                    liftIO $ assertEqual "check that there is one extra account" (noGenesisAccs + 2) noAccountsBp2
                    getAccount (bpState bp2) (getAccAddress cred1) >>= \case
                        Nothing -> liftIO $ assertFailure "Should yield cred1"
                        Just (accIndex, _) -> liftIO $ assertEqual "incorrect account index" noGenesisAccs (fromIntegral accIndex)

                    getAccount (bpState bp2) (getAccAddress cred2) >>= \case
                        Nothing -> liftIO $ assertFailure "Should yield cred2"
                        Just (accIndex, _) -> liftIO $ assertEqual "incorrect account index" (noGenesisAccs + 1) (fromIntegral accIndex)

                    getAccount (bpState bp2) (getAccAddress cred3) >>= \case
                        Nothing -> return ()
                        Just _ -> liftIO $ assertFailure $ "cred3 should not be present: " <> show (getAccAddress cred3)

            -- Check that cred1 and cred3 is present in b3 (but not cred2)
            case sd ^. blockTable . liveMap . at' (getHash b3) of
                Nothing -> liftIO $ assertFailure "failed getting bp1"
                Just bp3 -> do
                    noAccountsBp3 <- length <$> getAccountList (bpState bp3)
                    liftIO $ assertEqual "check that there is one extra account" (noGenesisAccs + 2) noAccountsBp3
                    getAccount (bpState bp3) (getAccAddress cred1) >>= \case
                        Nothing -> liftIO $ assertFailure "Should yield cred1"
                        Just (accIndex, _) -> liftIO $ assertEqual "incorrect account index" noGenesisAccs (fromIntegral accIndex)

                    getAccount (bpState bp3) (getAccAddress cred3) >>= \case
                        Nothing -> liftIO $ assertFailure "Should yield cred3"
                        Just (accIndex, _) -> liftIO $ assertEqual "incorrect account index" (noGenesisAccs + 1) (fromIntegral accIndex)

                    getAccount (bpState bp3) (getAccAddress cred2) >>= \case
                        Nothing -> return ()
                        Just _ -> liftIO $ assertFailure $ "cred2 should not be present: " <> show (getAccAddress cred3)

            -- finalize bp3 and make sure that the state of the lfb matches b3.
            let b4 = signedPB testBB4
            succeedReceiveBlock b4
            let b5 = signedPB testBB5
            succeedReceiveBlock b5

            lfbState <- use (lastFinalized . to bpState)
            noAccountsLfb <- length <$> getAccountList lfbState
            liftIO $ assertEqual "check that there aer two extra accounts (cred 1 and 3)" (noGenesisAccs + 2) noAccountsLfb

            getAccount lfbState (getAccAddress cred1) >>= \case
                Nothing -> liftIO $ assertFailure "Should yield cred1"
                Just (accIndex, _) -> liftIO $ assertEqual "incorrect account index" noGenesisAccs (fromIntegral accIndex)

            getAccount lfbState (getAccAddress cred3) >>= \case
                Nothing -> liftIO $ assertFailure "Should yield cred3"
                Just (accIndex, _) -> liftIO $ assertEqual "incorrect account index" (noGenesisAccs + 1) (fromIntegral accIndex)

            getAccount lfbState (getAccAddress cred2) >>= \case
                Nothing -> return ()
                Just _ -> liftIO $ assertFailure $ "cred2 should not be present: " <> show (getAccAddress cred2)

            -- Check that querying the old bs is not affected by the updated lmdb backed account map.
            noFinal <- length <$> getAccountList genesisState
            liftIO $ assertEqual "There should be the same number of accounts present" noGenesisAccs noFinal
            -- We thaw here so we can use @bsoGetAccountIndex@ for querying account index directly.
            updatableBlockState <- thawBlockState genesisState
            bsoGetAccountIndex updatableBlockState (getAccAddress cred1) >>= \case
                Nothing -> return ()
                Just _ -> liftIO $ assertFailure "cred 1 should not be present."

tests :: Word -> Spec
tests _ = describe "EndToEndTests.CredentialDeployments" $ do
    Common.forEveryProtocolVersionConsensusV1 $ \spv pvString ->
        describe pvString $ do
            testDeployCredential spv
            testDeployCredentialBranching spv
