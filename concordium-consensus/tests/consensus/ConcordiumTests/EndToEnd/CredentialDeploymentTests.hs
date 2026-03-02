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
    makeBlockItem (tt + 1) cred1
  where
    tt = utcTimeToTransactionTime testTime

-- | A credential deployment transaction yielding cred2.
credBi2 :: BlockItem
credBi2 =
    makeBlockItem (tt + 1) cred2
  where
    tt = utcTimeToTransactionTime testTime

-- | A credential deployment transaction yielding cred3
credBi3 :: BlockItem
credBi3 =
    makeBlockItem (tt + 1) cred3
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
                    { dbhv1BlockResultHash = case sProtocolVersion of
                        SP7 -> read "dca0b796dbfac96e7043942548c9d7cd470226740e2bdc793107de026d423e8d"
                        SP8 -> read "9edf091441b19468d82a637c270472b0474592f3089e56415e4982351b095d35"
                        SP9 -> read "ad588c91476f5865dabf2ffe9b6954c924479aa0a2ef4057a5b79dbc110b1219"
                        SP10 -> read "5272b398aa5ade5ef14c6be41d586537f5212d96ea609b414bf2051e0c3780f7"
                        SP11 -> read "5272b398aa5ade5ef14c6be41d586537f5212d96ea609b414bf2051e0c3780f7"
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
                    { dbhv1BlockResultHash = case sProtocolVersion of
                        SP7 -> read "3812ac3fa24c5676ea40c5879d9e88cd60e8af79d6ad7847c59df0880baacd01"
                        SP8 -> read "7ead7edff60ac23771f15052278304e3e2c9186826439ec7d04e28e55676f41b"
                        SP9 -> read "29d974384b047eaa8cb5b809dd37e9fe617e046ad22b7f9dbe605cdac9cf8e40"
                        SP10 -> read "29d974384b047eaa8cb5b809dd37e9fe617e046ad22b7f9dbe605cdac9cf8e40"
                        SP11 -> read "29d974384b047eaa8cb5b809dd37e9fe617e046ad22b7f9dbe605cdac9cf8e40"
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
                    { dbhv1BlockResultHash = case sProtocolVersion of
                        SP7 -> read "cad9520a6cfac6e3f08a75394d68dcfbb9fa1a857d79e0048be5c7752ca72907"
                        SP8 -> read "c8b3fc868c79703945638c709c9e2d03b67c3f70b023aac8ae5b980b41181726"
                        SP9 -> read "0282c255df3cb95180050ae3ee8838c0ab303fa7fe4e5e754ecf5d4c8db5152a"
                        SP10 -> read "0282c255df3cb95180050ae3ee8838c0ab303fa7fe4e5e754ecf5d4c8db5152a"
                        SP11 -> read "0282c255df3cb95180050ae3ee8838c0ab303fa7fe4e5e754ecf5d4c8db5152a"
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
                    { dbhv1BlockResultHash = case sProtocolVersion of
                        SP7 -> read "c97b0cc90e29c62eb0e094696ef38891799360c0a89ad69b418205ab3b15b17a"
                        SP8 -> read "3abd796108d6fdcdf8c4361973d7152973cad3695b58b0c92a4e5021c0f80e33"
                        SP9 -> read "e10fe99a06aec8675f442ef0232ff0af9106ad6f5845335513603bcb4f3ff707"
                        SP10 -> read "0182d9155fb80b22fa43b8f7e1d9da389209bfa27340ec5a823a3f587e4455f9"
                        SP11 -> read "0182d9155fb80b22fa43b8f7e1d9da389209bfa27340ec5a823a3f587e4455f9"
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
                    { dbhv1BlockResultHash = case sProtocolVersion of
                        SP7 -> read "f14475f1014ff13d2acf98f56a8f01823c1ae52ae01d53a8004f0e728c538357"
                        SP8 -> read "9e46988a9afd8470e25c33f2133d2c10cbb38050979957f200b4aca072e3c932"
                        SP9 -> read "075e1732e475a84b89f1ba89c06d58df7d7e42936dc16e0cf3d8b02dacf27c52"
                        SP10 -> read "2a9b336c419c9e64dc7db6735a297119649c4b579ac4d32ac58e1ab09302d17c"
                        SP11 -> read "2a9b336c419c9e64dc7db6735a297119649c4b579ac4d32ac58e1ab09302d17c"
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
                    { dbhv1BlockResultHash = case sProtocolVersion of
                        SP7 -> read "9f757ba3e2fb79512a3c199fbd8b6c0a45eaef0fd3a5cc6a3ca78cf8f7ae18a6"
                        SP8 -> read "5bdf447992d82321a921bca9eeb6211bf3a290029164976885cf6b2fd14d923c"
                        SP9 -> read "ff90510a80285645170b5ff4614af366de2d8898e2bee111cae61e05ad640ada"
                        SP10 -> read "ff90510a80285645170b5ff4614af366de2d8898e2bee111cae61e05ad640ada"
                        SP11 -> read "ff90510a80285645170b5ff4614af366de2d8898e2bee111cae61e05ad640ada"
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
                    { dbhv1BlockResultHash = case sProtocolVersion of
                        SP7 -> read "aca6161199f608947b265a048fd6dc404d31424a2a2a86d42be42310f6fd22a0"
                        SP8 -> read "e704583a45aec569aca9039e977d9a3a7c2db8bdfd650532182600b6a19cbb70"
                        SP9 -> read "6ad235a2db0349340044197b9c23da565f6bf4dd1ad40b89eab5bae7bcab0997"
                        SP10 -> read "6ad235a2db0349340044197b9c23da565f6bf4dd1ad40b89eab5bae7bcab0997"
                        SP11 -> read "6ad235a2db0349340044197b9c23da565f6bf4dd1ad40b89eab5bae7bcab0997"
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
