{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests for creating PLTs.
module SchedulerTests.TokenCreation (tests) where

import Data.Bool.Singletons
import qualified Data.ByteString.Short as BSS
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import qualified SchedulerTests.Helpers as Helpers
import Test.Hspec

import qualified Concordium.Crypto.DummyData as DummyData
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.ID.Types as ID
import qualified Concordium.Types.DummyData as DummyData
import qualified Concordium.Types.ProtocolLevelTokens.CBOR as CBOR
import Concordium.Types.Tokens
import Concordium.Types.Updates

import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Types as Types

dummyAddress2 :: AccountAddress
dummyAddress2 = Helpers.accountAddressFromSeed 2

dummyTokenHolder :: TokenHolder
dummyTokenHolder = HolderAccount dummyAddress2

dummyCborTokenHolder :: CBOR.CborTokenHolder
dummyCborTokenHolder =
    CBOR.CborHolderAccount
        { chaAccount = dummyAddress2,
          chaCoinInfo = Nothing
        }

dummyAccount ::
    (IsAccountVersion av, Blob.MonadBlobStore m) =>
    m (BS.PersistentAccount av)
dummyAccount = Helpers.makeTestAccountFromSeed 20_000_000 1

dummyAccount2 ::
    (IsAccountVersion av, Blob.MonadBlobStore m) =>
    m (BS.PersistentAccount av)
dummyAccount2 = Helpers.makeTestAccountFromSeed 20_000_000 2

-- | Create initial block state
initialBlockState ::
    (IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ dummyAccount,
          dummyAccount2
        ]

initialBlockStateWithCustomKeys ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsPLT pv) =>
    -- | Number of keys
    Int ->
    -- | Which key indices are authorized for CreatePLT
    [UpdateKeyIndex] ->
    -- | Threshold for CreatePLT
    UpdateKeysThreshold ->
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockStateWithCustomKeys numKeys authorizedKeys threshold = do
    accounts <- sequence [dummyAccount, dummyAccount2]
    let auths0 = DummyData.dummyAuthorizations @(AuthorizationsVersionFor pv)
    let createPLT = AccessStructure (Set.fromList authorizedKeys) threshold
    let keys =
            UpdateKeysCollection
                { rootKeys = DummyData.dummyHigherLevelKeys,
                  level1Keys = DummyData.dummyHigherLevelKeys,
                  level2Keys =
                    auths0
                        { asKeys = Vec.fromList (DummyData.deterministicVK <$> [0 .. numKeys - 1]),
                          asCreatePLT = createPLT <$ asCreatePLT auths0
                        }
                }
    Helpers.createTestBlockStateWithAccountsAndKeys accounts keys

testCreatePLT :: forall pv. (IsProtocolVersion pv, PVSupportsPLT pv) => SProtocolVersion pv -> String -> Spec
testCreatePLT _ pvString = describe pvString $ do
    it "Create PLT - no initial supply" $ do
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction = txCreatePLT 1 createPLT1,
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertSuccessWithEvents [TokenCreated{etcPayload = createPLT1}] result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactionsAndAssertions
    it "Create PLT - initial supply" $ do
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction = txCreatePLT 1 createPLT2,
                      biaaAssertion = \result _ -> do
                        return $
                            Helpers.assertSuccessWithEvents
                                [ TokenCreated{etcPayload = createPLT2},
                                  TokenMint{etmTokenId = plt2, etmAmount = TokenAmount 10 0, etmTarget = dummyTokenHolder}
                                ]
                                result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactionsAndAssertions
    it "Create PLT - duplicate" $ do
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction = txCreatePLT 1 createPLT1,
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertSuccessWithEvents [TokenCreated{etcPayload = createPLT1}] result
                    },
                  Helpers.BlockItemAndAssertion
                    { biaaTransaction = txCreatePLT 2 createPLT1,
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertUpdateFailureWithReason (DuplicateTokenId plt1) result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactionsAndAssertions
    it "Create PLT - multiple keys" $ do
        let numKeys = 3
            authorizedKeys = [1, 2]
            threshold = 2
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        txCreatePLTWithKeys
                            1
                            createPLT1
                            [(1, DummyData.deterministicKP 1), (2, DummyData.deterministicKP 2)],
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertSuccessWithEvents [TokenCreated{etcPayload = createPLT1}] result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            (initialBlockStateWithCustomKeys numKeys authorizedKeys threshold)
            transactionsAndAssertions
    it "Create PLT - multiple keys - additional authorized" $ do
        let numKeys = 3
            authorizedKeys = [1, 2]
            threshold = 1
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        txCreatePLTWithKeys
                            1
                            createPLT1
                            [(1, DummyData.deterministicKP 1), (2, DummyData.deterministicKP 2)],
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertSuccessWithEvents [TokenCreated{etcPayload = createPLT1}] result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            (initialBlockStateWithCustomKeys numKeys authorizedKeys threshold)
            transactionsAndAssertions
    it "Create PLT - multiple keys - incorrect signature" $ do
        let numKeys = 3
            authorizedKeys = [1, 2]
            threshold = 2
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        txCreatePLTWithKeys
                            1
                            createPLT1
                            [(1, DummyData.deterministicKP 1), (2, DummyData.deterministicKP 23)],
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertUpdateFailureWithReason IncorrectSignature result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            (initialBlockStateWithCustomKeys numKeys authorizedKeys threshold)
            transactionsAndAssertions
    it "Create PLT - multiple keys - incorrect key" $ do
        let numKeys = 3
            authorizedKeys = [1, 2]
            threshold = 1
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        txCreatePLTWithKeys
                            1
                            createPLT1
                            [(0, DummyData.deterministicKP 0)],
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertUpdateFailureWithReason IncorrectSignature result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            (initialBlockStateWithCustomKeys numKeys authorizedKeys threshold)
            transactionsAndAssertions
    it "Create PLT - multiple keys - additional unauthorized" $ do
        let numKeys = 3
            authorizedKeys = [0, 1]
            threshold = 1
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        txCreatePLTWithKeys
                            1
                            createPLT1
                            [(1, DummyData.deterministicKP 1), (2, DummyData.deterministicKP 2)],
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertUpdateFailureWithReason IncorrectSignature result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            (initialBlockStateWithCustomKeys numKeys authorizedKeys threshold)
            transactionsAndAssertions
    it "Create PLT - multiple keys - first unauthorized" $ do
        let numKeys = 3
            authorizedKeys = [1, 2]
            threshold = 1
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        txCreatePLTWithKeys
                            1
                            createPLT1
                            [(0, DummyData.deterministicKP 0)],
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertUpdateFailureWithReason IncorrectSignature result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            (initialBlockStateWithCustomKeys numKeys authorizedKeys threshold)
            transactionsAndAssertions
    it "Create PLT - multiple keys - first incorrect key" $ do
        let numKeys = 3
            authorizedKeys = [0, 1, 2]
            threshold = 1
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        txCreatePLTWithKeys
                            1
                            createPLT1
                            [(0, DummyData.deterministicKP 1)],
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertUpdateFailureWithReason IncorrectSignature result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            (initialBlockStateWithCustomKeys numKeys authorizedKeys threshold)
            transactionsAndAssertions
  where
    txCreatePLTWithKeys ctSeqNumber createPLT ctKeys =
        Runner.ChainUpdateTx
            Runner.ChainUpdateTransaction
                { ctEffectiveTime = 0,
                  ctTimeout = DummyData.dummyMaxTransactionExpiryTime,
                  ctPayload = Types.CreatePLTUpdatePayload createPLT,
                  ..
                }
    txCreatePLT ctSeqNumber createPLT =
        txCreatePLTWithKeys
            ctSeqNumber
            createPLT
            [(0, DummyData.dummyAuthorizationKeyPair)]
    dummyHash = Hash.hashShort BSS.empty
    plt1 = Types.TokenId "PLT1"
    plt2 = Types.TokenId "PLT2"
    params1 =
        CBOR.TokenInitializationParameters
            { tipName = "Protocol-level token",
              tipMetadata = CBOR.createTokenMetadataUrl "https://plt.token",
              tipGovernanceAccount = dummyCborTokenHolder,
              tipAllowList = False,
              tipDenyList = False,
              tipInitialSupply = Nothing,
              tipMintable = True,
              tipBurnable = True
            }
    toTokenParam = Types.TokenParameter . BSS.toShort . CBOR.tokenInitializationParametersToBytes
    createPLT1 =
        Types.CreatePLT
            { _cpltTokenModule = TokenModuleRef dummyHash,
              _cpltTokenId = plt1,
              _cpltInitializationParameters = toTokenParam params1,
              _cpltDecimals = 0
            }
    params2 =
        CBOR.TokenInitializationParameters
            { tipName = "Protocol-level token",
              tipMetadata = CBOR.createTokenMetadataUrl "https://plt.token",
              tipGovernanceAccount = dummyCborTokenHolder,
              tipAllowList = False,
              tipDenyList = False,
              tipInitialSupply = Just (TokenAmount 10 0),
              tipMintable = True,
              tipBurnable = True
            }
    createPLT2 =
        Types.CreatePLT
            { _cpltTokenModule = TokenModuleRef dummyHash,
              _cpltTokenId = plt2,
              _cpltInitializationParameters = toTokenParam params2,
              _cpltDecimals = 0
            }

tests :: Spec
tests =
    describe "Token creation" . sequence_ $
        Helpers.forEveryProtocolVersion testCases
  where
    testCases :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> String -> Spec
    testCases spv pvString = case sSupportsPLT (sAccountVersionFor spv) of
        STrue -> testCreatePLT spv pvString
        SFalse -> return ()
