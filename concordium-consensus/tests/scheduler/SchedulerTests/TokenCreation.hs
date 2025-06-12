{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests for creating PLTs.
module SchedulerTests.TokenCreation (tests) where

import Data.Bool.Singletons
import qualified Data.ByteString.Short as BSS
import qualified SchedulerTests.Helpers as Helpers
import Test.Hspec

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.ID.Types as ID
import qualified Concordium.Types.DummyData as DummyData
import qualified Concordium.Types.ProtocolLevelTokens.CBOR as CBOR
import Concordium.Types.Tokens

import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Types as Types

dummyAddress2 :: AccountAddress
dummyAddress2 = Helpers.accountAddressFromSeed 2

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
                                  TokenMint{etmTokenId = plt2, etmAmount = TokenAmount 10 0, etmTarget = dummyAddress2}
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
  where
    txCreatePLT ctSeqNumber createPLT =
        Runner.ChainUpdateTx
            Runner.ChainUpdateTransaction
                { ctEffectiveTime = 0,
                  ctTimeout = DummyData.dummyMaxTransactionExpiryTime,
                  ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)],
                  ctPayload = Types.CreatePLTUpdatePayload createPLT,
                  ..
                }
    dummyHash = Hash.hashShort BSS.empty
    plt1 = Types.TokenId "PLT1"
    plt2 = Types.TokenId "PLT2"
    params1 =
        CBOR.TokenInitializationParameters
            { tipName = "Protocol-level token",
              tipMetadata = CBOR.createTokenMetadataUrl "https://plt.token",
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
              _cpltGovernanceAccount = dummyAddress2,
              _cpltDecimals = 0
            }
    params2 =
        CBOR.TokenInitializationParameters
            { tipName = "Protocol-level token",
              tipMetadata = CBOR.createTokenMetadataUrl "https://plt.token",
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
              _cpltGovernanceAccount = dummyAddress2,
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
