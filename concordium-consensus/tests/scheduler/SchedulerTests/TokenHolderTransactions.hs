{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test of token holder transactions
module SchedulerTests.TokenHolderTransactions (tests) where

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.ID.Types as ID
import qualified Concordium.Types.ProtocolLevelTokens.CBOR as CBOR

import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.ProtocolLevelTokens.Module (tokenModuleV0Ref)
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Types.DummyData as DummyData

import Data.Bool.Singletons
import qualified Data.ByteString.Short as BSS
import Data.String
import qualified SchedulerTests.Helpers as Helpers
import Test.Hspec
import qualified Data.Map as Map

dummyKP :: SigScheme.KeyPair
dummyKP = Helpers.keyPairFromSeed 1

dummyAddress :: AccountAddress
dummyAddress = Helpers.accountAddressFromSeed 1

dummyAddress2 :: AccountAddress
dummyAddress2 = Helpers.accountAddressFromSeed 2

dummyCborAccountAddress :: CBOR.CborAccountAddress
dummyCborAccountAddress =
    CBOR.CborAccountAddress
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

-- | Test the following sequence of operations:
--   - Attempt a token holder transaction for a non-existent token (TokenId: GTU). (Fails: non-existent token)
--   - Create a token with the TokenId GTU. (Succeeds)
--   - Attempt a token holder transaction for GTU. (Fails: CBOR deserialization)
testTokenHolder ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsPLT pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testTokenHolder _ pvString =
    specify (pvString ++ ": Token holder operations") $ do
        let transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        Runner.AccountTx $
                            Runner.TJSON
                                { payload =
                                    Runner.TokenUpdate
                                        { tuTokenId = gtu,
                                          tuOperations = Types.TokenParameter BSS.empty
                                        },
                                  metadata = makeDummyHeader dummyAddress 1 1_000,
                                  keys = [(0, [(0, dummyKP)])]
                                },
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertRejectWithReason (NonExistentTokenId gtu) result
                    },
                  Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        Runner.ChainUpdateTx $
                            Runner.ChainUpdateTransaction
                                { ctSeqNumber = 1,
                                  ctEffectiveTime = 0,
                                  ctTimeout = DummyData.dummyMaxTransactionExpiryTime,
                                  ctPayload = plt,
                                  ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)]
                                },
                      biaaAssertion = \result _ -> do
                        return $ Helpers.assertSuccessWithEvents [gtuEvent] result
                    },
                  Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        Runner.AccountTx $
                            Runner.TJSON
                                { payload =
                                    Runner.TokenUpdate
                                        { tuTokenId = gtu2,
                                          tuOperations = Types.TokenParameter BSS.empty
                                        },
                                  metadata = makeDummyHeader dummyAddress 2 1_000,
                                  keys = [(0, [(0, dummyKP)])]
                                },
                      biaaAssertion = \result _ -> do
                        return $
                            Helpers.assertRejectWithReason
                                ( TokenUpdateTransactionFailed
                                    (TokenModuleRejectReason{tmrrTokenId = gtu, tmrrType = errType, tmrrDetails = Just cborFail})
                                )
                                result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactionsAndAssertions
  where
    gtu = Types.TokenId $ fromString "Gtu"
    gtu2 = Types.TokenId $ fromString "gtU"
    params =
        CBOR.TokenInitializationParameters
            { tipName = Just "Protocol-level token",
              tipMetadata = Just $ CBOR.createTokenMetadataUrl "https://plt.token",
              tipGovernanceAccount = Just dummyCborAccountAddress,
              tipAllowList = Just True,
              tipDenyList = Just False,
              tipInitialSupply = Nothing,
              tipMintable = Just True,
              tipBurnable = Just True,
              tipAdditional = Map.empty
            }
    tp = Types.TokenParameter $ BSS.toShort $ CBOR.tokenInitializationParametersToBytes params
    createPLT = Types.CreatePLT gtu tokenModuleV0Ref 0 tp
    plt = Types.CreatePLTUpdatePayload createPLT
    gtuEvent = TokenCreated{etcPayload = createPLT}
    -- This is CBOR-encoding of {"cause": "DeserialiseFailure 0 \"end of input\""}
    cborFail = Types.TokenEventDetails $ BSS.pack [161, 101, 99, 97, 117, 115, 101, 120, 35, 68, 101, 115, 101, 114, 105, 97, 108, 105, 115, 101, 70, 97, 105, 108, 117, 114, 101, 32, 48, 32, 34, 101, 110, 100, 32, 111, 102, 32, 105, 110, 112, 117, 116, 34]
    errType = Types.TokenEventType $ fromString "deserializationFailure"

tests :: Spec
tests =
    describe "Token holder transactions" $
        sequence_ $
            Helpers.forEveryProtocolVersion testCases
  where
    testCases :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> String -> Spec
    testCases spv pvString =
        case sSupportsPLT (sAccountVersionFor spv) of
            STrue -> testTokenHolder spv pvString
            SFalse -> return ()
