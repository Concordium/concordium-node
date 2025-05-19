{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Types.DummyData as DummyData

import Concordium.Scheduler.DummyData

import Data.Bool.Singletons
import qualified Data.ByteString.Short as BSS
import Data.String
import qualified SchedulerTests.Helpers as Helpers
import Test.Hspec

dummyKP :: SigScheme.KeyPair
dummyKP = Helpers.keyPairFromSeed 1

dummyAddress :: AccountAddress
dummyAddress = Helpers.accountAddressFromSeed 1

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

-- | Test removing a delegator even if the stake is over the threshold.
testTokenHolder ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsPLT pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testTokenHolder _ pvString =
    specify (pvString ++ ": Token holder operations") $ do
        let transactionsAndAssertions :: [Helpers.AnyTransactionAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.AnyTransactionAndAssertion
                    { ataaTransaction =
                        Runner.AccountTx $
                            Runner.TJSON
                                { payload =
                                    Runner.TokenHolder
                                        { thTokenSymbol = gtu,
                                          thOperations = Types.TokenParameter BSS.empty
                                        },
                                  metadata = makeDummyHeader dummyAddress 1 1_000,
                                  keys = [(0, [(0, dummyKP)])]
                                },
                      ataaAssertion = \result _ -> do
                        return $ Helpers.assertRejectWithReason (NonExistentTokenId gtu) result
                    },
                  Helpers.AnyTransactionAndAssertion
                    { ataaTransaction =
                        Runner.ChainTx $
                            Runner.ChainTransaction
                                { ctSeqNumber = 1,
                                  ctEffectiveTime = 0,
                                  ctTimeout = DummyData.dummyMaxTransactionExpiryTime,
                                  ctPayload = plt,
                                  ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)]
                                },
                      ataaAssertion = \result _ -> do
                        return $ Helpers.assertSuccessWithEvents [gtuEvent] result
                    },
                  Helpers.AnyTransactionAndAssertion
                    { ataaTransaction =
                        Runner.AccountTx $
                            Runner.TJSON
                                { payload =
                                    Runner.TokenHolder
                                        { thTokenSymbol = gtu,
                                          thOperations = Types.TokenParameter BSS.empty
                                        },
                                  metadata = makeDummyHeader dummyAddress 2 1_000,
                                  keys = [(0, [(0, dummyKP)])]
                                },
                      ataaAssertion = \result _ -> do
                        return $
                            Helpers.assertRejectWithReason
                                ( TokenHolderTransactionFailed
                                    (TokenModuleRejectReason{tmrrTokenSymbol = gtu, tmrrType = errType, tmrrDetails = Just cborFail})
                                )
                                result
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStatesAnyTransaction
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactionsAndAssertions
  where
    dummyHash = Hash.hashShort BSS.empty
    gtu = Types.TokenId $ fromString "GTU"
    params =
        CBOR.TokenInitializationParameters
            { tipName = "Protocol-level token",
              tipMetadata = "https://plt.token",
              tipAllowList = True,
              tipDenyList = False,
              tipInitialSupply = Nothing,
              tipMintable = True,
              tipBurnable = True
            }
    tp = Types.TokenParameter $ BSS.toShort $ CBOR.tokenInitializationParametersToBytes params
    plt = Types.CreatePLTUpdatePayload $ Types.CreatePLT gtu (TokenModuleRef dummyHash) dummyAddress2 0 tp
    gtuEvent = UpdateEnqueued{ueEffectiveTime = 0, uePayload = plt}
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
