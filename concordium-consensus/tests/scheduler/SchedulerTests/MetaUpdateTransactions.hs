{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests for meta-update transactions.
module SchedulerTests.MetaUpdateTransactions (tests) where

import Control.Monad
import Data.Bool.Singletons
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Word
import Test.HUnit
import Test.Hspec

import qualified Concordium.Cost as Cost
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.ID.Types as ID
import qualified Concordium.Types.ProtocolLevelTokens.CBOR as CBOR
import Concordium.Types.Queries.Tokens
import Concordium.Types.Tokens

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.ProtocolLevelTokens.Module (tokenModuleV0Ref)
import Concordium.Scheduler.ProtocolLevelTokens.Queries
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Types.DummyData as DummyData

import qualified SchedulerTests.Helpers as Helpers

dummyKP :: SigScheme.KeyPair
dummyKP = Helpers.keyPairFromSeed 1

-- | Address of 'dummyAccount'.
dummyAddress :: AccountAddress
dummyAddress = Helpers.accountAddressFromSeed 1

-- | Address of 'dummyAccount2'.
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

-- | Signing keys for 'dummyAccount'.
keys1 :: [(CredentialIndex, [(KeyIndex, SigScheme.KeyPair)])]
keys1 = [(0, [(0, dummyKP)])]

-- | Create initial block state
initialBlockState ::
    (IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ dummyAccount,
          dummyAccount2
        ]

makeMetaTx ::
    AccountAddress ->
    Nonce ->
    Energy ->
    [(CredentialIndex, [(KeyIndex, SigScheme.KeyPair)])] ->
    [CBOR.MetaUpdateOperation] ->
    Runner.BlockItemDescription
makeMetaTx sendAddr nonce nrg keys ops =
    Runner.AccountTx
        Runner.TJSON
            { payload = Runner.MetaUpdate{muOperations = mkOps ops},
              metadata = makeDummyHeader sendAddr nonce nrg,
              keys = keys
            }
  where
    mkOps =
        Types.rawCborFromBytes
            . CBOR.metaUpdateTransactionToBytes
            . CBOR.MetaUpdateTransaction
            . Seq.fromList

-- | Test an empty meta-update transaction at a given protocol version.
--  The transaction should be accepted if and only if the protocol version supports meta-update
--  transactions.
testMetaUpdateSupport :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testMetaUpdateSupport spv = it desc $ do
    Helpers.runSchedulerTestAssertIntermediateStates
        @pv
        Helpers.defaultTestConfig
        initialBlockState
        transactionsAndAssertions
  where
    desc =
        "Empty meta-update operation "
            ++ (if supportsMetaUpdate spv then "" else "not ")
            ++ "supported"
    -- Base cost: payload size = 6 = 1 (type) + 4 (CBOR size) + 1 (CBOR encoding of empty list)
    costFail = Cost.baseCost (transactionHeaderSize + 6) 1
    costSuccess = costFail + Cost.metaUpdateBaseCost
    transactionsAndAssertions =
        [ Helpers.BlockItemAndAssertion
            { biaaTransaction = makeMetaTx dummyAddress 1 1000 keys1 [],
              biaaAssertion = \result _newState -> do
                return $
                    if supportsMetaUpdate spv
                        then do
                            Helpers.assertSuccessWithEvents [] result
                            assertEqual "Used energy" costSuccess (Helpers.srUsedEnergy result)
                        else do
                            Helpers.assertRejectWithReason SerializationFailure result
                            assertEqual "Used energy" costFail (Helpers.srUsedEnergy result)
            }
        ]

-- | Helper for creating a PLT with a 'Helpers.BlockItemAndAssertion'.
createPltBiaa :: TokenId -> Word8 -> CBOR.TokenInitializationParameters -> UpdateSequenceNumber -> Helpers.BlockItemAndAssertion pv
createPltBiaa pltName numDecimals initParam seqNum =
    Helpers.BlockItemAndAssertion
        { biaaTransaction =
            Runner.ChainUpdateTx $
                Runner.ChainUpdateTransaction
                    { ctSeqNumber = seqNum,
                      ctEffectiveTime = 0,
                      ctTimeout = DummyData.dummyMaxTransactionExpiryTime,
                      ctPayload = Types.CreatePLTUpdatePayload createPLT,
                      ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)]
                    },
          biaaAssertion = \result _ -> do
            return $
                Helpers.assertSuccessWithEvents
                    ( [TokenCreated{etcPayload = createPLT}]
                        <> [ TokenMint
                                { etmTokenId = pltName,
                                  etmTarget = HolderAccount dummyAddress,
                                  etmAmount = mintAmt
                                }
                           | Just mintAmt <- [CBOR.tipInitialSupply initParam]
                           ]
                    )
                    result
        }
  where
    createPLT = Types.CreatePLT pltName tokenModuleV0Ref numDecimals tp
    tp = Types.rawCborFromBytes $ CBOR.tokenInitializationParametersToBytes initParam

-- | Create a "pltX" token.
createPlt1 :: UpdateSequenceNumber -> Helpers.BlockItemAndAssertion pv
createPlt1 =
    createPltBiaa (TokenId "pltX") 2 $
        CBOR.TokenInitializationParameters
            { tipName = Just "Test PLT 1",
              tipMetadata = Just $ CBOR.createTokenMetadataUrl "https://pltX.token",
              tipGovernanceAccount = Just $ CBOR.accountTokenHolder dummyAddress,
              tipAllowList = Nothing,
              tipDenyList = Nothing,
              tipInitialSupply = Just (TokenAmount 10000 2),
              tipMintable = Just True,
              tipBurnable = Just True,
              tipAdditional = Map.empty
            }

-- | Create a "pltY" token.
createPlt2 :: UpdateSequenceNumber -> Helpers.BlockItemAndAssertion pv
createPlt2 =
    createPltBiaa (TokenId "pltY") 0 $
        CBOR.TokenInitializationParameters
            { tipName = Just "Test PLT 2",
              tipMetadata = Just $ CBOR.createTokenMetadataUrl "https://pltY.token",
              tipGovernanceAccount = Just $ CBOR.accountTokenHolder dummyAddress,
              tipAllowList = Just True,
              tipDenyList = Just True,
              tipInitialSupply = Nothing,
              tipMintable = Just True,
              tipBurnable = Just True,
              tipAdditional = Map.empty
            }

-- | An alias for an 'AccountAddress' that is distinct.
distinctAlias :: AccountAddress -> AccountAddress
distinctAlias addr
    | alias == addr = alias2
    | otherwise = alias
  where
    alias = createAlias addr 0
    alias2 = createAlias addr 1

-- | A collection of 'CBOR.MetaUpdateOperations' for testing.
metaUpdateMultiOperation :: [CBOR.MetaUpdateOperation]
metaUpdateMultiOperation =
    [ CBOR.MetaTokenUpdate (TokenId "pltX") $
        CBOR.TokenTransfer $
            CBOR.TokenTransferBody
                { ttAmount = TokenAmount 100 2,
                  ttRecipient = CBOR.accountTokenHolder dummyAddress2,
                  ttMemo = Nothing
                },
      CBOR.MetaTokenUpdate (TokenId "pltY") $
        CBOR.TokenMint $
            TokenAmount 100000 0,
      CBOR.MetaTokenUpdate (TokenId "pltX") $
        CBOR.TokenPause,
      CBOR.MetaTokenUpdate (TokenId "pltY") $
        CBOR.TokenAddAllowList (CBOR.accountTokenHolderShort dummyAddress2),
      CBOR.MetaTokenUpdate (TokenId "pltY") $
        CBOR.TokenAddDenyList (CBOR.accountTokenHolder (distinctAlias dummyAddress)),
      CBOR.MetaTokenUpdate (TokenId "pltY") $
        CBOR.TokenAddAllowList (CBOR.accountTokenHolder dummyAddress),
      CBOR.MetaTokenUpdate (TokenId "PLTY") $
        CBOR.TokenRemoveDenyList (CBOR.accountTokenHolder dummyAddress),
      CBOR.MetaTokenUpdate (TokenId "pltY") $
        CBOR.TokenTransfer $
            CBOR.TokenTransferBody
                { ttAmount = TokenAmount 2200 0,
                  ttRecipient = CBOR.accountTokenHolder dummyAddress2,
                  ttMemo = Just $ CBOR.CBORMemo (Memo "\xa0")
                },
      CBOR.MetaTokenUpdate (TokenId "pltX") $
        CBOR.TokenUnpause,
      CBOR.MetaTokenUpdate (TokenId "PltX") $
        CBOR.TokenBurn $
            TokenAmount 10 2,
      CBOR.MetaTokenUpdate (TokenId "plty") $
        CBOR.TokenRemoveAllowList (CBOR.accountTokenHolderShort dummyAddress)
    ]

-- | The expected events from executing 'metaUpdateMultiOperations'.
metaUpdateMultiEvents :: [Event]
metaUpdateMultiEvents =
    [ TokenTransfer
        { ettTokenId = pltX,
          ettFrom = holder1,
          ettTo = holder2,
          ettAmount = TokenAmount{taValue = 100, taDecimals = 2},
          ettMemo = Nothing,
          ettFromLock = Nothing,
          ettToLock = Nothing
        },
      TokenMint
        { etmTokenId = pltY,
          etmTarget = holder1,
          etmAmount = TokenAmount{taValue = 100000, taDecimals = 0}
        },
      TokenModuleEvent
        { etmeTokenId = pltX,
          etmeType = TokenEventType "pause",
          etmeDetails = CBOR.emptyEventDetails
        },
      TokenModuleEvent
        { etmeTokenId = pltY,
          etmeType = TokenEventType "addAllowList",
          etmeDetails = CBOR.encodeTargetDetails (CBOR.accountTokenHolderShort dummyAddress2)
        },
      TokenModuleEvent
        { etmeTokenId = pltY,
          etmeType = TokenEventType "addDenyList",
          etmeDetails = CBOR.encodeTargetDetails (CBOR.accountTokenHolder (distinctAlias dummyAddress))
        },
      TokenModuleEvent
        { etmeTokenId = pltY,
          etmeType = TokenEventType "addAllowList",
          etmeDetails = CBOR.encodeTargetDetails (CBOR.accountTokenHolder dummyAddress)
        },
      TokenModuleEvent
        { etmeTokenId = pltY,
          etmeType = TokenEventType "removeDenyList",
          etmeDetails = CBOR.encodeTargetDetails (CBOR.accountTokenHolder dummyAddress)
        },
      TokenTransfer
        { ettTokenId = pltY,
          ettFrom = holder1,
          ettTo = holder2,
          ettAmount = TokenAmount{taValue = 2200, taDecimals = 0},
          ettMemo = Just (Memo "\xa0"),
          ettFromLock = Nothing,
          ettToLock = Nothing
        },
      TokenModuleEvent
        { etmeTokenId = pltX,
          etmeType = TokenEventType "unpause",
          etmeDetails = CBOR.emptyEventDetails
        },
      TokenBurn
        { etbTokenId = pltX,
          etbTarget = holder1,
          etbAmount = TokenAmount{taValue = 10, taDecimals = 2}
        },
      TokenModuleEvent
        { etmeTokenId = pltY,
          etmeType = TokenEventType "removeAllowList",
          etmeDetails = CBOR.encodeTargetDetails (CBOR.accountTokenHolderShort dummyAddress)
        }
    ]
  where
    pltX = TokenId "pltX"
    pltY = TokenId "pltY"
    holder1 = HolderAccount dummyAddress
    holder2 = HolderAccount dummyAddress2

-- | Test a meta-update transaction that consists of multiple steps and involves multiple PLTs.
testMetaUpdateMulti :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testMetaUpdateMulti spv = case sSupportsPLT (sAccountVersionFor spv) of
    SFalse -> return ()
    STrue ->
        when (supportsMetaUpdate spv) $
            it "Multi-token multi-step meta-update" $
                Helpers.runSchedulerTestAssertIntermediateStates
                    @pv
                    Helpers.defaultTestConfig
                    initialBlockState
                    transactionsAndAssertions
  where
    transactionsAndAssertions :: (PVSupportsPLT pv) => [Helpers.BlockItemAndAssertion pv]
    transactionsAndAssertions =
        [ createPlt1 1,
          createPlt2 2,
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                makeMetaTx dummyAddress 1 10000 keys1 metaUpdateMultiOperation,
              biaaAssertion = \result newST -> do
                st <- BS.freezeBlockState newST
                tiX <- queryTokenInfo (TokenId "pltX") st
                tiY <- queryTokenInfo (TokenId "pltY") st
                acc1 <- fromJust <$> BS.getAccount st dummyAddress
                ai1 <- queryAccountTokens acc1 st
                acc2 <- fromJust <$> BS.getAccount st dummyAddress2
                ai2 <- queryAccountTokens acc2 st
                return $ do
                    Helpers.assertSuccessWithEvents metaUpdateMultiEvents result
                    assertEqual "used energy" 1803 (Helpers.srUsedEnergy result)
                    assertEqual
                        "pltX supply"
                        (Right $ TokenAmount 9990 2)
                        (tsTotalSupply . tiTokenState <$> tiX)
                    assertEqual
                        "pltY supply"
                        (Right $ TokenAmount 100000 0)
                        (tsTotalSupply . tiTokenState <$> tiY)
                    assertEqual
                        "account 1 tokens"
                        [ Token
                            (TokenId "pltX")
                            ( TokenAccountState
                                { moduleAccountState = Just "\xa0",
                                  balance = TokenAmount 9890 2
                                }
                            ),
                          Token
                            (TokenId "pltY")
                            ( TokenAccountState
                                { moduleAccountState =
                                    Just
                                        "\xa2\x68\
                                        \denyList\xf4\x69\
                                        \allowList\xf4",
                                  balance =
                                    TokenAmount 97800 0
                                }
                            )
                        ]
                        ai1
                    assertEqual
                        "account 2 tokens"
                        [ Token
                            (TokenId "pltX")
                            ( TokenAccountState
                                { moduleAccountState = Just "\xa0",
                                  balance = TokenAmount 100 2
                                }
                            ),
                          Token
                            (TokenId "pltY")
                            ( TokenAccountState
                                { moduleAccountState =
                                    Just
                                        "\xa2\x68\
                                        \denyList\xf4\x69\
                                        \allowList\xf5",
                                  balance = TokenAmount 2200 0
                                }
                            )
                        ]
                        ai2
            }
        ]

-- | Scheduler tests for meta-update transactions.
tests :: Spec
tests = parallel $
    describe "Meta-update transactions" $
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                describe pvString $ do
                    testMetaUpdateSupport spv
                    testMetaUpdateMulti spv
