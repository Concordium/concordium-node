{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests for unscoped Token Update transactions.
module SchedulerTests.TokenUpdateTransactions (tests) where

import Control.Monad
import Data.Bool.Singletons
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Maybe
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

makeUnscopedTx :: AccountAddress -> Nonce -> Energy -> [(CredentialIndex, [(KeyIndex, SigScheme.KeyPair)])] -> ByteString -> Runner.BlockItemDescription
makeUnscopedTx sendAddr nonce nrg keys ops =
    Runner.AccountTx
        Runner.TJSON
            { payload = Runner.UnscopedTokenUpdate{utuOperations = Types.rawCborFromBytes ops},
              metadata = makeDummyHeader sendAddr nonce nrg,
              keys = keys
            }

-- | Test an empty unscoped Token Update transaction at a given protocol version.
--  The transaction should be accepted if and only if the protocol version supports unscoped Token Update
--  transactions.
testUnscopedSupport :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testUnscopedSupport spv = it desc $ do
    Helpers.runSchedulerTestAssertIntermediateStates
        @pv
        Helpers.defaultTestConfig
        initialBlockState
        transactionsAndAssertions
  where
    desc =
        "Empty unscoped Token Update operation "
            ++ (if supportsUnscopedTokenUpdate spv then "" else "not ")
            ++ "supported"
    -- Base cost: payload size = 7 = 1 (type) + 1 (empty token ID) + 4 (CBOR size) + 1 (CBOR encoding of empty list)
    costFail = Cost.baseCost (transactionHeaderSize + 7) 1
    costSuccess = costFail + Cost.tokenUpdateBaseCost
    transactionsAndAssertions =
        [ Helpers.BlockItemAndAssertion
            { biaaTransaction = makeUnscopedTx dummyAddress 1 1000 keys1 "\x80",
              biaaAssertion = \result _newState -> do
                return $
                    if supportsUnscopedTokenUpdate spv
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

-- | Opaque multi-token operations verified with the Rust CBOR decoder and encoder.
-- Diagnostic CBOR notation:
-- [
--   {"tokenTransfer": {
--     "token": "pltX", "amount": 4([-2, 100]),
--     "recipient": 40307({1: 40305({1: 919}),
--       3: h'170086c8ae4ab9a4c8158b907fdd731935fe99dcabce1fa6f3da0991dde82c50'})
--   }},
--   {"tokenMint": {"token": "pltY", "amount": 4([0, 100000])}},
--   {"tokenPause": {"token": "pltX"}},
--   {"tokenAddAllowList": {
--     "token": "pltY", "target": 40307({
--       3: h'170086c8ae4ab9a4c8158b907fdd731935fe99dcabce1fa6f3da0991dde82c50'})
--   }},
--   {"tokenAddDenyList": {
--     "token": "pltY", "target": 40307({1: 40305({1: 919}),
--       3: h'e26c23d707abfc3a1abb11ca6a286ddcf583febf9c7dda634e304bd059000000'})
--   }},
--   {"tokenAddAllowList": {
--     "token": "pltY", "target": 40307({1: 40305({1: 919}),
--       3: h'e26c23d707abfc3a1abb11ca6a286ddcf583febf9c7dda634e304bd059f588e3'})
--   }},
--   {"tokenRemoveDenyList": {
--     "token": "PLTY", "target": 40307({1: 40305({1: 919}),
--       3: h'e26c23d707abfc3a1abb11ca6a286ddcf583febf9c7dda634e304bd059f588e3'})
--   }},
--   {"tokenTransfer": {
--     "memo": 24(h'a0'), "token": "pltY", "amount": 4([0, 2200]),
--     "recipient": 40307({1: 40305({1: 919}),
--       3: h'170086c8ae4ab9a4c8158b907fdd731935fe99dcabce1fa6f3da0991dde82c50'})
--   }},
--   {"tokenUnpause": {"token": "pltX"}},
--   {"tokenBurn": {"token": "PltX", "amount": 4([-2, 10])}},
--   {"tokenRemoveAllowList": {
--     "token": "plty", "target": 40307({
--       3: h'e26c23d707abfc3a1abb11ca6a286ddcf583febf9c7dda634e304bd059f588e3'})
--   }}
-- ]
unscopedMultiOperations :: ByteString
unscopedMultiOperations =
    "\x8b\xa1\x6d\x74\x6f\x6b\x65\x6e\x54\x72\x61\x6e\x73\x66\x65\x72\xa3\x65\x74\x6f\x6b\x65\x6e\x64\
    \\x70\x6c\x74\x58\x66\x61\x6d\x6f\x75\x6e\x74\xc4\x82\x21\x18\x64\x69\x72\x65\x63\x69\x70\x69\x65\
    \\x6e\x74\xd9\x9d\x73\xa2\x01\xd9\x9d\x71\xa1\x01\x19\x03\x97\x03\x58\x20\x17\x00\x86\xc8\xae\x4a\
    \\xb9\xa4\xc8\x15\x8b\x90\x7f\xdd\x73\x19\x35\xfe\x99\xdc\xab\xce\x1f\xa6\xf3\xda\x09\x91\xdd\xe8\
    \\x2c\x50\xa1\x69\x74\x6f\x6b\x65\x6e\x4d\x69\x6e\x74\xa2\x65\x74\x6f\x6b\x65\x6e\x64\x70\x6c\x74\
    \\x59\x66\x61\x6d\x6f\x75\x6e\x74\xc4\x82\x00\x1a\x00\x01\x86\xa0\xa1\x6a\x74\x6f\x6b\x65\x6e\x50\
    \\x61\x75\x73\x65\xa1\x65\x74\x6f\x6b\x65\x6e\x64\x70\x6c\x74\x58\xa1\x71\x74\x6f\x6b\x65\x6e\x41\
    \\x64\x64\x41\x6c\x6c\x6f\x77\x4c\x69\x73\x74\xa2\x65\x74\x6f\x6b\x65\x6e\x64\x70\x6c\x74\x59\x66\
    \\x74\x61\x72\x67\x65\x74\xd9\x9d\x73\xa1\x03\x58\x20\x17\x00\x86\xc8\xae\x4a\xb9\xa4\xc8\x15\x8b\
    \\x90\x7f\xdd\x73\x19\x35\xfe\x99\xdc\xab\xce\x1f\xa6\xf3\xda\x09\x91\xdd\xe8\x2c\x50\xa1\x70\x74\
    \\x6f\x6b\x65\x6e\x41\x64\x64\x44\x65\x6e\x79\x4c\x69\x73\x74\xa2\x65\x74\x6f\x6b\x65\x6e\x64\x70\
    \\x6c\x74\x59\x66\x74\x61\x72\x67\x65\x74\xd9\x9d\x73\xa2\x01\xd9\x9d\x71\xa1\x01\x19\x03\x97\x03\
    \\x58\x20\xe2\x6c\x23\xd7\x07\xab\xfc\x3a\x1a\xbb\x11\xca\x6a\x28\x6d\xdc\xf5\x83\xfe\xbf\x9c\x7d\
    \\xda\x63\x4e\x30\x4b\xd0\x59\x00\x00\x00\xa1\x71\x74\x6f\x6b\x65\x6e\x41\x64\x64\x41\x6c\x6c\x6f\
    \\x77\x4c\x69\x73\x74\xa2\x65\x74\x6f\x6b\x65\x6e\x64\x70\x6c\x74\x59\x66\x74\x61\x72\x67\x65\x74\
    \\xd9\x9d\x73\xa2\x01\xd9\x9d\x71\xa1\x01\x19\x03\x97\x03\x58\x20\xe2\x6c\x23\xd7\x07\xab\xfc\x3a\
    \\x1a\xbb\x11\xca\x6a\x28\x6d\xdc\xf5\x83\xfe\xbf\x9c\x7d\xda\x63\x4e\x30\x4b\xd0\x59\xf5\x88\xe3\
    \\xa1\x73\x74\x6f\x6b\x65\x6e\x52\x65\x6d\x6f\x76\x65\x44\x65\x6e\x79\x4c\x69\x73\x74\xa2\x65\x74\
    \\x6f\x6b\x65\x6e\x64\x50\x4c\x54\x59\x66\x74\x61\x72\x67\x65\x74\xd9\x9d\x73\xa2\x01\xd9\x9d\x71\
    \\xa1\x01\x19\x03\x97\x03\x58\x20\xe2\x6c\x23\xd7\x07\xab\xfc\x3a\x1a\xbb\x11\xca\x6a\x28\x6d\xdc\
    \\xf5\x83\xfe\xbf\x9c\x7d\xda\x63\x4e\x30\x4b\xd0\x59\xf5\x88\xe3\xa1\x6d\x74\x6f\x6b\x65\x6e\x54\
    \\x72\x61\x6e\x73\x66\x65\x72\xa4\x64\x6d\x65\x6d\x6f\xd8\x18\x41\xa0\x65\x74\x6f\x6b\x65\x6e\x64\
    \\x70\x6c\x74\x59\x66\x61\x6d\x6f\x75\x6e\x74\xc4\x82\x00\x19\x08\x98\x69\x72\x65\x63\x69\x70\x69\
    \\x65\x6e\x74\xd9\x9d\x73\xa2\x01\xd9\x9d\x71\xa1\x01\x19\x03\x97\x03\x58\x20\x17\x00\x86\xc8\xae\
    \\x4a\xb9\xa4\xc8\x15\x8b\x90\x7f\xdd\x73\x19\x35\xfe\x99\xdc\xab\xce\x1f\xa6\xf3\xda\x09\x91\xdd\
    \\xe8\x2c\x50\xa1\x6c\x74\x6f\x6b\x65\x6e\x55\x6e\x70\x61\x75\x73\x65\xa1\x65\x74\x6f\x6b\x65\x6e\
    \\x64\x70\x6c\x74\x58\xa1\x69\x74\x6f\x6b\x65\x6e\x42\x75\x72\x6e\xa2\x65\x74\x6f\x6b\x65\x6e\x64\
    \\x50\x6c\x74\x58\x66\x61\x6d\x6f\x75\x6e\x74\xc4\x82\x21\x0a\xa1\x74\x74\x6f\x6b\x65\x6e\x52\x65\
    \\x6d\x6f\x76\x65\x41\x6c\x6c\x6f\x77\x4c\x69\x73\x74\xa2\x65\x74\x6f\x6b\x65\x6e\x64\x70\x6c\x74\
    \\x79\x66\x74\x61\x72\x67\x65\x74\xd9\x9d\x73\xa1\x03\x58\x20\xe2\x6c\x23\xd7\x07\xab\xfc\x3a\x1a\
    \\xbb\x11\xca\x6a\x28\x6d\xdc\xf5\x83\xfe\xbf\x9c\x7d\xda\x63\x4e\x30\x4b\xd0\x59\xf5\x88\xe3"

-- | The expected events from executing 'unscopedMultiOperations'.
unscopedMultiEvents :: [Event]
unscopedMultiEvents =
    [ TokenTransfer
        { ettTokenId = pltX,
          ettFrom = holder1,
          ettTo = holder2,
          ettAmount = TokenAmount{taValue = 100, taDecimals = 2},
          ettMemo = Nothing
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
          ettMemo = Just (Memo "\xa0")
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

-- | Test a unscoped Token Update transaction that consists of multiple steps and involves multiple PLTs.
testUnscopedMulti :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> Spec
testUnscopedMulti spv = case sSupportsPLT (sAccountVersionFor spv) of
    SFalse -> return ()
    STrue ->
        when (supportsUnscopedTokenUpdate spv) $
            it "Multi-token multi-step unscoped Token Update" $
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
                makeUnscopedTx dummyAddress 1 10000 keys1 unscopedMultiOperations,
              biaaAssertion = \result newST -> do
                st <- BS.freezeBlockState newST
                tiX <- queryTokenInfo (TokenId "pltX") st
                tiY <- queryTokenInfo (TokenId "pltY") st
                acc1 <- fromJust <$> BS.getAccount st dummyAddress
                ai1 <- queryAccountTokens acc1 st
                acc2 <- fromJust <$> BS.getAccount st dummyAddress2
                ai2 <- queryAccountTokens acc2 st
                return $ do
                    Helpers.assertSuccessWithEvents unscopedMultiEvents result
                    assertEqual "used energy" 1859 (Helpers.srUsedEnergy result)
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

-- | Scheduler tests for unscoped Token Update transactions.
tests :: Spec
tests = parallel $
    describe "Token Update transactions" $ do
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                describe pvString $ do
                    testUnscopedSupport spv
                    testUnscopedMulti spv
