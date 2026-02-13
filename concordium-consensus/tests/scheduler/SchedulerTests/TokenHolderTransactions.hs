{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
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

import Concordium.GlobalState.Types (BlockState)
import Data.Bool.Singletons
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as BSS
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.String
import qualified SchedulerTests.Helpers as Helpers
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

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

-- | A configuration for testing token holder transactions. This specifies testing conditions to
--  use for a transfer.
data TransferConfig = TransferConfig
    { -- | Is the token paused?
      tcPaused :: Bool,
      -- | Does the sender have sufficient balance?
      tcSenderBalanceSufficient :: Bool,
      -- | Allow list configured?
      tcAllowList :: Bool,
      -- | Sender on allow list?
      tcSenderAllow :: Bool,
      -- | Recipient on allow list?
      tcRecvAllow :: Bool,
      -- | Deny list configured?
      tcDenyList :: Bool,
      -- | Sender on deny list?
      tcSenderDeny :: Bool,
      -- | Recipient on deny list?
      tcRecvDeny :: Bool,
      -- | Recipient invalid?
      tcRecvInvalid :: Bool,
      -- | Energy insufficient?
      tcEnergyInsufficient :: Bool,
      -- | Transfer memo
      tcMemo :: Maybe CBOR.TaggableMemo,
      -- | Use short form of recipient address?
      tcShortRecv :: Bool,
      -- | Use an alias for the sender account?
      tcSenderAlias :: Bool,
      -- | Use an alias for the recipient account?
      tcRecvAlias :: Bool
    }
    deriving (Eq, Show)

instance Arbitrary TransferConfig where
    arbitrary = do
        tcPaused <- arbitrary
        tcSenderBalanceSufficient <- arbitrary
        tcEnergyInsufficient <- arbitrary
        tcRecvInvalid <- arbitrary
        tcAllowList <- arbitrary
        tcDenyList <- arbitrary
        tcSenderAllow <- (tcAllowList &&) <$> arbitrary
        tcRecvAllow <- ((tcAllowList && not tcRecvInvalid) &&) <$> arbitrary
        tcSenderDeny <- (tcDenyList &&) <$> arbitrary
        tcRecvDeny <- ((tcDenyList && not tcRecvInvalid) &&) <$> arbitrary
        tcMemo <-
            oneof
                [ pure Nothing,
                  Just . CBOR.UntaggedMemo <$> genMemo,
                  Just . CBOR.CBORMemo <$> genMemo
                ]
        tcShortRecv <- arbitrary
        tcSenderAlias <- arbitrary
        tcRecvAlias <- arbitrary
        return TransferConfig{..}
      where
        genMemo = do
            len <- chooseBoundedIntegral (0, maxMemoSize)
            Memo . BSS.pack <$> vector len
    shrink tc =
        [tc{tcAllowList = False, tcSenderAllow = False, tcRecvAllow = False} | tcAllowList tc]
            ++ [tc{tcDenyList = False, tcSenderDeny = False, tcRecvDeny = False} | tcDenyList tc]
            ++ [tc{tcMemo = Nothing} | isJust (tcMemo tc)]

-- | An alias for an 'AccountAddress' that is distinct.
distinctAlias :: AccountAddress -> AccountAddress
distinctAlias addr
    | alias == addr = alias2
    | otherwise = alias
  where
    alias = createAlias addr 0
    alias2 = createAlias addr 1

-- | This test constructs a PLT and then attempts a transfer. The setup for the transfer is
--  arbitrarily determined to cover possible failure cases and outside factors.
testTransfer ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsPLT pv) =>
    SProtocolVersion pv ->
    Property
testTransfer _ = property (ioProperty . theTest)
  where
    theTest TransferConfig{..} = do
        let govAcct = CBOR.accountTokenHolder dummyAddress
            recptAcct = CBOR.accountTokenHolder dummyAddress2
            mintAmt = TokenAmount 100 0
            excessiveAmt = TokenAmount 200 0
            params =
                CBOR.TokenInitializationParameters
                    { tipName = Just "Protocol-level token",
                      tipMetadata = Just $ CBOR.createTokenMetadataUrl "https://plt.token",
                      tipGovernanceAccount = Just govAcct,
                      tipAllowList = Just tcAllowList,
                      tipDenyList = Just tcDenyList,
                      tipInitialSupply = Just mintAmt,
                      tipMintable = Nothing,
                      tipBurnable = Nothing,
                      tipAdditional = Map.empty
                    }
            tp = Types.TokenParameter $ BSS.toShort $ CBOR.tokenInitializationParametersToBytes params
            pltName = Types.TokenId $ fromString "PLT"
            createPLT = Types.CreatePLT pltName tokenModuleV0Ref 0 tp
            condOp True = Seq.singleton
            condOp False = mempty
            mkOps = Types.TokenParameter . BSS.toShort . CBOR.tokenUpdateTransactionToBytes
            initOps =
                mkOps . CBOR.TokenUpdateTransaction $
                    condOp tcSenderAllow (CBOR.TokenAddAllowList govAcct)
                        <> condOp tcRecvAllow (CBOR.TokenAddAllowList recptAcct)
                        <> condOp tcSenderDeny (CBOR.TokenAddDenyList govAcct)
                        <> condOp tcRecvDeny (CBOR.TokenAddDenyList recptAcct)
                        <> condOp tcPaused CBOR.TokenPause
            invalidAddress = Helpers.accountAddressFromSeed (-1)
            actualRecipientAddress
                | tcRecvInvalid = invalidAddress
                | tcRecvAlias = distinctAlias dummyAddress2
                | otherwise = dummyAddress2
            actualRecipient
                | tcShortRecv = CBOR.accountTokenHolderShort actualRecipientAddress
                | otherwise = CBOR.accountTokenHolder actualRecipientAddress
            actualSenderAddress
                | tcSenderAlias = distinctAlias dummyAddress
                | otherwise = dummyAddress
            testOps =
                mkOps . CBOR.TokenUpdateTransaction . Seq.singleton $
                    CBOR.TokenTransfer $
                        CBOR.TokenTransferBody
                            { ttAmount = if tcSenderBalanceSufficient then mintAmt else excessiveAmt,
                              ttRecipient = actualRecipient,
                              ttMemo = tcMemo
                            }
            memoEnergy = case tcMemo of
                Nothing -> 0
                Just (CBOR.UntaggedMemo (Memo sbs))
                    | l < 24 -> fromIntegral $ 6 + l
                    | l < 256 -> fromIntegral $ 7 + l
                    | otherwise -> fromIntegral $ 8 + l
                  where
                    l = BSS.length sbs
                Just (CBOR.CBORMemo (Memo sbs))
                    | l < 24 -> fromIntegral $ 8 + l
                    | l < 256 -> fromIntegral $ 9 + l
                    | otherwise -> fromIntegral $ 10 + l
                  where
                    l = BSS.length sbs
            addressDeltaEnergy
                | tcShortRecv = 0
                | otherwise = 9
            requiredEnergy = 642 + memoEnergy + addressDeltaEnergy
            testEnergy = if tcEnergyInsufficient then requiredEnergy - 1 else requiredEnergy
            assertTokenReject trr =
                Helpers.assertRejectWithReason
                    . TokenUpdateTransactionFailed
                    . makeTokenModuleRejectReason pltName
                    . CBOR.encodeTokenRejectReason
                    $ trr
            expectModuleState =
                CBOR.tokenModuleStateToBytes $
                    CBOR.TokenModuleState
                        { tmsName = CBOR.tipName params,
                          tmsMetadata = CBOR.tipMetadata params,
                          tmsGovernanceAccount = Just (CBOR.accountTokenHolderShort dummyAddress),
                          tmsPaused = Just tcPaused,
                          tmsAllowList = Just tcAllowList,
                          tmsDenyList = Just tcDenyList,
                          tmsMintable = Just False,
                          tmsBurnable = Just False,
                          tmsAdditional = mempty
                        }
            expectSenderTokens sentOK =
                [ Token
                    { tokenAccountState =
                        TokenAccountState
                            { moduleAccountState =
                                Just . CBOR.tokenModuleAccountStateToBytes $
                                    CBOR.TokenModuleAccountState
                                        { tmasDenyList =
                                            if tcDenyList then Just tcSenderDeny else Nothing,
                                          tmasAllowList =
                                            if tcAllowList then Just tcSenderAllow else Nothing,
                                          tmasAdditional = mempty
                                        },
                              balance = if sentOK then TokenAmount 0 0 else mintAmt
                            },
                      tokenId = pltName
                    }
                ]
            expectDestTokens sentOK =
                [ Token
                    { tokenAccountState =
                        TokenAccountState
                            { moduleAccountState =
                                Just . CBOR.tokenModuleAccountStateToBytes $
                                    CBOR.TokenModuleAccountState
                                        { tmasDenyList =
                                            if tcDenyList then Just tcRecvDeny else Nothing,
                                          tmasAllowList =
                                            if tcAllowList then Just tcRecvAllow else Nothing,
                                          tmasAdditional = mempty
                                        },
                              balance = if sentOK then mintAmt else TokenAmount 0 0
                            },
                      tokenId = pltName
                    }
                | sentOK || tcRecvAllow || tcRecvDeny
                ]
            transactionsAndAssertions :: [Helpers.BlockItemAndAssertion pv]
            transactionsAndAssertions =
                [ Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        Runner.ChainUpdateTx $
                            Runner.ChainUpdateTransaction
                                { ctSeqNumber = 1,
                                  ctEffectiveTime = 0,
                                  ctTimeout = DummyData.dummyMaxTransactionExpiryTime,
                                  ctPayload = Types.CreatePLTUpdatePayload createPLT,
                                  ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)]
                                },
                      biaaAssertion = \result _ -> do
                        return $
                            Helpers.assertSuccessWithEvents
                                [ TokenCreated{etcPayload = createPLT},
                                  TokenMint
                                    { etmTokenId = pltName,
                                      etmTarget = HolderAccount dummyAddress,
                                      etmAmount = mintAmt
                                    }
                                ]
                                result
                    },
                  Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        Runner.AccountTx $
                            Runner.TJSON
                                { payload =
                                    Runner.TokenUpdate
                                        { tuTokenId = pltName,
                                          tuOperations = initOps
                                        },
                                  metadata = makeDummyHeader dummyAddress 1 10_000,
                                  keys = [(0, [(0, dummyKP)])]
                                },
                      biaaAssertion = \result _ -> return $ Helpers.assertSuccess result
                    },
                  Helpers.BlockItemAndAssertion
                    { biaaTransaction =
                        Runner.AccountTx $
                            Runner.TJSON
                                { payload =
                                    Runner.TokenUpdate
                                        { tuTokenId = pltName,
                                          tuOperations = testOps
                                        },
                                  metadata = makeDummyHeader actualSenderAddress 2 testEnergy,
                                  keys = [(0, [(0, dummyKP)])]
                                },
                      biaaAssertion = \result ust -> do
                        st <- BS.freezeBlockState ust
                        senderIndex <- fromJust <$> BS.getAccount st dummyAddress
                        destIndex <- fromJust <$> BS.getAccount st dummyAddress2
                        senderTokens <- queryAccountTokens senderIndex st
                        destTokens <- queryAccountTokens destIndex st
                        let postCheck sentOK = do
                                assertEqual "Used energy" testEnergy (Helpers.srUsedEnergy result)
                                assertEqual
                                    "Sender tokens after transfer"
                                    (expectSenderTokens sentOK)
                                    senderTokens
                                assertEqual
                                    "Recipient tokens after transfer"
                                    (expectDestTokens sentOK)
                                    destTokens
                        tokenInfo <- queryTokenInfo pltName st
                        return $ do
                            assertEqual
                                "Token info"
                                ( Right $
                                    TokenInfo
                                        { tiTokenId = pltName,
                                          tiTokenState =
                                            TokenState
                                                { tsTokenModuleRef = tokenModuleV0Ref,
                                                  tsDecimals = 0,
                                                  tsTotalSupply = mintAmt,
                                                  tsModuleState = expectModuleState
                                                }
                                        }
                                )
                                tokenInfo
                            if
                                | tcEnergyInsufficient -> do
                                    Helpers.assertRejectWithReason OutOfEnergy result
                                    -- The full supplied energy will be used in the case of an
                                    -- out-of-energy failure.
                                    postCheck False
                                | tcPaused -> do
                                    assertTokenReject
                                        CBOR.OperationNotPermitted
                                            { trrOperationIndex = 0,
                                              trrAddressNotPermitted = Nothing,
                                              trrReason = Just "token operation transfer is paused"
                                            }
                                        result
                                    postCheck False
                                | tcRecvInvalid -> do
                                    assertTokenReject
                                        CBOR.AddressNotFound
                                            { trrOperationIndex = 0,
                                              trrAddress = actualRecipient
                                            }
                                        result
                                    postCheck False
                                | tcAllowList && not tcSenderAllow -> do
                                    assertTokenReject
                                        CBOR.OperationNotPermitted
                                            { trrOperationIndex = 0,
                                              trrAddressNotPermitted = Just (CBOR.accountTokenHolder actualSenderAddress),
                                              trrReason = Just "sender not in allow list"
                                            }
                                        result
                                    postCheck False
                                | tcAllowList && not tcRecvAllow -> do
                                    assertTokenReject
                                        CBOR.OperationNotPermitted
                                            { trrOperationIndex = 0,
                                              trrAddressNotPermitted = Just actualRecipient,
                                              trrReason = Just "recipient not in allow list"
                                            }
                                        result
                                    postCheck False
                                | tcDenyList && tcSenderDeny -> do
                                    assertTokenReject
                                        CBOR.OperationNotPermitted
                                            { trrOperationIndex = 0,
                                              trrAddressNotPermitted = Just (CBOR.accountTokenHolder actualSenderAddress),
                                              trrReason = Just "sender in deny list"
                                            }
                                        result
                                    postCheck False
                                | tcDenyList && tcRecvDeny -> do
                                    assertTokenReject
                                        CBOR.OperationNotPermitted
                                            { trrOperationIndex = 0,
                                              trrAddressNotPermitted = Just actualRecipient,
                                              trrReason = Just "recipient in deny list"
                                            }
                                        result
                                    postCheck False
                                | not tcSenderBalanceSufficient -> do
                                    assertTokenReject
                                        CBOR.TokenBalanceInsufficient
                                            { trrOperationIndex = 0,
                                              trrRequiredBalance = excessiveAmt,
                                              trrAvailableBalance = mintAmt
                                            }
                                        result
                                    postCheck False
                                | otherwise -> do
                                    Helpers.assertSuccessWithEvents
                                        [ TokenTransfer
                                            { ettTokenId = pltName,
                                              ettFrom = HolderAccount actualSenderAddress,
                                              ettTo = HolderAccount actualRecipientAddress,
                                              ettAmount = mintAmt,
                                              ettMemo = CBOR.taggableMemoInner <$> tcMemo
                                            }
                                        ]
                                        result
                                    postCheck True
                    }
                ]
        Helpers.runSchedulerTestAssertIntermediateStates
            @pv
            Helpers.defaultTestConfig
            initialBlockState
            transactionsAndAssertions

getTokenModuleState :: (BS.BlockStateQuery m) => TokenId -> BlockState m -> m (Either String CBOR.TokenModuleState)
getTokenModuleState tokenId st =
    queryTokenInfo tokenId st >>= \case
        Left e -> return . Left $ show e
        Right r -> return $ CBOR.tokenModuleStateFromBytes $ LBS.fromStrict $ tsModuleState $ tiTokenState r

testPauseUnpause ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsPLT pv) =>
    SProtocolVersion pv ->
    Assertion
testPauseUnpause _ = do
    Helpers.runSchedulerTestAssertIntermediateStates
        @pv
        Helpers.defaultTestConfig
        initialBlockState
        transactionsAndAssertions
  where
    govAcct = CBOR.accountTokenHolder dummyAddress
    mintAmt = TokenAmount 1000 0
    params =
        CBOR.TokenInitializationParameters
            { tipName = Just "Protocol-level token",
              tipMetadata = Just $ CBOR.createTokenMetadataUrl "https://plt.token",
              tipGovernanceAccount = Just govAcct,
              tipAllowList = Nothing,
              tipDenyList = Nothing,
              tipInitialSupply = Just mintAmt,
              tipMintable = Nothing,
              tipBurnable = Nothing,
              tipAdditional = Map.empty
            }
    tp = Types.TokenParameter $ BSS.toShort $ CBOR.tokenInitializationParametersToBytes params
    pltName = Types.TokenId $ fromString "PLT"
    createPLT = Types.CreatePLT pltName tokenModuleV0Ref 0 tp
    keys1 = [(0, [(0, dummyKP)])]
    keys2 = [(0, [(0, Helpers.keyPairFromSeed 2)])]
    mkOps = Types.TokenParameter . BSS.toShort . CBOR.tokenUpdateTransactionToBytes . CBOR.TokenUpdateTransaction . Seq.fromList
    mkUpdateTx sendAddr nonce nrg keys ops =
        Runner.AccountTx
            Runner.TJSON
                { payload = Runner.TokenUpdate{tuTokenId = pltName, tuOperations = mkOps ops},
                  metadata = makeDummyHeader sendAddr nonce nrg,
                  keys = keys
                }
    pauseEvent =
        TokenModuleEvent
            { etmeTokenId = pltName,
              etmeType = TokenEventType "pause",
              etmeDetails = CBOR.emptyEventDetails
            }
    unpauseEvent =
        TokenModuleEvent
            { etmeTokenId = pltName,
              etmeType = TokenEventType "unpause",
              etmeDetails = CBOR.emptyEventDetails
            }
    checkEnergyStateEvents :: Energy -> Bool -> [Event] -> Helpers.TransactionAssertion pv
    checkEnergyStateEvents nrg expectPaused evts = \result ust -> do
        st <- BS.freezeBlockState ust
        tms <- getTokenModuleState pltName st
        return $ do
            assertEqual "Used energy" nrg (Helpers.srUsedEnergy result)
            assertEqual "Pause state" (Right (Just expectPaused)) (CBOR.tmsPaused <$> tms)
            Helpers.assertSuccessWithEvents evts result
    assertTokenReject trr =
        Helpers.assertRejectWithReason
            . TokenUpdateTransactionFailed
            . makeTokenModuleRejectReason pltName
            . CBOR.encodeTokenRejectReason
            $ trr
    checkEnergyStateReason nrg expectPaused rr = \result ust -> do
        st <- BS.freezeBlockState ust
        tms <- getTokenModuleState pltName st
        return $ do
            assertEqual "Used energy" nrg (Helpers.srUsedEnergy result)
            assertEqual "Pause state" (Right (Just expectPaused)) (CBOR.tmsPaused <$> tms)
            assertTokenReject rr result
    transactionsAndAssertions =
        [ -- Initialise the token.
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                Runner.ChainUpdateTx $
                    Runner.ChainUpdateTransaction
                        { ctSeqNumber = 1,
                          ctEffectiveTime = 0,
                          ctTimeout = DummyData.dummyMaxTransactionExpiryTime,
                          ctPayload = Types.CreatePLTUpdatePayload createPLT,
                          ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)]
                        },
              biaaAssertion = \result _ -> do
                return $
                    Helpers.assertSuccessWithEvents
                        [ TokenCreated{etcPayload = createPLT},
                          TokenMint
                            { etmTokenId = pltName,
                              etmTarget = HolderAccount dummyAddress,
                              etmAmount = mintAmt
                            }
                        ]
                        result
            },
          -- Pause from gov account (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress 1 1000 keys1 [CBOR.TokenPause],
              biaaAssertion = checkEnergyStateEvents 528 True [pauseEvent]
            },
          -- Pause from gov account while already paused (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress 2 1000 keys1 [CBOR.TokenPause],
              biaaAssertion = checkEnergyStateEvents 528 True [pauseEvent]
            },
          -- Unpause from gov account (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress 3 1000 keys1 [CBOR.TokenUnpause],
              biaaAssertion = checkEnergyStateEvents 530 False [unpauseEvent]
            },
          -- Unpause from gov account while already unpaused (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress 4 1000 keys1 [CBOR.TokenUnpause],
              biaaAssertion = checkEnergyStateEvents 530 False [unpauseEvent]
            },
          -- Pause from an alias of the gov account (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx (distinctAlias dummyAddress) 5 1000 keys1 [CBOR.TokenPause],
              biaaAssertion = checkEnergyStateEvents 528 True [pauseEvent]
            },
          -- Unpause from the gov account (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress 6 1000 keys1 [CBOR.TokenUnpause],
              biaaAssertion = checkEnergyStateEvents 530 False [unpauseEvent]
            },
          -- Pause twice from the gov account (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress 7 1000 keys1 [CBOR.TokenPause, CBOR.TokenPause],
              biaaAssertion = checkEnergyStateEvents 586 True [pauseEvent, pauseEvent]
            },
          -- Unpause from an alias of the gov account (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx (distinctAlias dummyAddress) 8 1000 keys1 [CBOR.TokenUnpause],
              biaaAssertion = checkEnergyStateEvents 530 False [unpauseEvent]
            },
          -- Unpause, pause, unpause from the gov account (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress 9 1000 keys1 [CBOR.TokenUnpause, CBOR.TokenPause, CBOR.TokenUnpause],
              biaaAssertion = checkEnergyStateEvents 648 False [unpauseEvent, pauseEvent, unpauseEvent]
            },
          -- Pause from a non-gov account (fails: not permitted).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress2 1 1000 keys2 [CBOR.TokenPause],
              biaaAssertion =
                checkEnergyStateReason 528 False $
                    CBOR.OperationNotPermitted
                        { trrOperationIndex = 0,
                          trrAddressNotPermitted = Just (CBOR.accountTokenHolder dummyAddress2),
                          trrReason = Just "sender is not the token governance account"
                        }
            },
          -- Pause and transfer from gov account (fails: transfer not permitted while paused).
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx dummyAddress 10 1000 keys1 $
                    [ CBOR.TokenPause,
                      CBOR.TokenTransfer
                        CBOR.TokenTransferBody
                            { ttRecipient = CBOR.accountTokenHolder dummyAddress,
                              ttMemo = Nothing,
                              ttAmount = TokenAmount 10 0
                            }
                    ],
              biaaAssertion =
                checkEnergyStateReason 708 False $
                    CBOR.OperationNotPermitted
                        { trrOperationIndex = 1,
                          trrAddressNotPermitted = Nothing,
                          trrReason = Just "token operation transfer is paused"
                        }
            },
          -- Unpause, pause from gov account (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress 11 1000 keys1 [CBOR.TokenUnpause, CBOR.TokenPause],
              biaaAssertion = checkEnergyStateEvents 588 True [unpauseEvent, pauseEvent]
            },
          -- Unpause, transfer, pause from gov account (OK).
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx dummyAddress 12 1000 keys1 $
                    [ CBOR.TokenUnpause,
                      CBOR.TokenTransfer
                        CBOR.TokenTransferBody
                            { ttRecipient = CBOR.accountTokenHolder dummyAddress,
                              ttMemo = Nothing,
                              ttAmount = TokenAmount 10 0
                            },
                      CBOR.TokenPause
                    ],
              biaaAssertion =
                checkEnergyStateEvents 768 True $
                    [ unpauseEvent,
                      TokenTransfer
                        { ettTokenId = pltName,
                          ettFrom = HolderAccount dummyAddress,
                          ettTo = HolderAccount dummyAddress,
                          ettAmount = TokenAmount 10 0,
                          ettMemo = Nothing
                        },
                      pauseEvent
                    ]
            },
          -- Unpause from non-gov account (fails: not permitted).
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress2 2 1000 keys2 [CBOR.TokenUnpause],
              biaaAssertion =
                checkEnergyStateReason 530 True $
                    CBOR.OperationNotPermitted
                        { trrOperationIndex = 0,
                          trrAddressNotPermitted = Just (CBOR.accountTokenHolder dummyAddress2),
                          trrReason = Just "sender is not the token governance account"
                        }
            }
        ]

testMintBurn :: forall pv. (IsProtocolVersion pv, PVSupportsPLT pv) => SProtocolVersion pv -> Bool -> Bool -> Assertion
testMintBurn _ mintEnabled burnEnabled = do
    Helpers.runSchedulerTestAssertIntermediateStates
        @pv
        Helpers.defaultTestConfig
        initialBlockState
        transactionsAndAssertions
  where
    govAcct = CBOR.accountTokenHolder dummyAddress
    mintAmt = TokenAmount 1000 0
    params =
        CBOR.TokenInitializationParameters
            { tipName = Just "Protocol-level token",
              tipMetadata = Just $ CBOR.createTokenMetadataUrl "https://plt.token",
              tipGovernanceAccount = Just govAcct,
              tipAllowList = Just True, -- NB: allow and deny list should not affect mint/burn
              tipDenyList = Just True,
              tipInitialSupply = Just mintAmt,
              tipMintable = Just mintEnabled,
              tipBurnable = Just burnEnabled,
              tipAdditional = Map.empty
            }
    tp = Types.TokenParameter $ BSS.toShort $ CBOR.tokenInitializationParametersToBytes params
    pltName = Types.TokenId $ fromString "PLT"
    createPLT = Types.CreatePLT pltName tokenModuleV0Ref 0 tp
    keys1 = [(0, [(0, dummyKP)])]
    keys2 = [(0, [(0, Helpers.keyPairFromSeed 2)])]
    mkOps = Types.TokenParameter . BSS.toShort . CBOR.tokenUpdateTransactionToBytes . CBOR.TokenUpdateTransaction . Seq.fromList
    mkUpdateTx sendAddr nonce nrg keys ops =
        Runner.AccountTx
            Runner.TJSON
                { payload = Runner.TokenUpdate{tuTokenId = pltName, tuOperations = mkOps ops},
                  metadata = makeDummyHeader sendAddr nonce nrg,
                  keys = keys
                }
    mintEventFor addr amt =
        TokenMint
            { etmTokenId = pltName,
              etmTarget = HolderAccount addr,
              etmAmount = amt
            }
    mintEvent = mintEventFor dummyAddress
    burnEventFor addr amt =
        TokenBurn
            { etbTokenId = pltName,
              etbTarget = HolderAccount addr,
              etbAmount = amt
            }
    burnEvent = burnEventFor dummyAddress
    expectGovTokens amount =
        [ Token
            { tokenId = pltName,
              tokenAccountState =
                TokenAccountState
                    { balance = amount,
                      moduleAccountState =
                        Just . CBOR.tokenModuleAccountStateToBytes $
                            CBOR.TokenModuleAccountState
                                { tmasDenyList = Just False,
                                  tmasAllowList = Just False,
                                  tmasAdditional = mempty
                                }
                    }
            }
        ]
    checkEnergyStateEvents :: Bool -> Energy -> Integer -> Integer -> Integer -> [Event] -> Helpers.TransactionAssertion pv
    checkEnergyStateEvents isMint nrg mintTotal burnTotal delta evts = \result ust -> do
        let opEnabled = if isMint then mintEnabled else burnEnabled
        st <- BS.freezeBlockState ust
        mSupply <- fmap (tsTotalSupply . tiTokenState) <$> queryTokenInfo pltName st
        govIndex <- fromJust <$> BS.getAccount st dummyAddress
        govAccountTokens <- queryAccountTokens govIndex st
        let cnd b a = if b then a else 0
        let expectSupply =
                TokenAmount
                    (fromInteger $ 1000 + cnd opEnabled delta + cnd mintEnabled mintTotal - cnd burnEnabled burnTotal)
                    0
        return $ do
            assertEqual "Used energy" nrg (Helpers.srUsedEnergy result)
            assertEqual "Total supply" (Right expectSupply) mSupply
            assertEqual "Governance account tokens" (expectGovTokens expectSupply) govAccountTokens
            if opEnabled
                then Helpers.assertSuccessWithEvents evts result
                else
                    assertTokenReject
                        CBOR.UnsupportedOperation
                            { trrOperationIndex = 0,
                              trrOperationType = if isMint then "mint" else "burn",
                              trrReason = Just $ "feature not enabled"
                            }
                        result
    assertTokenReject trr =
        Helpers.assertRejectWithReason
            . TokenUpdateTransactionFailed
            . makeTokenModuleRejectReason pltName
            . CBOR.encodeTokenRejectReason
            $ trr
    checkEnergyStateReason :: Energy -> Integer -> Integer -> CBOR.TokenRejectReason -> Helpers.TransactionAssertion pv
    checkEnergyStateReason nrg mintTotal burnTotal rr = \result ust -> do
        st <- BS.freezeBlockState ust
        mSupply <- fmap (tsTotalSupply . tiTokenState) <$> queryTokenInfo pltName st
        govIndex <- fromJust <$> BS.getAccount st dummyAddress
        govAccountTokens <- queryAccountTokens govIndex st
        let cnd b a = if b then a else 0
        let expectSupply =
                TokenAmount
                    (fromInteger $ 1000 + cnd mintEnabled mintTotal - cnd burnEnabled burnTotal)
                    0
        return $ do
            assertEqual "Used energy" nrg (Helpers.srUsedEnergy result)
            assertEqual "Total supply" (Right expectSupply) mSupply
            assertEqual "Governance account tokens" (expectGovTokens expectSupply) govAccountTokens
            assertTokenReject rr result
    transactionsAndAssertions =
        [ -- Initialise the token.
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                Runner.ChainUpdateTx $
                    Runner.ChainUpdateTransaction
                        { ctSeqNumber = 1,
                          ctEffectiveTime = 0,
                          ctTimeout = DummyData.dummyMaxTransactionExpiryTime,
                          ctPayload = Types.CreatePLTUpdatePayload createPLT,
                          ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)]
                        },
              biaaAssertion = \result _ -> do
                return $
                    Helpers.assertSuccessWithEvents
                        [TokenCreated{etcPayload = createPLT}, mintEvent mintAmt]
                        result
            },
          -- Mint 50 from gov acct (OK if mint enabled)
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx dummyAddress 1 1000 keys1 $
                    [CBOR.TokenMint (TokenAmount 50 0)],
              biaaAssertion =
                checkEnergyStateEvents True 539 0 0 50 $
                    [mintEvent (TokenAmount 50 0)]
            },
          -- Burn 50 from gov acct (OK if burn enabled)
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx dummyAddress 2 1000 keys1 $
                    [CBOR.TokenBurn (TokenAmount 50 0)],
              biaaAssertion =
                checkEnergyStateEvents False 539 50 0 (-50) $
                    [burnEvent (TokenAmount 50 0)]
            },
          -- Mint 50 from gov acct with alias (OK if mint enabled)
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx (distinctAlias dummyAddress) 3 1000 keys1 $
                    [CBOR.TokenMint (TokenAmount 50 0)],
              biaaAssertion =
                checkEnergyStateEvents True 539 50 50 50 $
                    [mintEventFor (distinctAlias dummyAddress) (TokenAmount 50 0)]
            },
          -- Burn 50 from gov acct with alias (OK if burn enabled)
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx (distinctAlias dummyAddress) 4 1000 keys1 $
                    [CBOR.TokenBurn (TokenAmount 50 0)],
              biaaAssertion =
                checkEnergyStateEvents False 539 100 50 (-50) $
                    [burnEventFor (distinctAlias dummyAddress) (TokenAmount 50 0)]
            },
          -- Mint 50 from non-gov acct (fails: not permitted)
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx dummyAddress2 1 1000 keys2 $
                    [CBOR.TokenMint (TokenAmount 50 0)],
              biaaAssertion =
                checkEnergyStateReason 539 100 100 $
                    CBOR.OperationNotPermitted
                        { trrOperationIndex = 0,
                          trrAddressNotPermitted = Just (CBOR.accountTokenHolder dummyAddress2),
                          trrReason = Just "sender is not the token governance account"
                        }
            },
          -- Burn 50 from non-gov acct (fails: not permitted)
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx dummyAddress2 2 1000 keys2 $
                    [CBOR.TokenBurn (TokenAmount 50 0)],
              biaaAssertion =
                checkEnergyStateReason 539 100 100 $
                    CBOR.OperationNotPermitted
                        { trrOperationIndex = 0,
                          trrAddressNotPermitted = Just (CBOR.accountTokenHolder dummyAddress2),
                          trrReason = Just "sender is not the token governance account"
                        }
            },
          -- Mint too much from gov acct (fails: would overflow, or not enabled)
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx dummyAddress 5 1000 keys1 $
                    [CBOR.TokenMint (TokenAmount (maxBound - 100) 0)],
              biaaAssertion =
                checkEnergyStateReason 546 100 100 $
                    if mintEnabled
                        then
                            CBOR.MintWouldOverflow
                                { trrOperationIndex = 0,
                                  trrRequestedAmount = TokenAmount (maxBound - 100) 0,
                                  trrMaxRepresentableAmount = TokenAmount maxBound 0,
                                  trrCurrentSupply = TokenAmount (1100 - (if burnEnabled then 100 else 0)) 0
                                }
                        else
                            CBOR.UnsupportedOperation
                                { trrOperationIndex = 0,
                                  trrOperationType = "mint",
                                  trrReason = Just $ "feature not enabled"
                                }
            },
          -- Burn too much from gov acct (fails: balance insufficient, or not enabled)
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx dummyAddress 6 1000 keys1 $
                    [CBOR.TokenBurn (TokenAmount (1101) 0)],
              biaaAssertion =
                checkEnergyStateReason 540 100 100 $
                    if burnEnabled
                        then
                            CBOR.TokenBalanceInsufficient
                                { trrOperationIndex = 0,
                                  trrRequiredBalance = TokenAmount 1101 0,
                                  trrAvailableBalance = TokenAmount (900 + (if mintEnabled then 100 else 0)) 0
                                }
                        else
                            CBOR.UnsupportedOperation
                                { trrOperationIndex = 0,
                                  trrOperationType = "burn",
                                  trrReason = Just $ "feature not enabled"
                                }
            },
          -- Pause the token (OK)
          Helpers.BlockItemAndAssertion
            { biaaTransaction = mkUpdateTx dummyAddress 7 1000 keys1 [CBOR.TokenPause],
              biaaAssertion = \result _ ->
                return $
                    Helpers.assertSuccessWithEvents
                        [ TokenModuleEvent
                            { etmeTokenId = pltName,
                              etmeType = TokenEventType "pause",
                              etmeDetails = CBOR.emptyEventDetails
                            }
                        ]
                        result
            },
          -- Mint from gov acct while paused (fails: operation not permitted)
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx dummyAddress 8 1000 keys1 $
                    [CBOR.TokenMint (TokenAmount 50 0)],
              biaaAssertion =
                checkEnergyStateReason 539 100 100 $
                    CBOR.OperationNotPermitted
                        { trrOperationIndex = 0,
                          trrAddressNotPermitted = Nothing,
                          trrReason = Just "token operation mint is paused"
                        }
            },
          -- Burn from gov acct while paused (fails: operation not permitted)
          Helpers.BlockItemAndAssertion
            { biaaTransaction =
                mkUpdateTx dummyAddress 9 1000 keys1 $
                    [CBOR.TokenBurn (TokenAmount 50 0)],
              biaaAssertion =
                checkEnergyStateReason 539 100 100 $
                    CBOR.OperationNotPermitted
                        { trrOperationIndex = 0,
                          trrAddressNotPermitted = Nothing,
                          trrReason = Just "token operation burn is paused"
                        }
            }
        ]

tests :: Spec
tests =
    parallel $
        describe "Token holder transactions" $
            sequence_ $
                Helpers.forEveryProtocolVersion testCases
  where
    testCases :: forall pv. (IsProtocolVersion pv) => SProtocolVersion pv -> String -> Spec
    testCases spv pvString =
        case sSupportsPLT (sAccountVersionFor spv) of
            STrue -> describe pvString $ do
                testTokenHolder spv pvString
                it "PLT transfers" $ withMaxSuccess 500 $ testTransfer spv
                it "Pause/unpause" $ testPauseUnpause spv
                describe "Mint/burn" $ do
                    it "mint enabled, burn enabled" $ testMintBurn spv True True
                    it "mint enabled, burn disabled" $ testMintBurn spv True False
                    it "mint disabled, burn enabled" $ testMintBurn spv False True
                    it "mint disabled, burn disabled" $ testMintBurn spv False False
            SFalse -> return ()
