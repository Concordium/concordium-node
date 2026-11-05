{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.KonsensusV1.AutoCancelExpiredLocks (tests) where

import Codec.CBOR.Encoding
import qualified Codec.CBOR.Term as Term
import Control.Monad (foldM)
import Control.Monad.IO.Class
import Data.Bool.Singletons
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.CapitalDistribution (makeCapitalDistribution)
import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.Account as Account
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.KonsensusV1.Scheduler as Block
import Concordium.Scheduler.DummyData (makeDummyHeader)
import qualified Concordium.Scheduler.Environment as Env
import Concordium.Scheduler.ProtocolLevelTokens.Module (tokenModuleV0Ref)
import Concordium.Scheduler.ProtocolLevelTokens.Queries
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import Concordium.Types.Accounts
import qualified Concordium.Types.Locks.CBOR as Locks
import qualified Concordium.Types.ProtocolLevelTokens.CBOR as CBOR
import Concordium.Types.Queries.Tokens
import qualified SchedulerTests.Helpers as Helpers

-- | Observe lock existence and the owner's total and available token holdings.
observeLockBalance ::
    BS.HashedPersistentBlockState 'P11 ->
    Helpers.PersistentBSM 'P11 ([LockId], TokenAmount, TokenAmount)
observeLockBalance state = do
    locks <- queryLockList state
    account <- fromJust <$> BS.getAccount state (Helpers.accountAddressFromSeed 0)
    tokens <- queryAccountTokens account state
    case tokens of
        [Token{tokenAccountState = TokenAccountState{..}}] -> do
            let available = do
                    bytes <- maybe (Left "Missing token module account state") Right moduleAccountState
                    moduleState <- CBOR.tokenModuleAccountStateFromBytes (LBS.fromStrict bytes)
                    case Map.lookup "available" (CBOR.tmasAdditional moduleState) of
                        Nothing -> Right balance
                        Just amount ->
                            CBOR.decodeFromBytes CBOR.decodeTokenAmount "available balance" $
                                LBS.fromStrict $
                                    CBOR.encodeToBytes $
                                        Term.encodeTerm amount
            case available of
                Left err -> liftIO $ assertFailure err >> fail err
                Right amount -> return (locks, balance, amount)
        _ -> liftIO $ assertFailure ("Expected one token, got " ++ show tokens) >> fail "Invalid token fixture"

tests :: Spec
tests = describe "Automatic lock cancellation" $
    it "REQ-001-S2: block execution unlocks an expired funded lock" $
        Helpers.runTestBlockState @'P11 $ do
            owner <- Helpers.makeTestAccountFromSeed 20_000_000 0
            let (baker, electionKey, _, _) = DummyData.mkFullBaker 0 0
                bakerInfoEx =
                    BakerInfoExV1
                        { _bieBakerInfo = baker ^. theBakerInfo,
                          _bieBakerPoolInfo = BakerPoolInfo OpenForAll (UrlText "") (CommissionRates (makeAmountFraction 0) (makeAmountFraction 0) (makeAmountFraction 0)),
                          _bieIsSuspended = conditionally STrue False
                        }
            bakerAccount <- Account.addAccountBakerV1 bakerInfoEx 1_000_000 True owner
            accountState <- Helpers.createTestBlockStateWithAccounts [bakerAccount]
            -- The account fixture leaves pool reward tracking empty; block execution needs it.
            thawed <- BS.thawBlockState accountState
            withCapital <- BS.bsoSetNextCapitalDistribution thawed (makeCapitalDistribution [(0, 1_000_000, [])] [])
            initialState <- BS.freezeBlockState =<< BS.bsoRotateCurrentCapitalDistribution withCapital
            let ownerAddress = Helpers.accountAddressFromSeed 0
                token = TokenId "LOCK"
                lockId = LockId 0 1 0
                tokenParams =
                    CBOR.TokenInitializationParameters
                        { tipName = Just "Lock token",
                          tipMetadata = Just $ CBOR.createTokenMetadataUrl "https://plt.token",
                          tipGovernanceAccount = Just $ CBOR.accountTokenHolder ownerAddress,
                          tipAllowList = Just False,
                          tipDenyList = Just False,
                          tipInitialSupply = Just $ TokenAmount 1_000 0,
                          tipMintable = Just False,
                          tipBurnable = Just False,
                          tipAdditional = Map.empty
                        }
                createToken =
                    CreatePLT token tokenModuleV0Ref 0 $
                        rawCborFromBytes $
                            CBOR.tokenInitializationParametersToBytes tokenParams
                lockConfig =
                    Locks.LockConfigSimpleV0 $
                        Locks.SimpleLockConfigV0
                            { lcsv0Recipients = Locks.LockRecipientsAny,
                              lcsv0Expiry = 10,
                              lcsv0Grants =
                                Seq.singleton $
                                    Locks.LockControllerSimpleV0Grant
                                        (CBOR.accountTokenHolder ownerAddress)
                                        (Seq.singleton Locks.LockControllerSimpleV0Fund),
                              lcsv0Tokens = Seq.singleton token,
                              lcsv0KeepAlive = False,
                              lcsv0Memo = Nothing,
                              lcsv0Metadata = Nothing
                            }
                operations =
                    rawCborFromBytes $
                        CBOR.encodeToBytes $
                            encodeListLen 2
                                <> encodeMapLen 1
                                <> encodeString "lockCreate"
                                <> Locks.encodeLockConfig lockConfig
                                <> encodeMapLen 1
                                <> encodeString "lockFund"
                                <> encodeMapLen 3
                                <> encodeString "lock"
                                <> Locks.encodeLockId lockId
                                <> encodeString "token"
                                <> CBOR.encodeTokenId token
                                <> encodeString "amount"
                                <> CBOR.encodeTokenAmount (TokenAmount 250 0)
                -- Creation just before expiry fits the fixture's 42 ms maximum lock duration.
                config =
                    Helpers.defaultTestConfig
                        { Helpers.tcContextState =
                            Helpers.defaultContextState{Env._chainMetadata = ChainMetadata 9_999}
                        }
                setupTransactions =
                    [ Runner.ChainUpdateTx $
                        Runner.ChainUpdateTransaction
                            { ctSeqNumber = 1,
                              ctEffectiveTime = 0,
                              ctTimeout = TransactionTime maxBound,
                              ctPayload = CreatePLTUpdatePayload createToken,
                              ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)]
                            },
                      Runner.AccountTx $
                        Runner.TJSON
                            { payload = Runner.UnscopedTokenUpdate operations,
                              metadata = makeDummyHeader ownerAddress 1 10_000,
                              keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
                            }
                    ]
            parentState <- foldM (runSetupTransaction config) initialState setupTransactions
            parentBalances <- observeLockBalance parentState
            liftIO $ assertEqual "Funded lock before block execution" ([lockId], TokenAmount 1_000 0, TokenAmount 750 0) parentBalances
            chainParams <- BS.getChainParameters parentState
            liftIO $ assertBool "Block has enough energy to cancel one balance reference" (chainParams ^. cpConsensusParameters . cpBlockEnergyLimit >= 500)
            let executionData =
                    Block.BlockExecutionData
                        { bedIsNewEpoch = False,
                          bedEpochDuration = 3_600_000,
                          bedTimestamp = 10_001,
                          bedBlockNonce = VRF.prove (VRF.KeyPair electionKey (VRF.pubKey electionKey)) "",
                          bedParticipatingBakers = Block.ParticipatingBakers 0 [0],
                          bedParentState = parentState,
                          bedMissedRounds = Map.empty
                        }
            Block.executeBlockState executionData [] >>= \case
                Left failure -> liftIO $ assertFailure $ "Block execution failed: " ++ show failure
                Right (stateAfter, _) -> do
                    (remainingLocks, totalAfter, availableAfter) <- observeLockBalance stateAfter
                    let (_, totalBefore, _) = parentBalances
                    liftIO $ do
                        assertEqual "Total token holdings are unchanged" totalBefore totalAfter
                        assertEqual
                            "Expired lock is destroyed and its funds become available"
                            ([], TokenAmount 1_000 0)
                            (remainingLocks, availableAfter)
  where
    runSetupTransaction ::
        Helpers.TestConfig ->
        BS.HashedPersistentBlockState 'P11 ->
        Runner.BlockItemDescription ->
        Helpers.PersistentBSM 'P11 (BS.HashedPersistentBlockState 'P11)
    runSetupTransaction config state tx = do
        transactions <- liftIO $ Runner.processUngroupedBlockItems [tx]
        (result, updated) <- Helpers.runScheduler config state transactions
        liftIO $ Helpers.assertSuccess result
        BS.freezeBlockState updated
