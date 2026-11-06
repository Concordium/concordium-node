{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistent P11 fixtures shared by cleanup integration tests and full-block benchmarks.
module SchedulerTests.KonsensusV1.AutoCancelExpiredLocksFixture where

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
import qualified Concordium.TransactionVerification as Verification
import Concordium.Types.Accounts
import qualified Concordium.Types.Locks.CBOR as Locks
import qualified Concordium.Types.ProtocolLevelTokens.CBOR as CBOR
import Concordium.Types.Queries.Tokens
import qualified SchedulerTests.Helpers as Helpers

-- Rust owns this tariff; no Haskell cost API exposes it. Assert the cross-language contract.
emptyCancellationCost, oneReferenceCancellationCost :: Energy
emptyCancellationCost = 100
oneReferenceCancellationCost = 600

-- | Observe lock existence and the owner's total and available token holdings.
observeLockBalance ::
    BS.HashedPersistentBlockState 'P11 ->
    Helpers.PersistentBSM 'P11 ([LockId], TokenAmount, TokenAmount)
observeLockBalance state = do
    locks <- queryLockList state
    account <- fromJust <$> BS.getAccount state ownerAddress
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

ownerAddress :: AccountAddress
ownerAddress = Helpers.accountAddressFromSeed 0

token :: TokenId
token = TokenId "LOCK"

setupConfig :: Helpers.TestConfig
setupConfig =
    Helpers.defaultTestConfig
        { Helpers.tcContextState = Helpers.defaultContextState{Env._chainMetadata = ChainMetadata 9_999}
        }

runSetupTransaction ::
    BS.HashedPersistentBlockState 'P11 ->
    Runner.BlockItemDescription ->
    Helpers.PersistentBSM 'P11 (BS.HashedPersistentBlockState 'P11)
runSetupTransaction state tx = do
    transactions <- liftIO $ Runner.processUngroupedBlockItems [tx]
    (result, updated) <- Helpers.runScheduler setupConfig state transactions
    liftIO $ do
        assertBool ("Setup transaction not included: " ++ show result) (not $ null $ ftAdded $ Helpers.srTransactions result)
        Helpers.assertSuccess result
    BS.freezeBlockState updated

-- | Set up valid batches of at most 100 locks, without timing setup or signing.
makeFixture :: Int -> Bool -> Helpers.PersistentBSM 'P11 (Block.BlockExecutionData 'P11, [LockId], Nonce)
makeFixture count funded = do
    owner <- Helpers.makeTestAccountFromSeed 100_000_000_000 0
    let (baker, electionKey, _, _) = DummyData.mkFullBaker 0 0
        bakerInfoEx =
            BakerInfoExV1
                { _bieBakerInfo = baker ^. theBakerInfo,
                  _bieBakerPoolInfo = BakerPoolInfo OpenForAll (UrlText "") (CommissionRates (makeAmountFraction 0) (makeAmountFraction 0) (makeAmountFraction 0)),
                  _bieIsSuspended = conditionally STrue False
                }
    bakerAccount <- Account.addAccountBakerV1 bakerInfoEx 1_000_000 True owner
    accountState <- Helpers.createTestBlockStateWithAccounts [bakerAccount]
    thawed <- BS.thawBlockState accountState
    withCapital <- BS.bsoSetNextCapitalDistribution thawed (makeCapitalDistribution [(0, 1_000_000, [])] [])
    initialState <- BS.freezeBlockState =<< BS.bsoRotateCurrentCapitalDistribution withCapital
    let tokenParams =
            CBOR.TokenInitializationParameters
                { tipName = Just "Lock token",
                  tipMetadata = Just $ CBOR.createTokenMetadataUrl "https://plt.token",
                  tipGovernanceAccount = Just $ CBOR.accountTokenHolder ownerAddress,
                  tipAllowList = Just False,
                  tipDenyList = Just False,
                  tipInitialSupply = Just $ TokenAmount (fromIntegral count * 250) 0,
                  tipMintable = Just False,
                  tipBurnable = Just False,
                  tipAdditional = Map.empty
                }
        createToken = CreatePLT token tokenModuleV0Ref 0 $ rawCborFromBytes $ CBOR.tokenInitializationParametersToBytes tokenParams
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
                      lcsv0KeepAlive = True,
                      lcsv0Memo = Nothing,
                      lcsv0Metadata = Nothing
                    }
        createLock lockId =
            encodeMapLen 1
                <> encodeString "lockCreate"
                <> Locks.encodeLockConfig lockConfig
                <> if funded
                    then
                        encodeMapLen 1
                            <> encodeString "lockFund"
                            <> encodeMapLen 3
                            <> encodeString "lock"
                            <> Locks.encodeLockId lockId
                            <> encodeString "token"
                            <> CBOR.encodeTokenId token
                            <> encodeString "amount"
                            <> CBOR.encodeTokenAmount (TokenAmount 250 0)
                    else mempty
        batches = [(fromIntegral batch + 1, min 100 (count - batch * 100)) | batch <- [0 .. (count - 1) `div` 100]]
        ids = [LockId 0 nonce (fromIntegral i) | (nonce, size) <- batches, i <- [0 .. size - 1]]
        createBatch (nonce, size) =
            Runner.AccountTx $
                Runner.TJSON
                    { payload =
                        Runner.UnscopedTokenUpdate $
                            rawCborFromBytes $
                                CBOR.encodeToBytes $
                                    encodeListLen (fromIntegral $ size * if funded then 2 else 1)
                                        <> foldMap (createLock . LockId 0 nonce . fromIntegral) [0 .. size - 1],
                      metadata = makeDummyHeader ownerAddress (fromIntegral nonce) 200_000,
                      keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
                    }
    withToken <-
        runSetupTransaction initialState $
            Runner.ChainUpdateTx $
                Runner.ChainUpdateTransaction
                    { ctSeqNumber = 1,
                      ctEffectiveTime = 0,
                      ctTimeout = TransactionTime maxBound,
                      ctPayload = CreatePLTUpdatePayload createToken,
                      ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)]
                    }
    parent <- foldM runSetupTransaction withToken (createBatch <$> batches)
    return
        ( Block.BlockExecutionData
            { bedIsNewEpoch = False,
              bedEpochDuration = 3_600_000,
              bedTimestamp = 10_001,
              bedBlockNonce = VRF.prove (VRF.KeyPair electionKey (VRF.pubKey electionKey)) "",
              bedParticipatingBakers = Block.ParticipatingBakers 0 [0],
              bedParentState = parent,
              bedMissedRounds = Map.empty
            },
          ids,
          fromIntegral (length batches) + 1
        )

-- | Apply the effective block-energy limit before executing the fixture.
withEnergyLimit :: Energy -> Block.BlockExecutionData 'P11 -> Helpers.PersistentBSM 'P11 (Block.BlockExecutionData 'P11)
withEnergyLimit limit executionData = do
    state <- BS.thawBlockState $ Block.bedParentState executionData
    updated <- BS.bsoEnqueueUpdate state 0 (UVBlockEnergyLimit limit)
    parent <- BS.freezeBlockState updated
    return executionData{Block.bedParentState = parent}

makeTransfer :: Nonce -> IO BlockItem
makeTransfer nonce = do
    [tx] <-
        Runner.processTransactions
            [ Runner.TJSON
                { payload = Runner.Transfer ownerAddress 1,
                  metadata = makeDummyHeader ownerAddress nonce Helpers.simpleTransferCost,
                  keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
                }
            ]
    return $ toBlockItem $ fromAccountTransaction 0 tx

verification :: Verification.VerificationResult
verification = Verification.Ok Verification.TrustedSuccess

requireExecuted :: Either (Maybe FailureKind) a -> Helpers.PersistentBSM 'P11 a
requireExecuted = either (\failure -> liftIO $ assertFailure ("Block execution failed: " ++ show failure) >> fail "Invalid block") return
