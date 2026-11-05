{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.KonsensusV1.AutoCancelExpiredLocks (tests) where

import Codec.CBOR.Encoding
import qualified Codec.CBOR.Term as Term
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when)
import Control.Monad.IO.Class
import Data.Bool.Singletons
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Serialize as Serialize
import GHC.Clock (getMonotonicTimeNSec)
import Lens.Micro.Platform
import System.Environment (lookupEnv)
import System.Mem (performGC)
import Test.HUnit
import Test.Hspec hiding (after, before, pending)
import Text.Printf (printf)

import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.CapitalDistribution (makeCapitalDistribution)
import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.Account as Account
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.TransactionTable as TT
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
        assertBool ("Setup transaction not included: " ++ show (snd <$> ftFailed (Helpers.srTransactions result))) (not $ null $ ftAdded $ Helpers.srTransactions result)
        Helpers.assertSuccess result
    BS.freezeBlockState updated

-- Setup uses valid batches of at most 100 locks; no setup or signing is timed.
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

tests :: Spec
tests = do
    describe "Automatic lock cancellation" $
        it "REQ-001-S2: production and replay unlock expired funds using post-transaction capacity" $
            Helpers.runTestBlockState @'P11 $ do
                (fixture, ids, nonce) <- makeFixture 3 True
                (firstId, remainingIds) <- case ids of
                    first : rest -> return (first, rest)
                    [] -> liftIO $ assertFailure "Expected funded locks" >> fail "Empty fixture"
                before <- observeLockBalance $ Block.bedParentState fixture
                liftIO $ assertEqual "Funded locks before block execution" (ids, TokenAmount 750 0, TokenAmount 0 0) before
                -- All three fit without a transaction: retain the original group 6 regression.
                (unrestricted, energy) <- requireExecuted =<< Block.executeBlockState fixture []
                after <- observeLockBalance unrestricted
                liftIO $ do
                    assertEqual "Expired locks are destroyed; total holdings stay unchanged" ([], TokenAmount 750 0, TokenAmount 750 0) after
                    assertEqual "Cleanup reports energy for three references" 1_500 energy
                executionData <- withEnergyLimit (Helpers.simpleTransferCost + 500) fixture
                tx <- liftIO $ makeTransfer nonce
                let table = snd $ TT.addTransaction tx 0 verification TT.emptyTransactionTable
                    pending = case wmdData tx of
                        NormalTransaction accountTx -> TT.addPendingTransaction nonce accountTx TT.emptyPendingTransactionTable
                        _ -> error "Expected normal transaction"
                (included, produced, producedEnergy) <- Block.constructBlockState defaultRuntimeParameters table pending executionData
                let added = ftAdded included
                liftIO $ do
                    assertEqual "Included transaction is not displaced by cleanup" [tx] (fst . fst <$> added)
                    assertEqual "Individual transaction energy excludes cleanup" [Helpers.simpleTransferCost] (tsEnergyCost . snd <$> added)
                    assertBool "Transaction leaves less than two cancellations" (Helpers.simpleTransferCost + 500 < 1_500)
                (replayed, replayEnergy) <- requireExecuted =<< Block.executeBlockState executionData [(fst $ fst item, verification) | item <- added]
                producedBalance <- observeLockBalance produced
                replayBalance <- observeLockBalance replayed
                producedOutcomes <- BS.getSpecialOutcomes produced
                replayOutcomes <- BS.getSpecialOutcomes replayed
                producedHash <- BS.getStateHash produced
                replayHash <- BS.getStateHash replayed
                producedOutcomeHash <- BS.getTransactionOutcomesHash produced
                replayOutcomeHash <- BS.getTransactionOutcomesHash replayed
                let expectedCleanup = [SpecialUnlockAmount (HolderAccount ownerAddress) firstId token (TokenAmount 250 0), SpecialLockDestroyed firstId]
                    outcomes = toList producedOutcomes
                liftIO $ do
                    assertEqual "Only one lock fits post-transaction capacity" (remainingIds, TokenAmount 750 0, TokenAmount 250 0) producedBalance
                    assertEqual "Production/replay balances" producedBalance replayBalance
                    assertEqual "Reported energy includes cleanup, exactly filling the effective limit" (Helpers.simpleTransferCost + 500) producedEnergy
                    assertEqual "Production/replay energy" producedEnergy replayEnergy
                    assertEqual "Decoded cleanup outcomes precede rewards" expectedCleanup (take 2 outcomes)
                    assertBool "Reward outcomes follow cleanup" (not $ null $ drop 2 outcomes)
                    assertEqual "Production/replay special outcomes" producedOutcomes replayOutcomes
                    assertEqual "Production/replay state hashes" producedHash replayHash
                    assertEqual "Production/replay outcome hashes" producedOutcomeHash replayOutcomeHash
    benchmark <- runIO $ lookupEnv "AUTO_CANCEL_BENCHMARK"
    when (benchmark == Just "1") $
        describe "Automatic lock cancellation performance" $
            it "times complete blocks after persistent setup" $
                forM_ [10_000, 30_000, 60_000] benchmarkCleanup

-- Opt-in benchmark through the real received-block path, including decoded special outcomes and hashing.
benchmarkCleanup :: Int -> IO ()
benchmarkCleanup count = forM_ [False, True] $ \funded -> Helpers.runTestBlockState @'P11 $ do
    (fixture, ids, nonce) <- makeFixture count funded
    before <- observeLockBalance $ Block.bedParentState fixture
    liftIO $ assertEqual "Equivalent initial backlog" ids (let (locks, _, _) = before in locks)
    executionData <- withEnergyLimit (if funded then Helpers.simpleTransferCost else 0) fixture
    transfers <- liftIO $ mapM makeTransfer [nonce .. nonce + 2]
    let runBlock parent iteration = do
            liftIO performGC
            start <- liftIO getMonotonicTimeNSec
            let transactions = if funded then [(transfers !! iteration, verification)] else []
            (state, energy) <- requireExecuted =<< Block.executeBlockState executionData{Block.bedParentState = parent} transactions
            outcomes <- BS.getSpecialOutcomes state
            outcomeHash <- BS.getTransactionOutcomesHash state
            stateHash <- BS.getStateHash state
            -- Force outcomes and hashes inside timing rather than defer work until assertions.
            _ <-
                liftIO $
                    evaluate $
                        Bytes.length (Serialize.encode outcomeHash)
                            + Bytes.length (Serialize.encode stateHash)
                            + sum [Bytes.length $ Serialize.encode outcome | outcome <- toList outcomes]
            end <- liftIO getMonotonicTimeNSec
            (locks, total, available) <- observeLockBalance state
            liftIO $ do
                assertEqual "Cleanup uses only available capacity" (if funded then Helpers.simpleTransferCost else 0) energy
                assertEqual "Persistent backlog after block" (if funded then ids else []) locks
                assertEqual "Total holdings unchanged" (TokenAmount (fromIntegral count * 250) 0) total
                assertEqual "Expected available balance" (if funded then TokenAmount 0 0 else total) available
                let cleanupOutcomes = [outcome | outcome@SpecialLockDestroyed{} <- toList outcomes]
                assertEqual "Special destruction outcomes" (if funded then [] else SpecialLockDestroyed <$> ids) cleanupOutcomes
                printf
                    "automatic-cleanup count=%d scenario=%s iteration=%d elapsed_ms=%.3f remaining_locks=%d energy=%s special_outcomes=%d\n"
                    count
                    (if funded then "funded-skip" else "empty-remove" :: String)
                    (iteration + 1)
                    (fromIntegral (end - start) / 1_000_000 :: Double)
                    (length locks)
                    (show energy)
                    (Seq.length outcomes)
            return state
    -- Removal iterations use equivalent parent roots; skipping advances through three actual blocks.
    _ <-
        foldM
            (\parent iteration -> runBlock (if funded then parent else Block.bedParentState executionData) iteration)
            (Block.bedParentState executionData)
            [0 .. 2]
    return ()
