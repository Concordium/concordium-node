{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module tests the migration of accounts from protocol version 6 to protocol version 7.
-- In particular, it tests that bakers and delegators are migrated correctly, with any bakers
-- or delegators that are in cooldown (for removal or stake reduction) are moved to cooldown after
-- migration. Specifically:
--
--  * Bakers/delegators that are in cooldown for removal are removed and have their stake put in
--    pre-pre-cooldown.
--  * Bakers/delegators that are in cooldown for reduction have their stake reduced and the
--    reduction put in pre-pre-cooldown.
--  * Any account that is put in pre-pre-cooldown is recorded in 'migrationPrePreCooldown'.
--  * Delegators to bakers that are removed (as a result of migration) are moved to passive
--    delegation.
--  * All bakers and delegators that are not removed are correctly recorded in the persistent
--    active bakers.
module GlobalStateTests.AccountsMigrationP6ToP7 where

import Test.HUnit
import Test.Hspec

import Concordium.Types
import Concordium.Types.Accounts

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.EncryptedTransfers
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Genesis.Data
import Concordium.GlobalState.Account
import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule as Transient
import Concordium.GlobalState.CooldownQueue
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Persistent.Account
import qualified Concordium.GlobalState.Persistent.Account.MigrationState as MigrationState
import Concordium.GlobalState.Persistent.Accounts
import Concordium.GlobalState.Persistent.Bakers
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as M
import Concordium.GlobalState.Persistent.Cooldown
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.ID.Types
import Concordium.Scheduler.DummyData
import Concordium.Types.Conditionally
import Concordium.Types.DummyData
import Concordium.Types.Execution
import Concordium.Types.Option
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import GlobalStateTests.Accounts (NoLoggerT (..))
import Lens.Micro.Platform
import System.FilePath
import System.IO.Temp

dummyPersisingAccountData :: Int -> PersistingAccountData
dummyPersisingAccountData seed =
    PersistingAccountData
        { _accountAddress = addr,
          _accountEncryptionKey = encryptionKey,
          _accountVerificationKeys = getAccountInformation 1 creds,
          _accountCredentials = creds,
          _accountRemovedCredentials = makeHashed EmptyRemovedCredentials
        }
  where
    cred = makeTestCredentialFromSeed seed
    creds = Map.singleton 0 (toRawAccountCredential cred)
    addr = accountAddressFromSeed seed
    encryptionKey = toRawEncryptionKey (makeEncryptionKey dummyCryptographicParameters (credId cred))

-- | A dummy account encrypted amount, with a non-trivial self balance.
-- This is used to test the migration of accounts with non-trivial encrypted balances.
dummyAccountEncryptedAmount :: AccountEncryptedAmount
dummyAccountEncryptedAmount =
    initialAccountEncryptedAmount
        { _selfAmount = encryptAmountZeroRandomness dummyCryptographicParameters 10
        }

-- | Create a test account with the given persisting data and stake.
--  The balance of the account is set to 1 billion CCD (10^15 uCCD).
testAccount ::
    forall av.
    (IsAccountVersion av, SupportsPLT av ~ 'False) =>
    PersistingAccountData ->
    AccountStake av ->
    Transient.Account av
testAccount persisting stake =
    Transient.Account
        { _accountPersisting = Transient.makeAccountPersisting persisting,
          _accountNonce = minNonce,
          _accountAmount = 1_000_000_000_000_000,
          _accountEncryptedAmount = dummyAccountEncryptedAmount,
          _accountReleaseSchedule = Transient.emptyAccountReleaseSchedule,
          _accountStaking = stake,
          _accountStakeCooldown = Transient.emptyCooldownQueue (accountVersion @av),
          _accountTokenStateTable = CFalse
        }

-- | Initial stake for a test account, set to 500 million CCD plus @2^accountIndex@ uCCD.
initialStake :: AccountIndex -> Amount
initialStake accIndex = 500_000_000_000_000 + 2 ^ accIndex

-- | Target reduced stake for a test account, set to 10_000 CCD plus @2^accountIndex@ uCCD.
reducedStake :: AccountIndex -> Amount
reducedStake accIndex = 10_000_000_000 + 2 ^ accIndex

-- | Create a baker stake for a given (small (<38)) account index. The stake is set at 500 million
-- CCD plus @2^accountIndex@ uCCD. This is to ensure that any given combination of accounts have a
-- unique total stake.
dummyBakerStake ::
    (AVSupportsDelegation av, SupportsValidatorSuspension av ~ 'False) =>
    (AccountIndex -> Amount) ->
    AccountIndex ->
    StakePendingChange av ->
    AccountStake av
dummyBakerStake compStake accIndex pc =
    AccountStakeBaker $
        AccountBaker
            { _stakedAmount = compStake accIndex,
              _stakeEarnings = True,
              _bakerPendingChange = pc,
              _accountBakerInfo =
                BakerInfoExV1
                    { _bieBakerPoolInfo =
                        BakerPoolInfo
                            { _poolOpenStatus = OpenForAll,
                              _poolMetadataUrl = emptyUrlText,
                              _poolCommissionRates =
                                CommissionRates
                                    { _finalizationCommission = makeAmountFraction 50_000,
                                      _bakingCommission = makeAmountFraction 50_000,
                                      _transactionCommission = makeAmountFraction 50_000
                                    }
                            },
                      _bieBakerInfo =
                        BakerInfo
                            { _bakerSignatureVerifyKey = Sig.verifyKey (bakerSignKey seed),
                              _bakerIdentity = BakerId accIndex,
                              _bakerElectionVerifyKey = VRF.publicKey (bakerElectionKey seed),
                              _bakerAggregationVerifyKey =
                                Bls.derivePublicKey (bakerAggregationKey seed)
                            },
                      _bieIsSuspended = CFalse
                    }
            }
  where
    seed = fromIntegral accIndex

dummyDelegatorStake ::
    (AVSupportsDelegation av) =>
    (AccountIndex -> Amount) ->
    AccountIndex ->
    DelegationTarget ->
    StakePendingChange av ->
    AccountStake av
dummyDelegatorStake compStake accIndex target pc =
    AccountStakeDelegate $
        AccountDelegationV1
            { _delegationTarget = target,
              _delegationStakedAmount = compStake accIndex,
              _delegationStakeEarnings = True,
              _delegationPendingChange = pc,
              _delegationIdentity = DelegatorId accIndex
            }

-- | Create a set of test accounts for migration testing.
--  The accounts consist of 3 bakers, one with no pending changes, one with a reduction and one
--  with a removal. Each baker has 3 delegators, one with no pending changes, one with a reduction
--  and one with a removal. There are also 3 passive delegators, similarly configured.
setupTestAccounts :: (SupportsPersistentAccount 'P6 m, MonadFail m) => m (Accounts 'P6)
setupTestAccounts = do
    a0 <- mkBakerAccount 0 NoChange
    a1 <- mkBakerAccount 1 (ReduceStake (reducedStake 1) (PendingChangeEffectiveV1 1000))
    a2 <- mkBakerAccount 2 (RemoveStake (PendingChangeEffectiveV1 2000))
    a3 <- mkDelegatorAccount 3 (DelegateToBaker 0) NoChange
    a4 <-
        mkDelegatorAccount
            4
            (DelegateToBaker 0)
            (ReduceStake (reducedStake 4) (PendingChangeEffectiveV1 3000))
    a5 <- mkDelegatorAccount 5 (DelegateToBaker 0) (RemoveStake (PendingChangeEffectiveV1 4000))
    a6 <- mkDelegatorAccount 6 (DelegateToBaker 1) NoChange
    a7 <-
        mkDelegatorAccount
            7
            (DelegateToBaker 1)
            (ReduceStake (reducedStake 7) (PendingChangeEffectiveV1 5000))
    a8 <- mkDelegatorAccount 8 (DelegateToBaker 1) (RemoveStake (PendingChangeEffectiveV1 6000))
    a9 <- mkDelegatorAccount 9 (DelegateToBaker 2) NoChange
    a10 <-
        mkDelegatorAccount
            10
            (DelegateToBaker 2)
            (ReduceStake (reducedStake 10) (PendingChangeEffectiveV1 7000))
    a11 <- mkDelegatorAccount 11 (DelegateToBaker 2) (RemoveStake (PendingChangeEffectiveV1 8000))
    a12 <- mkDelegatorAccount 12 DelegatePassive NoChange
    a13 <-
        mkDelegatorAccount
            13
            DelegatePassive
            (ReduceStake (reducedStake 13) (PendingChangeEffectiveV1 9000))
    a14 <- mkDelegatorAccount 14 DelegatePassive (RemoveStake (PendingChangeEffectiveV1 10_000))
    accounts0 <- emptyAccounts
    accounts1 <-
        foldM
            (\accts a -> snd <$> putNewAccount a accts)
            accounts0
            [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14]
    -- Store and load the accounts to ensure the data is flushed.
    loadRef =<< storeRef accounts1
  where
    mkBakerAccount accIdx pc =
        makePersistentAccount $
            testAccount
                (dummyPersisingAccountData (fromIntegral accIdx))
                (dummyBakerStake initialStake accIdx pc)
    mkDelegatorAccount accIndex target pc =
        makePersistentAccount $
            testAccount
                (dummyPersisingAccountData (fromIntegral accIndex))
                (dummyDelegatorStake initialStake accIndex target pc)

initPersistentActiveBakers ::
    forall pv m.
    (SupportsPersistentAccount pv m, PVSupportsDelegation pv) =>
    Accounts pv ->
    m (PersistentActiveBakers (AccountVersionFor pv))
initPersistentActiveBakers = foldAccounts addAcct emptyPersistentActiveBakers
  where
    addAcct pab acct =
        accountStake acct >>= \case
            AccountStakeNone -> return pab
            AccountStakeBaker b -> do
                let upd Nothing = return ((), Trie.Insert emptyPersistentActiveDelegators)
                    upd _ = return ((), Trie.NoChange)
                (_, newActiveBakers) <- Trie.adjust upd (b ^. bakerIdentity) (pab ^. activeBakers)
                newAggregationKeys <-
                    Trie.insert (b ^. bakerAggregationVerifyKey) () (pab ^. aggregationKeys)
                return $!
                    pab
                        & activeBakers .~ newActiveBakers
                        & totalActiveCapital . tacAmount +~ (b ^. stakedAmount)
                        & aggregationKeys .~ newAggregationKeys
            AccountStakeDelegate d -> do
                (totalActiveCapital . tacAmount +~ delAmt)
                    <$> case d ^. delegationTarget of
                        DelegatePassive -> do
                            passiveDelegators (addDelegatorHelper delId delAmt) pab
                        DelegateToBaker bid -> do
                            activeBakers (fmap snd . Trie.adjust upd bid) pab
              where
                delId = d ^. delegationIdentity
                delAmt = d ^. delegationStakedAmount
                upd Nothing = do
                    singletonPAD <-
                        addDelegatorHelper delId delAmt emptyPersistentActiveDelegators
                    return ((), Trie.Insert singletonPAD)
                upd (Just pad) = do
                    newPAD <- addDelegatorHelper delId delAmt pad
                    return ((), Trie.Insert newPAD)

migrationTest :: PersistentBlockStateContext 'P6 -> PersistentBlockStateContext 'P7 -> Expectation
migrationTest c0 c1 = runNoLoggerT $ flip runBlobStoreT c0 $ do
    accounts <- setupTestAccounts
    pab <- initPersistentActiveBakers accounts
    flip runBlobStoreT c1 $ do
        initMigrationState :: MigrationState.AccountMigrationState 'P6 'P7 <-
            MigrationState.makeInitialAccountMigrationState accounts pab

        (newAccounts :: Accounts 'P7, newMigrationState) <-
            MigrationState.runAccountMigrationStateTT
                (migrateAccounts @'P6 @'P7 StateMigrationParametersP6ToP7 accounts)
                initMigrationState
        assertMigrationStateCorrect newMigrationState
        assertAccountsCorrect newAccounts
        unless (accountDiffMapRef accounts == accountDiffMapRef newAccounts) $
            liftIO $
                assertFailure "Expected the same account difference map"

-- | Assert that the accounts marked as in pre-pre-cooldown and the persistent active bakers are as
--  expected after migration from the test accounts.
assertMigrationStateCorrect :: forall m. (MonadBlobStore m) => MigrationState.AccountMigrationState 'P6 'P7 -> m ()
assertMigrationStateCorrect migrationState = do
    prePreCooldownList <- loadAccountList (migrationState ^. MigrationState.migrationPrePreCooldown . unconditionally)
    -- All accounts that were in cooldown before migration should be in pre-pre-cooldown after migration.
    let expectPrePreCooldownList = [14, 13, 11, 10, 8, 7, 5, 4, 2, 1]
    liftIO $ assertEqual "Expected pre-pre-cooldown list" expectPrePreCooldownList prePreCooldownList
    let pab = migrationState ^. MigrationState.persistentActiveBakers . unconditionally
    let unDel :: PersistentActiveDelegators 'AccountV3 -> m ([DelegatorId], Amount)
        unDel PersistentActiveDelegatorsV1{..} = (,adDelegatorTotalCapital) <$> Trie.keysAsc adDelegators
    actBkrs <- mapM unDel =<< Trie.toMap (pab ^. activeBakers)
    let expectActiveBakers =
            Map.fromList
                [ (0, ([3, 4], initialStake 3 + reducedStake 4)),
                  (1, ([6, 7], initialStake 6 + reducedStake 7))
                ]
    liftIO $ assertEqual "Active bakers" expectActiveBakers actBkrs
    aggKeys <- Trie.keys (pab ^. aggregationKeys)
    -- Note: the aggregation keys happen to be in this order. Technically the order doesn't matter.
    let expectAggreationKeys = Bls.derivePublicKey . bakerAggregationKey <$> [0, 1]
    liftIO $ assertEqual "Aggregation keys" expectAggreationKeys aggKeys
    pasvDlg <- unDel (pab ^. passiveDelegators)
    let expectPassiveDelegators = ([9, 10, 12, 13], initialStake 9 + reducedStake 10 + initialStake 12 + reducedStake 13)
    liftIO $ assertEqual "Passive delegators" expectPassiveDelegators pasvDlg
    let actCapital = pab ^. totalActiveCapital . tacAmount
    let expectTotalActiveCapital = sum (initialStake <$> [0, 3, 6, 9, 12]) + sum (reducedStake <$> [1, 4, 7, 10, 13])
    liftIO $ assertEqual "Total active capital" expectTotalActiveCapital actCapital

assertAccountsCorrect :: forall m. (SupportsPersistentAccount 'P7 m, MonadFail m) => Accounts 'P7 -> m ()
assertAccountsCorrect accounts = do
    accountExpect 0 (dummyBakerStake initialStake 0 NoChange) Nothing
    accountExpect 1 (dummyBakerStake reducedStake 1 NoChange) (prePreExpect (cooldownReduce 1))
    accountExpect 2 AccountStakeNone (prePreExpect (initialStake 2))
    accountExpect 3 (dummyDelegatorStake initialStake 3 (DelegateToBaker 0) NoChange) Nothing
    accountExpect 4 (dummyDelegatorStake reducedStake 4 (DelegateToBaker 0) NoChange) (prePreExpect (cooldownReduce 4))
    accountExpect 5 AccountStakeNone (prePreExpect (initialStake 5))
    accountExpect 6 (dummyDelegatorStake initialStake 6 (DelegateToBaker 1) NoChange) Nothing
    accountExpect 7 (dummyDelegatorStake reducedStake 7 (DelegateToBaker 1) NoChange) (prePreExpect (cooldownReduce 7))
    accountExpect 8 AccountStakeNone (prePreExpect (initialStake 8))
    accountExpect 9 (dummyDelegatorStake initialStake 9 DelegatePassive NoChange) Nothing
    accountExpect 10 (dummyDelegatorStake reducedStake 10 DelegatePassive NoChange) (prePreExpect (cooldownReduce 10))
    accountExpect 11 AccountStakeNone (prePreExpect (initialStake 11))
    accountExpect 12 (dummyDelegatorStake initialStake 12 DelegatePassive NoChange) Nothing
    accountExpect 13 (dummyDelegatorStake reducedStake 13 DelegatePassive NoChange) (prePreExpect (cooldownReduce 13))
    accountExpect 14 AccountStakeNone (prePreExpect (initialStake 14))
  where
    availableExpect accIndex = 1_000_000_000_000_000 - initialStake accIndex
    cooldownReduce accIndex = initialStake accIndex - reducedStake accIndex
    prePreExpect amt =
        Just
            ( Cooldowns
                { inCooldown = Map.empty,
                  preCooldown = Absent,
                  prePreCooldown = Present amt
                }
            )
    accountExpect accIndex expectStake expectCooldowns = do
        (Just a) <- indexedAccount accIndex accounts
        liftIO . assertEqual ("Account " ++ show accIndex ++ " stake") expectStake
            =<< accountStake a
        liftIO . assertEqual ("Account " ++ show accIndex ++ " cooldowns") expectCooldowns
            =<< accountCooldowns a
        liftIO . assertEqual ("Account " ++ show accIndex ++ " available amount") (availableExpect accIndex)
            =<< accountAvailableAmount a

tests :: Spec
tests = describe "GlobalStateTests.AccountsMigrationP6ToP7"
    $ around
        ( \kont ->
            withTempDirectory "." "blockstate" $ \dir ->
                bracket
                    ( do
                        c0 <- createPBSC dir "0"
                        c1 <- createPBSC dir "1"
                        return (c0, c1)
                    )
                    ( \(c0, c1) -> do
                        destroyPBSC c0
                        destroyPBSC c1
                    )
                    kont
        )
    $ do
        it "migration" (uncurry migrationTest)
  where
    createPBSC dir i = do
        pbscBlobStore <- createBlobStore (dir </> ("blockstate" ++ i ++ ".dat"))
        -- Set the account cache size to 0 to ensure that the accounts are always loaded from the
        -- blob store.
        pbscAccountCache <- newAccountCache 0
        pbscModuleCache <- M.newModuleCache 0
        pbscAccountMap <- LMDBAccountMap.openDatabase (dir </> ("accountmap" ++ i))
        return PersistentBlockStateContext{..}
    destroyPBSC PersistentBlockStateContext{..} = do
        closeBlobStore pbscBlobStore
        LMDBAccountMap.closeDatabase pbscAccountMap
