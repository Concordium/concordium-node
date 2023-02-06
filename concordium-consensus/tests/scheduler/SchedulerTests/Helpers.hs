{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SchedulerTests.Helpers (
    module SchedulerTests.Helpers,
    DummyData.makeTestAccountFromSeed,
    DummyData.keyPairFromSeed,
    DummyData.accountAddressFromSeed,
    DummyData.makeTestAccount,
    DummyData.makeTestCredentialFromSeed,
    DummyData.makeTestCredential,
) where

import qualified Control.Monad.Except as Except
import Control.Monad.RWS.Strict
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word
import Lens.Micro.Platform
import Test.HUnit

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.ID.Types as Types
import qualified Concordium.Types.Accounts as Types
import qualified Concordium.Types.Accounts.Releases as Types
import Concordium.Types.SeedState (initialSeedState)
import qualified Concordium.Wasm as Wasm

import qualified Concordium.Common.Time as Time
import qualified Concordium.Cost as Cost
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as BS
import Concordium.GlobalState.Persistent.Cache
import qualified Concordium.GlobalState.Rewards as Rewards
import Concordium.GlobalState.Types
import Concordium.Logger
import Concordium.Scheduler
import qualified Concordium.Scheduler.DummyData as DummyData
import qualified Concordium.Scheduler.EnvironmentImplementation as EI
import qualified Concordium.Scheduler.Runner as SchedTest
import qualified Concordium.Scheduler.Types as Types
import Concordium.TimeMonad

getResults :: [(a, Types.TransactionSummary)] -> [(a, Types.ValidResult)]
getResults = map (\(x, r) -> (x, Types.tsResult r))

-- |The cost for processing a simple transfer (account to account)
-- with one signature in the transaction.
--
-- * @SPEC: <$DOCS/Transactions#transaction-cost-header-simple-transfer>
simpleTransferCost :: Types.Energy
simpleTransferCost = Cost.baseCost (Types.transactionHeaderSize + 41) 1 + Cost.simpleTransferCost

simpleTransferCostWithMemo1 :: Word64 -> Types.Energy
simpleTransferCostWithMemo1 memoSize =
    Cost.baseCost
        (Types.transactionHeaderSize + 41 + 2 + memoSize)
        1

simpleTransferCostWithMemo2 :: Word64 -> Types.Energy
simpleTransferCostWithMemo2 memoSize =
    Cost.baseCost
        (Types.transactionHeaderSize + 41 + 2 + memoSize)
        1
        + Cost.simpleTransferCost

-- | Monad that implements the necessary constraints to be used for running the scheduler.
newtype PersistentBSM pv a = PersistentBSM
    { _runPersistentBSM ::
        BS.PersistentBlockStateMonad
            pv
            (BS.PersistentBlockStateContext pv)
            (Blob.BlobStoreM' (BS.PersistentBlockStateContext pv))
            a
    }
    deriving
        ( Applicative,
          Functor,
          Monad,
          BS.ContractStateOperations,
          BS.ModuleQuery,
          BlockStateTypes,
          Blob.MonadBlobStore,
          MonadIO,
          MonadCache BS.ModuleCache
        )

deriving instance (Types.IsProtocolVersion pv) => BS.AccountOperations (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.BlockStateOperations (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.BlockStateQuery (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => Types.MonadProtocolVersion (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.BlockStateStorage (PersistentBSM pv)

deriving instance
    (Types.AccountVersionFor pv ~ av) =>
    MonadCache (BS.AccountCache av) (PersistentBSM pv)

instance MonadLogger (PersistentBSM pv) where
    logEvent _ _ _ = return ()

instance TimeMonad (PersistentBSM pv) where
    currentTime = return $ read "1970-01-01 13:27:13.257285424 UTC"

-- |Call a function for each protocol version, returning a list of results.
-- Notice the return type for the function must be independent of the protocol version.
--
-- This is used to run a test against every protocol version.
forEveryProtocolVersion ::
    (forall pv. (Types.IsProtocolVersion pv) => Types.SProtocolVersion pv -> String -> a) ->
    [a]
forEveryProtocolVersion check =
    [ check Types.SP1 "P1",
      check Types.SP2 "P2",
      check Types.SP3 "P3",
      check Types.SP4 "P4",
      check Types.SP5 "P5",
      check Types.SP6 "P6"
    ]

-- |Construct a test block state containing the provided accounts.
createTestBlockStateWithAccounts ::
    forall pv.
    Types.IsProtocolVersion pv =>
    [BS.PersistentAccount (Types.AccountVersionFor pv)] ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv)
createTestBlockStateWithAccounts accounts =
    BS.initialPersistentState
        (initialSeedState (Hash.hash "") 1_000)
        DummyData.dummyCryptographicParameters
        accounts
        DummyData.dummyIdentityProviders
        DummyData.dummyArs
        keys
        DummyData.dummyChainParameters
  where
    keys = Types.withIsAuthorizationsVersionForPV (Types.protocolVersion @pv) $ DummyData.dummyKeyCollection

-- |Construct a test block state containing the provided accounts.
createTestBlockStateWithAccountsM ::
    (Types.IsProtocolVersion pv) =>
    [PersistentBSM pv (BS.PersistentAccount (Types.AccountVersionFor pv))] ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv)
createTestBlockStateWithAccountsM accounts =
    createTestBlockStateWithAccounts =<< sequence accounts

-- |Run test block state computation provided an account cache size.
-- The module cache size is 100.
--
-- This function creates a temporary file for the blobstore, which is removed right after the
-- running the computation, meaning the result of the computation should not retain any references
-- and should be fully evaluated.
runTestBlockStateWithCacheSize :: Int -> PersistentBSM pv a -> IO a
runTestBlockStateWithCacheSize cacheSize computation =
    Blob.runBlobStoreTemp "." $
        BS.withNewAccountCache cacheSize $
            BS.runPersistentBlockStateMonad $
                _runPersistentBSM computation

-- |Run test block state computation with a account cache size and module cache size of 100.
--
-- This function creates a temporary file for the blobstore, which is removed right after the
-- running the computation, meaning the result of the computation should not retain any references
-- and should be fully evaluated.
runTestBlockState :: PersistentBSM pv a -> IO a
runTestBlockState = runTestBlockStateWithCacheSize 100

-- |Config for running the scheduler in a test environment.
data TestConfig = TestConfig
    { -- | Maximum block size in bytes.
      tcBlockSize :: Integer,
      -- |Timeout for block construction in milliseconds.
      -- This is the absolute time after which we stop trying to add new transctions to the block.
      tcBlockTimeout :: Time.Timestamp,
      -- |The context state used for running the scheduler.
      tcContextState :: EI.ContextState
    }

-- |Default settings the running the scheduler in a test environment.
defaultTestConfig :: TestConfig
defaultTestConfig =
    TestConfig
        { tcBlockSize = DummyData.dummyBlockSize,
          tcBlockTimeout = Time.utcTimeToTimestamp DummyData.dummyBlockTimeout,
          tcContextState = defaultContextState
        }

defaultContextState :: EI.ContextState
defaultContextState =
    EI.ContextState
        { _chainMetadata = DummyData.dummyChainMeta,
          _maxBlockEnergy = maxBound,
          _accountCreationLimit = maxBound
        }

-- |Result from running the scheduler in a test environment.
data SchedulerResult = SchedulerResult
    { -- | The outcome for constructing a block.
      srTransactions :: FilteredTransactions,
      -- | The total execution cost of the block.
      srExecutionCosts :: Types.Amount,
      -- | The total execution energy of the block.
      srUsedEnergy :: Types.Energy
    }

-- | Run the scheduler on transactions in a test environment.
runScheduler ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    TestConfig ->
    BS.HashedPersistentBlockState pv ->
    Types.GroupedTransactions ->
    PersistentBSM pv (SchedulerResult, BS.PersistentBlockState pv)
runScheduler TestConfig{..} stateBefore transactions = do
    blockStateBefore <- BS.thawBlockState stateBefore
    let txs = filterTransactions tcBlockSize (Time.timestampToUTCTime tcBlockTimeout) transactions
    let schedulerState = EI.makeInitialSchedulerState @(PersistentBSM pv) blockStateBefore
    (filteredTransactions, stateAfter) <- EI.runSchedulerT txs tcContextState schedulerState
    let result =
            SchedulerResult
                { srTransactions = filteredTransactions,
                  srExecutionCosts = stateAfter ^. EI.ssExecutionCosts,
                  srUsedEnergy = stateAfter ^. EI.ssEnergyUsed
                }
    return (result, stateAfter ^. EI.ssBlockState)

-- | Run the scheduler on transactions in a test environment.
-- Allows for a block state monad computation for constructing the initial block state and takes a
-- block state monad computation for extracting relevant values.
--
-- This function creates a temporary file for the blobstore, which is removed right after the
-- running transactions and the extractor, meaning the result of the extractor should not retain any
-- references and should be fully evaluated.
runSchedulerTest ::
    forall pv a.
    (Types.IsProtocolVersion pv) =>
    TestConfig ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv) ->
    (SchedulerResult -> BS.PersistentBlockState pv -> PersistentBSM pv a) ->
    Types.GroupedTransactions ->
    IO (SchedulerResult, a)
runSchedulerTest config constructState extractor transactions = runTestBlockState computation
  where
    computation :: PersistentBSM pv (SchedulerResult, a)
    computation = do
        blockStateBefore <- constructState
        (result, blockStateAfter) <- runScheduler config blockStateBefore transactions
        (result,) <$> extractor result blockStateAfter

-- | Run the scheduler on transactions in a test environment.
-- Allows for a block state monad computation for constructing the initial block state and takes a
-- block state monad computation for extracting relevant values.
--
-- The transactions are provided using TransactionJSON.
runSchedulerTestTransactionJson ::
    forall pv a.
    (Types.IsProtocolVersion pv) =>
    TestConfig ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv) ->
    (SchedulerResult -> BS.PersistentBlockState pv -> PersistentBSM pv a) ->
    [SchedTest.TransactionJSON] ->
    IO (SchedulerResult, a)
runSchedulerTestTransactionJson config constructState extractor transactionJsonList = do
    transactions <- SchedTest.processUngroupedTransactions transactionJsonList
    runSchedulerTest config constructState extractor transactions

-- | Check assertions on the result of running a transaction in the scheduler and the resulting
-- block state.
type TransactionAssertion pv =
    SchedulerResult ->
    BS.PersistentBlockState pv ->
    PersistentBSM pv Assertion

-- |A test transaction paired with assertions to run on the scheduler result and block state.
data TransactionAndAssertion pv = TransactionAndAssertion
    { -- | A transaction to run in the scheduler.
      taaTransaction :: SchedTest.TransactionJSON,
      -- | Assertions to make about the outcome from the scheduler and the resulting block state.
      taaAssertion :: TransactionAssertion pv
    }

-- |Run the scheduler on transactions in a test environment. Each transaction in the list of
-- transactions is paired with the assertions to run on the scheduler result and the resulting block
-- state right after executing each transaction in the intermediate block state.
--
-- This will also run invariant assertions on each intermediate block state, see
-- @assertBlockStateInvariantsH@ for more details.
runSchedulerTestAssertIntermediateStates ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    TestConfig ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv) ->
    [TransactionAndAssertion pv] ->
    Assertion
runSchedulerTestAssertIntermediateStates config constructState transactionsAndAssertions =
    join $ runTestBlockState blockStateComputation
  where
    blockStateComputation :: PersistentBSM pv Assertion
    blockStateComputation = do
        blockStateBefore <- constructState
        (assertions, _, _) <- foldM transactionRunner (return (), blockStateBefore, 0) transactionsAndAssertions
        return assertions

    transactionRunner ::
        (Assertion, BS.HashedPersistentBlockState pv, Types.Amount) ->
        TransactionAndAssertion pv ->
        PersistentBSM pv (Assertion, BS.HashedPersistentBlockState pv, Types.Amount)
    transactionRunner (assertedSoFar, currentState, costsSoFar) step = do
        transactions <- liftIO $ SchedTest.processUngroupedTransactions [taaTransaction step]
        (result, updatedState) <- runScheduler config currentState transactions
        let nextCostsSoFar = costsSoFar + srExecutionCosts result
        doStateAssertions <- assertBlockStateInvariantsH updatedState nextCostsSoFar
        doAssertTransaction <- taaAssertion step result updatedState
        let nextAssertedSoFar = do
                assertedSoFar
                doAssertTransaction
                doStateAssertions
        nextState <- BS.freezeBlockState updatedState
        return (nextAssertedSoFar, nextState, nextCostsSoFar)

-- | Intermediate results collected while running a number of transactions.
type IntermediateResults a = [(SchedulerResult, a)]

-- |Run the scheduler on transactions in a test environment, while collecting all of the
-- intermediate results and extracted values.
runSchedulerTestWithIntermediateStates ::
    forall pv a.
    (Types.IsProtocolVersion pv) =>
    TestConfig ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv) ->
    (SchedulerResult -> BS.PersistentBlockState pv -> PersistentBSM pv a) ->
    Types.GroupedTransactions ->
    IO (IntermediateResults a, BS.HashedPersistentBlockState pv)
runSchedulerTestWithIntermediateStates config constructState extractor transactions =
    runTestBlockState blockStateComputation
  where
    blockStateComputation :: PersistentBSM pv (IntermediateResults a, BS.HashedPersistentBlockState pv)
    blockStateComputation = do
        blockStateBefore <- constructState
        foldM transactionRunner ([], blockStateBefore) transactions

    transactionRunner ::
        (IntermediateResults a, BS.HashedPersistentBlockState pv) ->
        Types.TransactionGroup ->
        PersistentBSM pv (IntermediateResults a, BS.HashedPersistentBlockState pv)
    transactionRunner (acc, currentState) tx = do
        (result, updatedState) <- runScheduler config currentState [tx]
        extracted <- extractor result updatedState
        nextState <- BS.freezeBlockState updatedState
        return (acc ++ [(result, extracted)], nextState)

-- |Save and load block state, used to test the block state written to disc.
--
-- This can be used together with `runSchedulerTest*` functions for creating assertions about the
-- block state on disc.
reloadBlockState ::
    (Types.IsProtocolVersion pv) =>
    BS.PersistentBlockState pv ->
    PersistentBSM pv (BS.PersistentBlockState pv)
reloadBlockState persistentState = do
    frozen <- BS.freezeBlockState persistentState
    br <- BS.saveBlockState frozen
    BS.thawBlockState =<< BS.loadBlockState (BS.hpbsHash frozen) br

-- |Takes a function for checking the block state, which is then run on the block state, the block
-- state is reloaded (save to the blobstore and loaded again) and the check is run again against the
-- reloaded state.
-- This is useful to run checks against both the current in-memory block state and on the block
-- state read from the disc.
checkReloadCheck ::
    (Types.IsProtocolVersion pv) =>
    TransactionAssertion pv ->
    TransactionAssertion pv
checkReloadCheck check result blockState = do
    doCheck <- check result blockState
    reloadedState <- reloadBlockState blockState
    doCheckReloadedState <- check result reloadedState
    return $ do
        doCheck
        doCheckReloadedState

-- |Information accumulated when iterating the accounts in block state during
-- checkBlockStateInvariants.
data AccountAccumulated av = AccountAccumulated
    { -- | Account addresses seen so far. Used to ensure no duplicates between accounts.
      aaAccountsSoFar :: Map.Map Types.AccountAddress Types.AccountIndex,
      -- | Credentials seen so far. Used to ensure no duplicates between accounts.
      aaCredentialsSoFar :: Map.Map Types.RawCredentialRegistrationID Types.AccountIndex,
      -- | Set of baker IDs of the active bakers not seen yet.
      aaBakerIdsLeft :: Set.Set Types.BakerId,
      -- | Set of Delegator IDs of the active delegators not seen yet.
      -- This is only defined for relevant protocol versions.
      aaDelegatorIdsLeft :: DefinedWhenSupportDelegation av (Set.Set Types.DelegatorId),
      -- | Accumulated total public balance.
      aaTotalAccountAmount :: Types.Amount
    }

-- | Hash and check block state for a number of invariants.
--
-- The invariants being checked are:
-- - No duplicate account addresses.
-- - No duplicate account credentials.
-- - For each account
--   - Ensure the tracking of the total locked amount matches the sum of the pending releases.
--   - If the account is a baker, check the baker ID matches account index and it is part of the set
--     of active bakers.
--   - When protocol version supports delegation, see if the account is an active delegator.
--     If so, check the delegator ID matches account index and it is part of the set of active
--     delegators.
-- - Ensure all of the active bakers have a corresponding account in block state.
-- - When protocol version supports delegation, ensure all of the active delegators have a
--   corresponding account in block state.
-- - Ensure the total balance of accounts and instances, matches the total amount tracked in the
--   block state.
assertBlockStateInvariantsH ::
    (Types.IsProtocolVersion pv) =>
    BS.PersistentBlockState pv ->
    Types.Amount ->
    PersistentBSM pv Assertion
assertBlockStateInvariantsH blockState extraBalance = do
    hashedState <- BS.hashBlockState blockState
    assertBlockStateInvariants hashedState extraBalance

-- | Check block state for a number of invariants.
--
-- The invariants being checked are:
-- - No duplicate account addresses.
-- - No duplicate account credentials.
-- - For each account
--   - Ensure the tracking of the total locked amount matches the sum of the pending releases.
--   - If the account is a baker, check the baker ID matches account index and it is part of the set
--     of active bakers.
--   - When protocol version supports delegation, see if the account is an active delegator.
--     If so, check the delegator ID matches account index and it is part of the set of active
--     delegators.
-- - Ensure all of the active bakers have a corresponding account in block state.
-- - When protocol version supports delegation, ensure all of the active delegators have a
--   corresponding account in block state.
-- - Ensure the total balance of accounts and instances, matches the total amount tracked in the
--   block state.
assertBlockStateInvariants ::
    (Types.IsProtocolVersion pv) =>
    BS.HashedPersistentBlockState pv ->
    Types.Amount ->
    PersistentBSM pv Assertion
assertBlockStateInvariants bs extraBalance = do
    result <- Except.runExceptT $ checkBlockStateInvariants bs extraBalance
    return $ either assertFailure return result

-- | Data type for holding data which is only defined for account versions supporting delegation.
data DefinedWhenSupportDelegation (av :: Types.AccountVersion) a where
    Defined :: (Types.AVSupportsDelegation av) => a -> DefinedWhenSupportDelegation av a
    NotDefined :: DefinedWhenSupportDelegation 'Types.AccountV0 a

-- | Check block state for a number of invariants.
--
-- The invariants being checked are:
-- - No duplicate account addresses.
-- - No duplicate account credentials.
-- - For each account
--   - Ensure the tracking of the total locked amount matches the sum of the pending releases.
--   - If the account is a baker, check the baker ID matches account index and it is part of the set
--     of active bakers.
--   - When protocol version supports delegation, see if the account is an active delegator.
--     If so, check the delegator ID matches account index and it is part of the set of active
--     delegators.
-- - Ensure all of the active bakers have a corresponding account in block state.
-- - When protocol version supports delegation, ensure all of the active delegators have a
--   corresponding account in block state.
-- - Ensure the total balance of accounts and instances, matches the total amount tracked in the
--   block state.
checkBlockStateInvariants ::
    forall pv av.
    (Types.IsProtocolVersion pv, Types.IsAccountVersion av, Types.AccountVersionFor pv ~ av) =>
    BS.HashedPersistentBlockState pv ->
    Types.Amount ->
    Except.ExceptT String (PersistentBSM pv) ()
checkBlockStateInvariants bs extraBalance = do
    -- Iterate all of the accounts in block state, check and accumulate the total public balance.
    allAccounts <- BS.getAccountList bs

    -- Get the active bakers and for protocol versions supporting delegation, get the active delegators.
    (allActiveBakers, allActiveDelegators) <- case Types.delegationSupport @av of
        Types.SAVDelegationNotSupported -> do
            allActiveBakers <- Set.fromList <$> BS.getActiveBakers bs
            return (allActiveBakers, NotDefined)
        Types.SAVDelegationSupported -> do
            (allActiveBakerInfoList, passiveActiveDelegatorInfoList) <-
                BS.getActiveBakersAndDelegators bs
            allActiveBakerList <- mapM (BS.loadBakerId . BS.activeBakerInfoRef) allActiveBakerInfoList
            let getDelegators bakerId = do
                    maybeDelegators <- BS.getActiveDelegators bs (Just bakerId)
                    delegators <-
                        maybe
                            (Except.throwError "Delegation to non-existing pool")
                            return
                            maybeDelegators
                    return $ snd <$> delegators
            nonPassiveActiveDelegatorInfoList <- concat <$> mapM getDelegators allActiveBakerList
            let allActiveDelegatorList =
                    BS.activeDelegatorId
                        <$> passiveActiveDelegatorInfoList ++ nonPassiveActiveDelegatorInfoList
            return (Set.fromList allActiveBakerList, Defined @av $ Set.fromList allActiveDelegatorList)

    let initialAccountAccumulated =
            AccountAccumulated
                { aaAccountsSoFar = Map.empty,
                  aaCredentialsSoFar = Map.empty,
                  aaBakerIdsLeft = allActiveBakers,
                  aaDelegatorIdsLeft = allActiveDelegators,
                  aaTotalAccountAmount = 0
                }

    -- Iterates all of the accounts, running checks and accumulates the total public balance.
    accountAccumulated <-
        foldM
            checkAccount
            initialAccountAccumulated
            allAccounts

    -- Ensure all of the active bakers were covered by the above iteration of accounts.
    unless (Set.null $ aaBakerIdsLeft accountAccumulated) $
        Except.throwError $
            "Active bakers with no baker record: " ++ show (aaBakerIdsLeft accountAccumulated)

    -- For protocol versions with delegation:
    -- ensure all of the active bakers were covered by the above iteration of accounts.
    _ :: () <- case aaDelegatorIdsLeft accountAccumulated of
        NotDefined -> return ()
        Defined idsLeft ->
            unless (Set.null idsLeft) $
                Except.throwError $
                    "Active delegators with no delegator record: " ++ show idsLeft

    -- Check the total amount of CCD matches the one being tracked in block state.
    allInstances <- BS.getContractInstanceList bs
    totalAmountInstances <- foldM sumInstanceBalance 0 allInstances
    bankStatus <- BS.bsoGetBankStatus =<< BS.thawBlockState bs
    let totalAmountCalculated =
            aaTotalAccountAmount accountAccumulated
                + (bankStatus ^. Rewards.totalEncryptedGTU)
                + totalAmountInstances
                + (bankStatus ^. Rewards.bankRewardAccounts . to Rewards.rewardsTotal)
                + extraBalance
    let totalAmountBank = bankStatus ^. Rewards.totalGTU
    unless (totalAmountBank == totalAmountCalculated) $
        Except.throwError $
            "Total CCD "
                ++ show totalAmountBank
                ++ " does not match the sum of all accounts, instances and rewards "
                ++ show totalAmountCalculated
  where
    -- Check account and accumulate the total public balance.
    checkAccount ::
        AccountAccumulated av ->
        Types.AccountAddress ->
        Except.ExceptT String (PersistentBSM pv) (AccountAccumulated av)
    checkAccount AccountAccumulated{..} accountAddress = do
        -- Check that we didn't already find this same account.
        when (Map.member accountAddress aaAccountsSoFar) $
            Except.throwError $
                "Duplicate account address: " ++ show accountAddress

        maybeAccount <- BS.getAccount bs accountAddress
        (accountIndex, account) <-
            maybe
                (Except.throwError $ "No account information for address: " ++ show accountAddress)
                return
                maybeAccount

        let nextAccountsSoFar = Map.insert accountAddress accountIndex aaAccountsSoFar

        -- Check that we didn't already find this credential.
        credentials <- BS.accountCredentials account
        nextCredentialsSoFar <-
            foldM
                (checkAndInsertAccountCredential accountIndex)
                aaCredentialsSoFar
                credentials

        -- Check that the locked balance is the same as the sum of the pending releases.
        lockedBalance <- BS.accountLockedAmount account
        sumOfReleases <- Types.releaseTotal <$> BS.accountReleaseSummary account
        unless (sumOfReleases == lockedBalance) $
            Except.throwError $
                "Sum of pending releases ("
                    ++ show sumOfReleases
                    ++ ") does not match the total locked amount "
                    ++ show lockedBalance
                    ++ " for account "
                    ++ show accountAddress

        -- If the account is a baker, check the baker ID matches account index and it is part of the
        -- set of active bakers.
        maybeAccountBakerInfoRef <- BS.accountBakerInfoRef account
        nextBakerIdsLeft <- case maybeAccountBakerInfoRef of
            Nothing -> return aaBakerIdsLeft
            Just bakerInfoRef -> do
                bakerId <- BS.loadBakerId bakerInfoRef
                unless (bakerId == fromIntegral accountIndex) $
                    Except.throwError $
                        "Baker ID ("
                            ++ show bakerId
                            ++ ") does not match the account index ("
                            ++ show accountIndex
                            ++ ") for account "
                            ++ show accountAddress
                unless (Set.member bakerId aaBakerIdsLeft) $
                    Except.throwError $
                        "Account has baker record, but is not an active baker "
                            ++ show bakerId
                            ++ " account "
                            ++ show accountAddress
                return $ Set.delete bakerId aaBakerIdsLeft

        -- When protocol version supports delegation, see if the account is an active delegator.
        -- If so, check the delegator ID matches account index and it is part of the
        -- set of active delegators.
        nextDelegatorIdsLeft <- case aaDelegatorIdsLeft of
            NotDefined -> return aaDelegatorIdsLeft
            Defined idsLeft -> do
                maybeAccountDelegation <- BS.accountDelegator account
                case maybeAccountDelegation of
                    Nothing -> return aaDelegatorIdsLeft
                    Just accountDelegation -> do
                        let delegatorId = Types._delegationIdentity accountDelegation

                        unless (delegatorId == fromIntegral accountIndex) $
                            Except.throwError $
                                "Delegator ID ("
                                    ++ show delegatorId
                                    ++ ") does not match the account index ("
                                    ++ show accountIndex
                                    ++ ") for account "
                                    ++ show accountAddress
                        unless (Set.member delegatorId idsLeft) $
                            Except.throwError $
                                "Account has delegator record, but is not an active delegator "
                                    ++ show delegatorId
                                    ++ " account "
                                    ++ show accountAddress

                        return $ Defined $ Set.delete delegatorId idsLeft

        -- Add the public balance to the accumulated total public balance of accounts.
        publicBalance <- BS.accountAmount account
        let nextTotalAccountAmount = aaTotalAccountAmount + publicBalance

        return
            AccountAccumulated
                { aaAccountsSoFar = nextAccountsSoFar,
                  aaCredentialsSoFar = nextCredentialsSoFar,
                  aaBakerIdsLeft = nextBakerIdsLeft,
                  aaDelegatorIdsLeft = nextDelegatorIdsLeft,
                  aaTotalAccountAmount = nextTotalAccountAmount
                }

    -- Ensure the provided contract address have and entry in the block state and accumulate the
    -- total balance of instances.
    sumInstanceBalance ::
        Types.Amount -> Types.ContractAddress -> Except.ExceptT String (PersistentBSM pv) Types.Amount
    sumInstanceBalance totalInstanceAmount instanceAddress = do
        maybeInstance <- BS.getContractInstance bs instanceAddress
        instanceInfo <-
            maybe
                (Except.throwError $ "No instance information for address: " ++ show instanceAddress)
                return
                maybeInstance
        let instanceAmount = case instanceInfo of
                BS.InstanceInfoV0 info -> BS.iiBalance info
                BS.InstanceInfoV1 info -> BS.iiBalance info
        return $! totalInstanceAmount + instanceAmount

    -- Check whether this is the first time we see a credential.
    checkAndInsertAccountCredential ::
        Types.AccountIndex ->
        Map.Map Types.RawCredentialRegistrationID Types.AccountIndex ->
        Types.RawAccountCredential ->
        Except.ExceptT
            String
            (PersistentBSM pv)
            (Map.Map Types.RawCredentialRegistrationID Types.AccountIndex)
    checkAndInsertAccountCredential accountIndex credentialsSoFar credential = do
        let credentialRegistrationId = Types.credId credential
        when (Map.member credentialRegistrationId credentialsSoFar) $
            Except.throwError $
                "Duplicate account credentials: " ++ show credential
        return $ Map.insert credentialRegistrationId accountIndex credentialsSoFar

-- |Read a WASM file as a smart contract module V0.
readV0ModuleFile :: FilePath -> IO Wasm.WasmModule
readV0ModuleFile filePath = do
    moduleSource <- ByteString.readFile filePath
    return $ Wasm.WasmModuleV0 $ Wasm.WasmModuleV Wasm.ModuleSource{..}

-- |Read a WASM file as a smart contract module V1.
readV1ModuleFile :: FilePath -> IO Wasm.WasmModule
readV1ModuleFile filePath = do
    moduleSource <- ByteString.readFile filePath
    return $ Wasm.WasmModuleV1 $ Wasm.WasmModuleV Wasm.ModuleSource{..}

-- | Assert the scheduler result have added one successful transaction.
assertSuccess :: SchedulerResult -> Assertion
assertSuccess = assertSuccessWhere (const (return ()))

-- | Assert the scheduler result have added one successful transaction and check the events.
assertSuccessWhere :: ([Types.Event] -> Assertion) -> SchedulerResult -> Assertion
assertSuccessWhere assertEvents result =
    case getResults $ ftAdded (srTransactions result) of
        [(_, Types.TxSuccess events)] ->
            assertEvents events
        [(_, Types.TxReject reason)] -> assertFailure $ "Transaction rejected unexpectedly: " ++ show reason
        [] -> assertFailure "No transactions were added"
        other -> assertFailure $ "Multiple transactions were added " ++ show other

-- | Assert the scheduler result have added one successful transaction and check the events are
-- equal to the provided events.
assertSuccessWithEvents :: [Types.Event] -> SchedulerResult -> Assertion
assertSuccessWithEvents expectedEvents =
    assertSuccessWhere (assertEqual "The correct event is produced" expectedEvents)

-- | Assert the number of events produced.
assertNumberOfEvents :: Int -> [Types.Event] -> Assertion
assertNumberOfEvents expectedLength events =
    assertEqual "Correct number of events produced" expectedLength (length events)

-- | Assert the scheduler result have added one rejected transaction and check the reason.
assertRejectWhere :: (Types.RejectReason -> Assertion) -> SchedulerResult -> Assertion
assertRejectWhere assertReason result =
    case getResults $ ftAdded (srTransactions result) of
        [(_, Types.TxReject reason)] ->
            assertReason reason
        [(_, Types.TxSuccess reason)] -> assertFailure $ "Transaction succeeded unexpectedly: " ++ show reason
        [] -> assertFailure "No transactions were added"
        other -> assertFailure $ "Multiple transactions were added " ++ show other

-- | Assert the scheduler result have added one rejected transaction and check the reason.
assertRejectWithReason :: Types.RejectReason -> SchedulerResult -> Assertion
assertRejectWithReason expectedReason =
    assertRejectWhere $
        assertEqual "The correct reject reason is produced" expectedReason

-- | Assert the scheduler result have failed one transaction and check the reason.
assertFailureWithReason :: Types.FailureKind -> SchedulerResult -> Assertion
assertFailureWithReason expectedReason result =
    case ftFailed $ srTransactions result of
        [(_, reason)] ->
            assertEqual
                "The correct reason for failure is produced"
                expectedReason
                reason
        [] -> assertFailure "No transaction failed"
        other -> assertFailure $ "Multiple transactions failed: " ++ show other

-- |Assert the scheduler have used energy the exact energy needed to deploy a provided V0 smart
-- contract module. Assuming the transaction was signed with a single signature.
-- The provided module should be a WASM module and without the smart contract version prefix.
assertUsedEnergyDeploymentV0 :: FilePath -> SchedulerResult -> Assertion
assertUsedEnergyDeploymentV0 sourceFile result = do
    contractModule <- readV0ModuleFile sourceFile
    let len = fromIntegral $ ByteString.length $ Wasm.wasmSource contractModule
        -- size of the module deploy payload
        payloadSize =
            Types.payloadSize $
                Types.encodePayload $
                    Types.DeployModule contractModule
        -- size of the transaction minus the signatures.
        txSize = Types.transactionHeaderSize + fromIntegral payloadSize
    -- transaction is signed with 1 signature
    assertEqual
        "Deployment has correct cost "
        (Cost.baseCost txSize 1 + Cost.deployModuleCost len)
        (srUsedEnergy result)

-- |Assert the scheduler have used energy the exact energy needed to deploy a provided V1 smart
-- contract module. Assuming the transaction was signed with a single signature.
-- The provided module should be a WASM module and without the smart contract version prefix.
assertUsedEnergyDeploymentV1 :: FilePath -> SchedulerResult -> Assertion
assertUsedEnergyDeploymentV1 sourceFile result = do
    contractModule <- readV0ModuleFile sourceFile
    let len = fromIntegral $ ByteString.length $ Wasm.wasmSource contractModule
        payload = Types.DeployModule contractModule
        -- size of the module deploy payload
        payloadSize = Types.payloadSize $ Types.encodePayload payload
        -- size of the transaction minus the signatures.
        txSize = Types.transactionHeaderSize + fromIntegral payloadSize
    -- transaction is signed with 1 signature
    assertEqual
        "Deployment has correct cost "
        (Cost.baseCost txSize 1 + Cost.deployModuleCost len)
        (srUsedEnergy result)

-- |Assert the scheduler have used energy at least the administrative energy needed to
-- initialization a smart contract. Assuming the transaction was signed with a single signature.
-- The provided module should be a WASM module and without the smart contract version prefix.
--
-- It is not practical to check the exact cost because the execution cost of the init function is hard to
-- have an independent number for, other than executing.
assertUsedEnergyInitialization ::
    FilePath ->
    Wasm.InitName ->
    Wasm.Parameter ->
    Maybe Wasm.ByteSize ->
    SchedulerResult ->
    Assertion
assertUsedEnergyInitialization sourceFile initName parameter initialStateSize result = do
    moduleSource <- ByteString.readFile sourceFile
    let modLen = fromIntegral $ ByteString.length moduleSource
        modRef = Types.ModuleRef (Hash.hash moduleSource)
        payload = Types.InitContract 0 modRef initName parameter
        payloadSize = Types.payloadSize $ Types.encodePayload payload
        -- size of the transaction minus the signatures.
        txSize = Types.transactionHeaderSize + fromIntegral payloadSize
        -- transaction is signed with 1 signature
        baseTxCost = Cost.baseCost txSize 1
        -- lower bound on the cost of the transaction, assuming no interpreter energy
        costLowerBound = baseTxCost + Cost.initializeContractInstanceCost 0 modLen initialStateSize
    unless (srUsedEnergy result >= costLowerBound) $
        assertFailure $
            "Actual initialization cost "
                ++ show (srUsedEnergy result)
                ++ " not more than lower bound "
                ++ show costLowerBound
