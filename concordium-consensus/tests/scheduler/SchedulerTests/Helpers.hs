{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SchedulerTests.Helpers where

import qualified Control.Monad.Except as Except
import Control.Monad.RWS.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time
import Data.Word
import Lens.Micro.Platform
import Test.HUnit

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.ID.Types as Types
import qualified Concordium.Types.Accounts.Releases as Types
import Concordium.Types.SeedState (initialSeedState)

import qualified Concordium.Cost as Cost
import qualified Concordium.GlobalState.Basic.BlockState.Account as Basic
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
import Concordium.Scheduler.TreeStateEnvironment
import qualified Concordium.Scheduler.Types as Types
import Concordium.TimeMonad

getResults :: [(a, Types.TransactionSummary)] -> [(a, Types.ValidResult)]
getResults = map (\(x, r) -> (x, Types.tsResult r))

-- | The cost for processing a simple transfer (account to account)
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
          BS.AccountOperations,
          BS.ContractStateOperations,
          BS.ModuleQuery,
          MonadProtocolVersion,
          BlockStateTypes,
          Blob.MonadBlobStore,
          BS.BlockStateQuery,
          BS.BlockStateOperations,
          MonadIO,
          MonadCache BS.ModuleCache,
          BS.BlockStateStorage
        )

deriving instance
    (Types.AccountVersionFor pv ~ av) =>
    MonadCache (BS.AccountCache av) (PersistentBSM pv)

instance MonadLogger (PersistentBSM pv) where
    logEvent _ _ _ = return ()

instance TimeMonad (PersistentBSM pv) where
    currentTime = return $ read "1970-01-01 13:27:13.257285424 UTC"

-- |Call a function for each protocol version, returning a list of results.
-- This is used to run a test against every protocol version.
forEveryProtocolVersion ::
    (forall pv. (Types.IsProtocolVersion pv) => Types.SProtocolVersion pv -> String -> a) ->
    [a]
forEveryProtocolVersion check =
    [ check Types.SP1 "P1",
      check Types.SP2 "P2",
      check Types.SP3 "P3",
      check Types.SP4 "P4",
      check Types.SP5 "P5"
    ]

-- |Construct a test block state containing the provided accounts.
createTestBlockStateWithAccounts ::
    (Types.IsProtocolVersion pv) =>
    [Basic.Account (Types.AccountVersionFor pv)] ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv)
createTestBlockStateWithAccounts accounts =
    BS.initialPersistentState
        (initialSeedState (Hash.hash "") 1_000)
        DummyData.dummyCryptographicParameters
        accounts
        DummyData.dummyIdentityProviders
        DummyData.dummyArs
        DummyData.dummyKeyCollection
        DummyData.dummyChainParameters

-- |Run test block state computation provided a cache size.
runTestBlockStateWithCacheSize :: Int -> PersistentBSM pv a -> IO a
runTestBlockStateWithCacheSize cacheSize computation =
    Blob.runBlobStoreTemp "." $
        BS.withNewAccountCache cacheSize $
            BS.runPersistentBlockStateMonad $
                _runPersistentBSM computation

-- |Run test block state computation with a cache size of 100.
runTestBlockState :: PersistentBSM pv a -> IO a
runTestBlockState = runTestBlockStateWithCacheSize 100

-- |Config for running the scheduler in a test environment.
data TestConfig = TestConfig
    { -- | Maximum block size in bytes.
      tcBlockSize :: Integer,
      -- |Timeout for block construction.
      -- This is the absolute time after which we stop trying to add new transctions to the block.
      tcBlockTimeout :: UTCTime,
      -- |The context state used for running the scheduler.
      tcContextState :: EI.ContextState
    }

-- |Default settings the running the scheduler in a test environment.
defaultTestConfig :: TestConfig
defaultTestConfig =
    TestConfig
        { tcBlockSize = DummyData.dummyBlockSize,
          tcBlockTimeout = DummyData.dummyBlockTimeout,
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
    let txs = filterTransactions tcBlockSize tcBlockTimeout transactions
    let schedulerState = EI.mkInitialSS @(PersistentBSM pv) blockStateBefore
    (filteredTransactions, stateAfter, ()) <- runRWST (_runBSM txs) tcContextState schedulerState

    let result =
            SchedulerResult
                { srTransactions = filteredTransactions,
                  srExecutionCosts = EI._ssSchedulerExecutionCosts stateAfter,
                  srUsedEnergy = EI._ssSchedulerEnergyUsed stateAfter
                }
    return (result, EI._ssBlockState stateAfter)

-- | Run the scheduler on transactions in a test environment.
-- Allows for a block state monad computation for constructing the initial block state and takes a
-- block state monad computation for extracting relevant values.
runSchedulerTest ::
    forall pv a.
    (Types.IsProtocolVersion pv) =>
    TestConfig ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv) ->
    (BS.PersistentBlockState pv -> PersistentBSM pv a) ->
    Types.GroupedTransactions ->
    IO (SchedulerResult, a)
runSchedulerTest config constructState extractor transactions = runTestBlockState computation
  where
    computation :: PersistentBSM pv (SchedulerResult, a)
    computation = do
        blockStateBefore <- constructState
        (result, blockStateAfter) <- runScheduler config blockStateBefore transactions
        (result,) <$> extractor blockStateAfter

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
    (BS.PersistentBlockState pv -> PersistentBSM pv a) ->
    [SchedTest.TransactionJSON] ->
    IO (SchedulerResult, a)
runSchedulerTestTransactionJson config constructState extractor transactionJsonList = do
    transactions <- SchedTest.processUngroupedTransactions transactionJsonList
    runSchedulerTest config constructState extractor transactions

-- | Intermediate results collected while running a number of transactions.
type IntermediateResults a = [(SchedulerResult, a)]

-- | Run the scheduler on transactions in a test environment, while collecting all of the
-- intermediate results and extracted values.
runSchedulerTestWithIntermediateStates ::
    forall pv a.
    (Types.IsProtocolVersion pv) =>
    TestConfig ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv) ->
    (BS.PersistentBlockState pv -> PersistentBSM pv a) ->
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
        extracted <- extractor updatedState
        nextState <- BS.freezeBlockState updatedState
        return (acc ++ [(result, extracted)], nextState)

-- | Save and load block state, used to test the block state written to disc.
reloadBlockState ::
    (Types.IsProtocolVersion pv) =>
    BS.PersistentBlockState pv ->
    PersistentBSM pv (BS.PersistentBlockState pv)
reloadBlockState persistentState = do
    frozen <- BS.freezeBlockState persistentState
    br <- BS.saveBlockState frozen
    BS.thawBlockState =<< BS.loadBlockState (BS.hpbsHash frozen) br

-- | Information accumulated when iterating the accounts in block state during
-- checkBlockStateInvariants.
type AccountAccumulated =
    ( Map.Map Types.AccountAddress Types.AccountIndex,
      Map.Map Types.RawCredentialRegistrationID Types.AccountIndex,
      Set.Set Types.BakerId,
      Types.Amount
    )

-- | Check block state for a number of invariants.
assertBlockStateInvariants ::
    (Types.IsProtocolVersion pv) =>
    BS.HashedPersistentBlockState pv ->
    Types.Amount ->
    PersistentBSM pv Assertion
assertBlockStateInvariants bs extraBalance = do
    result <- Except.runExceptT $ checkBlockStateInvariants bs extraBalance
    return $ either assertFailure return result

-- | Check block state for a number of invariants.
checkBlockStateInvariants ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    BS.HashedPersistentBlockState pv ->
    Types.Amount ->
    Except.ExceptT String (PersistentBSM pv) ()
checkBlockStateInvariants bs extraBalance = do
    -- Iterate all of the accounts in block state, check and accumulate the total public balance.
    allAccounts <- BS.getAccountList bs
    allActiveBakers <- Set.fromList <$> BS.getActiveBakers bs

    (_, _, bakerIdsLeft, totalAmountAccounts) <-
        foldM
            checkAccount
            (Map.empty, Map.empty, allActiveBakers, 0)
            allAccounts
    -- Ensure all of the active bakers were covered by the above iteration of accounts.
    unless (Set.null bakerIdsLeft) $
        Except.throwError $
            "Active bakers with no baker record: " ++ show bakerIdsLeft

    -- Check the total amount of CCD matches the one being tracked in block state.
    allInstances <- BS.getContractInstanceList bs
    totalAmountInstances <- foldM sumInstanceBalance 0 allInstances
    bankStatus <- BS.bsoGetBankStatus =<< BS.thawBlockState bs
    let totalAmountCalculated =
            totalAmountAccounts
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
    checkAccount ::
        AccountAccumulated ->
        Types.AccountAddress ->
        Except.ExceptT String (PersistentBSM pv) AccountAccumulated
    checkAccount (accountsSoFar, credentialsSoFar, bakerIdsLeft, totalAccountAmount) accountAddress = do
        -- check that we didn't already find this same account
        when (Map.member accountAddress accountsSoFar) $
            Except.throwError $
                "Duplicate account address: " ++ show accountAddress

        maybeAccount <- BS.getAccount bs accountAddress
        (accountIndex, account) <-
            maybe
                (Except.throwError $ "No account information for address: " ++ show accountAddress)
                return
                maybeAccount

        let nextAccountsSoFar = Map.insert accountAddress accountIndex accountsSoFar

        -- check that we didn't already find this credential
        credentials <- BS.accountCredentials account
        nextCredentialsSoFar <-
            foldM
                (checkAndInsertAccountCredential accountIndex)
                credentialsSoFar
                credentials

        -- check that the locked balance is the same as the sum of the pending releases
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

        maybeAccountBakerInfoRef <- BS.accountBakerInfoRef account
        nextBakerIdsLeft <- case maybeAccountBakerInfoRef of
            Nothing -> return bakerIdsLeft
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
                unless (Set.member bakerId bakerIdsLeft) $
                    Except.throwError $
                        "Account has baker record, but is not an active baker "
                            ++ show bakerId
                            ++ " account "
                            ++ show accountAddress
                return $ Set.delete bakerId bakerIdsLeft

        publicBalance <- BS.accountAmount account
        let nextTotalAccountAmount = totalAccountAmount + publicBalance

        return (nextAccountsSoFar, nextCredentialsSoFar, nextBakerIdsLeft, nextTotalAccountAmount)

    sumInstanceBalance :: Types.Amount -> Types.ContractAddress -> Except.ExceptT String (PersistentBSM pv) Types.Amount
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

    checkAndInsertAccountCredential ::
        Types.AccountIndex ->
        Map.Map Types.RawCredentialRegistrationID Types.AccountIndex ->
        Types.RawAccountCredential ->
        Except.ExceptT String (PersistentBSM pv) (Map.Map Types.RawCredentialRegistrationID Types.AccountIndex)
    checkAndInsertAccountCredential accountIndex credentialsSoFar credential = do
        let credentialRegistrationId = Types.credId credential
        when (Map.member credentialRegistrationId credentialsSoFar) $
            Except.throwError $
                "Duplicate account credentials: " ++ show credential
        return $ Map.insert credentialRegistrationId accountIndex credentialsSoFar
