{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SchedulerBench.Helpers (
    module SchedulerBench.Helpers,
    DummyData.makeTestAccountFromSeed,
    DummyData.keyPairFromSeed,
    DummyData.accountAddressFromSeed,
    DummyData.makeTestAccount,
    DummyData.makeTestCredentialFromSeed,
    DummyData.makeTestCredential,
) where

import Control.Monad
import Control.Monad.RWS.Strict
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Types.Accounts as Types
import qualified Concordium.Types.Parameters as Types
import Concordium.Types.SeedState (initialSeedStateV0, initialSeedStateV1)

import qualified Concordium.Common.Time as Time
import qualified Concordium.Cost as Cost
import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
import qualified Concordium.GlobalState.AccountMap.ModuleMap as ModuleMap
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as BS
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Types
import Concordium.Logger
import Concordium.Scheduler
import qualified Concordium.Scheduler.DummyData as DummyData
import qualified Concordium.Scheduler.EnvironmentImplementation as EI
import qualified Concordium.Scheduler.Types as Types
import Concordium.TimeMonad
import qualified Data.Bifunctor as Bifunctor

getResults :: [(a, Types.TransactionSummary)] -> [(a, Types.ValidResult)]
getResults = map $ Bifunctor.second Types.tsResult

-- | The cost for processing a simple transfer (account to account)
--  with one signature in the transaction.
--
--  * @SPEC: <$DOCS/Transactions#transaction-cost-header-simple-transfer>
simpleTransferCost :: Types.Energy
simpleTransferCost = Cost.baseCost (Types.transactionHeaderSize + 41) 1 + Cost.simpleTransferCost

-- | Monad that implements the necessary constraints to be used for running the scheduler.
newtype PersistentBSM pv a = PersistentBSM
    { _runPersistentBSM ::
        BS.PersistentBlockStateMonad
            pv
            (BS.PersistentBlockStateContext pv)
            (Blob.BlobStoreT (BS.PersistentBlockStateContext pv) LogIO)
            a
    }
    deriving
        ( Applicative,
          Functor,
          Monad,
          BlockStateTypes,
          MonadIO
        )

deriving instance (Types.IsProtocolVersion pv) => BS.AccountOperations (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.TokenStateOperations StateV1.MutableState (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.PLTQuery (BS.PersistentBlockState pv) StateV1.MutableState (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.PLTQuery (BS.HashedPersistentBlockState pv) StateV1.MutableState (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.BlockStateOperations (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.BlockStateQuery (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => Types.MonadProtocolVersion (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.BlockStateStorage (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.ModuleQuery (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => BS.ContractStateOperations (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => Blob.MonadBlobStore (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => MonadCache BS.ModuleCache (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => LMDBAccountMap.MonadAccountMapStore (PersistentBSM pv)
deriving instance (Types.IsProtocolVersion pv) => ModuleMap.MonadModuleMapStore (PersistentBSM pv)

deriving instance
    (Types.AccountVersionFor pv ~ av) =>
    MonadCache (BS.AccountCache av) (PersistentBSM pv)

instance MonadLogger (PersistentBSM pv) where
    logEvent _ _ _ = return ()

instance TimeMonad (PersistentBSM pv) where
    currentTime = return $ read "1970-01-01 13:27:13.257285424 UTC"

-- | Construct a test block state containing the provided accounts.
createTestBlockStateWithAccounts ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    [BS.PersistentAccount (Types.AccountVersionFor pv)] ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv)
createTestBlockStateWithAccounts accounts = do
    bs <-
        BS.initialPersistentState
            seedState
            DummyData.dummyCryptographicParameters
            accounts
            DummyData.dummyIdentityProviders
            DummyData.dummyArs
            keys
            DummyData.dummyChainParameters
    -- save block state and accounts.
    void $ BS.saveBlockState bs
    void $ BS.saveGlobalMaps bs
    return bs
  where
    keys = Types.withIsAuthorizationsVersionFor (Types.protocolVersion @pv) DummyData.dummyKeyCollection
    seedState = case Types.consensusVersionFor (Types.protocolVersion @pv) of
        Types.ConsensusV0 -> initialSeedStateV0 (Hash.hash "") 1_000
        Types.ConsensusV1 -> initialSeedStateV1 (Hash.hash "") 3_600_000

-- | Construct a test block state containing the provided accounts.
createTestBlockStateWithAccountsM ::
    (Types.IsProtocolVersion pv) =>
    [PersistentBSM pv (BS.PersistentAccount (Types.AccountVersionFor pv))] ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv)
createTestBlockStateWithAccountsM accounts =
    createTestBlockStateWithAccounts =<< sequence accounts

-- | Run test block state computation provided an account cache size.
--  The module cache size is 100.
--
--  This function creates temporary files for the blobstore and account map. These are removed right after the
--  running the computation, meaning the result of the computation should not retain any references
--  and should be fully evaluated.
runTestBlockStateWithCacheSize :: Int -> PersistentBSM pv a -> IO a
runTestBlockStateWithCacheSize cacheSize computation =
    runSilentLogger $
        Blob.runBlobStoreTemp "." $
            BS.withNewAccountCacheAndLMDBAccountMap cacheSize "accountmap" $
                BS.runPersistentBlockStateMonad $
                    _runPersistentBSM computation

-- | Run test block state computation with a account cache size and module cache size of 100.
--
--  This function creates a temporary files for the blobstore and account map. These are removed right after the
--  running the computation, meaning the result of the computation should not retain any references
--  and should be fully evaluated.
runTestBlockState :: PersistentBSM pv a -> IO a
runTestBlockState = runTestBlockStateWithCacheSize 100

-- | Config for running the scheduler in a test environment.
data TestConfig = TestConfig
    { -- | Maximum block size in bytes.
      tcBlockSize :: Integer,
      -- | Timeout for block construction in milliseconds.
      --  This is the absolute time after which we stop trying to add new transactions to the block.
      tcBlockTimeout :: Time.Timestamp,
      -- | The context state used for running the scheduler.
      tcContextState :: EI.ContextState
    }

-- | Default settings the running the scheduler in a test environment.
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

-- | Result from running the scheduler in a test environment.
data SchedulerResult tov = SchedulerResult
    { -- | The outcome for constructing a block.
      srTransactions :: FilteredTransactions tov,
      -- | The total execution cost of the block.
      srExecutionCosts :: Types.Amount,
      -- | The total execution energy of the block.
      srUsedEnergy :: Types.Energy
    }
    deriving (Show)

-- | Run the scheduler on transactions in a test environment.
runScheduler ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    TestConfig ->
    BS.HashedPersistentBlockState pv ->
    Types.GroupedTransactions ->
    PersistentBSM pv (SchedulerResult (Types.TransactionOutcomesVersionFor pv), BS.PersistentBlockState pv)
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
    forall tov pv a.
    (Types.IsProtocolVersion pv, tov ~ Types.TransactionOutcomesVersionFor pv) =>
    TestConfig ->
    PersistentBSM pv (BS.HashedPersistentBlockState pv) ->
    (SchedulerResult tov -> BS.PersistentBlockState pv -> PersistentBSM pv a) ->
    Types.GroupedTransactions ->
    IO (SchedulerResult tov, a)
runSchedulerTest config constructState extractor transactions = runTestBlockState computation
  where
    computation :: PersistentBSM pv (SchedulerResult tov, a)
    computation = do
        blockStateBefore <- constructState
        (result, blockStateAfter) <- runScheduler config blockStateBefore transactions
        (result,) <$> extractor result blockStateAfter
