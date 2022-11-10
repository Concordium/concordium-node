{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |Helpers for running scheduler unit tests using persistent state.
module SchedulerTests.SmartContracts.V1.PersistentStateHelpers where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec

import Control.Monad.RWS.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize as S
import qualified Data.Set as Set
import System.IO.Unsafe
import Data.Word

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Scheduler.Types as Types

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.BlockState.Modules
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.Logger
import Concordium.Scheduler
import Concordium.Scheduler.EnvironmentImplementation
import qualified Concordium.Scheduler.EnvironmentImplementation as EI
import qualified Concordium.Scheduler.Runner as SchedTest
import Concordium.Scheduler.TreeStateEnvironment
import Concordium.TimeMonad
import Concordium.Types.Execution
import Concordium.Types.SeedState (initialSeedState)
import qualified Concordium.Wasm as Wasm
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import qualified Concordium.GlobalState.Persistent.Instances as Instances

import Concordium.Crypto.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Scheduler.DummyData
import Concordium.Types.DummyData

import SchedulerTests.TestUtils

-- |Monad that implements the necessary constraints to be used for
-- running the scheduler.
newtype PersistentBSM a = PersistentBSM {_runPersistentBSM :: PersistentBlockStateMonad PV5 (PersistentBlockStateContext PV5) (BlobStoreM' (PersistentBlockStateContext PV5)) a}
    deriving (Applicative, Functor, Monad,
              AccountOperations,
              ContractStateOperations,
              ModuleQuery,
              MonadProtocolVersion,
              BlockStateTypes,
              MonadBlobStore,
              BlockStateQuery,
              BlockStateOperations,
              MonadIO,
              MonadCache (AccountCache 'Types.AccountV2),
              MonadCache ModuleCache, BlockStateStorage)

instance MonadLogger PersistentBSM where
    logEvent _ _ _ = return ()

instance TimeMonad PersistentBSM where
    currentTime = return $ read "1970-01-01 13:27:13.257285424 UTC"

-- an initial state with a single account with 10CCD
initialBlockState :: PersistentBSM (HashedPersistentBlockState PV5)
initialBlockState =
    initialPersistentState
        (initialSeedState (Hash.hash "") 1_000)
        dummyCryptographicParameters
        [mkAccount alesVK alesAccount 10_000_000]
        dummyIdentityProviders
        dummyArs
        dummyKeyCollection
        dummyChainParameters

runTest :: forall a. [SchedTest.TransactionJSON] -> Bool -> (PersistentBlockState 'Types.P5 -> PersistentBSM a) -> IO ([(Types.BlockItem, TransactionSummary)], a)
runTest input reloadState k = do
    (Types.FilteredTransactions{..}, r) <- runBlobStoreTemp "." . withNewAccountCache 100 . runPersistentBlockStateMonad . _runPersistentBSM $ computation
    assertEqual "No failed transactions" [] ftFailed
    assertEqual "No failed credentials" [] ftFailedCredentials
    assertEqual "No failed updates" [] ftFailedUpdates
    assertEqual "No unprocessed transactions" [] ftUnprocessed
    assertEqual "No unprocessed credentials" [] ftUnprocessedCredentials
    assertEqual "No unprocessed updates" [] ftUnprocessedUpdates
    return (map (\(a, s) -> (fst a, s)) ftAdded, r)
  where
    computation :: PersistentBSM (FilteredTransactions, a)
    computation = do
        s <- thawBlockState =<< initialBlockState
        let txs = filterTransactions 3_000_000 timeout =<< BSM (liftIO (SchedTest.processUngroupedTransactions input))
        (ft, fs, ()) <- runRWST (_runBSM txs) contextState (EI.mkInitialSS @PersistentBSM s)
        newState <-
            if reloadState
                then do
                    frozen <- freezeBlockState (_ssBlockState fs)
                    br <- saveBlockState frozen
                    thawBlockState =<< loadBlockState (hpbsHash frozen) br
                else return (_ssBlockState fs)
        (ft,) <$> k newState

    contextState =
        ContextState
            { _chainMetadata = dummyChainMeta
            , _maxBlockEnergy = 3_000_000
            , _accountCreationLimit = 10
            }
    -- timeout for block construction. We do not care about that here, so set to something after current time
    timeout = read "3000-01-01 13:27:13.257285424 UTC"
