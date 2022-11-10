{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |Tests that make sure that the persistent state implementation
 correctly handles the different cases of smart contract updates,
 i.e., if the state, or module, or amount was updated
-}
module SchedulerTests.SmartContracts.V1.UpgradingPersistent (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec

import Control.Monad.RWS.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize as S
import qualified Data.Set as Set
import System.IO.Unsafe

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

-- The module which supports an upgrade.
testModuleSourceFile :: FilePath
testModuleSourceFile = "./testdata/contracts/v1/upgrading-cases.wasm"

-- The module we will upgrade to
targetSourceFile :: FilePath
targetSourceFile = "./testdata/contracts/v1/upgrading-cases-target.wasm"

-- |Get a 'ModuleRef' from a given V1 'Module' specified via the 'FilePath'.
{-# NOINLINE getModuleRefFromV1File #-}
getModuleRefFromV1File :: FilePath -> Types.ModuleRef
getModuleRefFromV1File f = unsafePerformIO $ do
    Wasm.getModuleRef @Wasm.V1 . Wasm.WasmModuleV . Wasm.ModuleSource <$> BS.readFile f

-- Construct a basic upgrade test case.
-- Deploy two modules, initialize an instance from the module that supports an upgrade,
-- and then do the upgrade.
testCase :: Bool -> Bool -> [SchedTest.TransactionJSON]
testCase changeAmount changeState =
    [ SchedTest.TJSON
        { payload = SchedTest.DeployModule Wasm.V1 testModuleSourceFile
        , metadata = makeDummyHeader alesAccount 1 1_000
        , keys = [(0, [(0, alesKP)])]
        }
    , SchedTest.TJSON
        { payload = SchedTest.DeployModule Wasm.V1 targetSourceFile
        , metadata = makeDummyHeader alesAccount 2 1_000
        , keys = [(0, [(0, alesKP)])]
        }
    , SchedTest.TJSON
        { payload = SchedTest.InitContract 0 Wasm.V1 testModuleSourceFile "init_contract" ""
        , metadata = makeDummyHeader alesAccount 3 1_000
        , keys = [(0, [(0, alesKP)])]
        }
    , SchedTest.TJSON
        { payload = SchedTest.Update (if changeAmount then 123 else 0) (Types.ContractAddress 0 0) "contract.upgrade" upgradeParameters
        , metadata = makeDummyHeader alesAccount 4 10_000
        , keys = [(0, [(0, alesKP)])]
        }
    ]
  where
    -- the upgrade parameters are the module to upgrade to and a tag stating whether the state should or should not be updated
    upgradeParameters = BSS.toShort (S.runPut (S.put (getModuleRefFromV1File targetSourceFile) <> S.putWord8 (if changeState then 1 else 0)))

runTest :: forall a. Bool -> Bool -> Bool -> (PersistentBlockState 'Types.P5 -> PersistentBSM a) -> IO ([(Types.BlockItem, TransactionSummary)], a)
runTest changeAmount changeState reloadState k = do
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
        let txs = filterTransactions 3_000_000 timeout =<< BSM (liftIO (SchedTest.processUngroupedTransactions (testCase changeAmount changeState)))
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

-- Run the upgrade tests in different scenarios. The boolean flags indicate
-- whether the amount should be changed, the balance should be changed, or the
-- state should be reloaded before inspecting it.
runUpgradeTests :: Bool -> Bool -> Bool -> Assertion
runUpgradeTests changeAmount changeState reloadState = do
    (outcomes, (params, bal, newState)) <- runTest changeAmount changeState reloadState $ \ubs ->
        bsoGetInstance ubs (Types.ContractAddress 0 0) >>= \case
            Nothing -> error "Missing instance."
            Just (InstanceInfoV0 _) -> error "Expected V1 instance, but got V0."
            Just (InstanceInfoV1 ii) -> do
              let Instances.InstanceStateV1 s = iiState ii
              bs <- StateV1.toByteString s
              return (iiParameters ii, iiBalance ii, bs)
    forM_ outcomes $ \(_, summary) -> do
        case tsResult summary of
            TxSuccess{} -> return ()
            TxReject{..} -> assertFailure $ "Transaction rejected: " ++ show vrRejectReason
    assertEqual "No entrypoints in the upgraded contract" Set.empty (Types.instanceReceiveFuns params)
    let mi = Types.instanceModuleInterface params
    assertEqual "Upgrade to the new module interface" (getModuleRefFromV1File targetSourceFile) (GSWasm.miModuleRef mi)
    if changeAmount then
      assertEqual "Amount was updated" 123 bal
    else
      assertEqual "Amount was not updated" 0 bal
    if changeState then
      assertEqual "State was updated" 1 (BS.index newState 0) -- non-empty state serialization starts with a 1 tag.
    else
      assertEqual "State was not updated" (BS.singleton 0) newState  -- empty state serialization just puts a 0 tag.

tests :: Spec
tests = describe "Upgrade contract cases with persistent state" $ do
    specify "V1: Just module upgrade" $ runUpgradeTests False False False
    specify "V1: Module + balance update" $ runUpgradeTests False True False
    specify "V1: Module + state update" $ runUpgradeTests True False False
    specify "V1: Module + state + balance" $ runUpgradeTests True True False
    specify "V1: Reload: Just module upgrade" $ runUpgradeTests False False True
    specify "V1: Reload: Module + balance update" $ runUpgradeTests False True True
    specify "V1: Reload: Module + state update" $ runUpgradeTests True False True
    specify "V1: Reload: Module + state + balance" $ runUpgradeTests True True True
