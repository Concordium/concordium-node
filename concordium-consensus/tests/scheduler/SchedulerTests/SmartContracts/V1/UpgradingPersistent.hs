{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tests that make sure that the persistent state implementation
--  correctly handles the different cases of smart contract updates,
--  i.e., if the state, or module, or amount was updated
module SchedulerTests.SmartContracts.V1.UpgradingPersistent (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize as S
import qualified Data.Set as Set
import Data.Word
import System.IO.Unsafe

import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.Types as Types

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.Runner as SchedTest
import Concordium.Types.Execution
import qualified Concordium.Wasm as Wasm

import Concordium.Scheduler.DummyData

import qualified SchedulerTests.Helpers as Helpers
import SchedulerTests.TestUtils

-- The module which supports an upgrade.
testModuleSourceFile :: FilePath
testModuleSourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-cases.wasm"

-- The module we will upgrade to
targetSourceFile :: FilePath
targetSourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/upgrading-cases-target.wasm"

initialBlockState :: Helpers.PersistentBSM PV5 (HashedPersistentBlockState PV5)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 10_000_000 0]

-- | Get a 'ModuleRef' from a given V1 'Module' specified via the 'FilePath'.
{-# NOINLINE getModuleRefFromV1File #-}
getModuleRefFromV1File :: FilePath -> Types.ModuleRef
getModuleRefFromV1File f = unsafePerformIO $ do
    Wasm.getModuleRef @Wasm.V1 . Wasm.WasmModuleV . Wasm.ModuleSource <$> BS.readFile f

-- Construct a basic upgrade test case.
-- Deploy two modules, initialize an instance from the module that supports an upgrade,
-- and then do the upgrade.
-- The Word8 should be 0 for no state changes, 1 for a state change **before** the upgrade,
-- and 2 for a state change **after** the upgrade
testCase :: Bool -> Word8 -> [SchedTest.TransactionJSON]
testCase changeAmount changeState =
    [ SchedTest.TJSON
        { payload = SchedTest.DeployModule Wasm.V1 testModuleSourceFile,
          metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 1 1_000,
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        },
      SchedTest.TJSON
        { payload = SchedTest.DeployModule Wasm.V1 targetSourceFile,
          metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 2 1_000,
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        },
      SchedTest.TJSON
        { payload = SchedTest.InitContract 0 Wasm.V1 testModuleSourceFile "init_contract" "",
          metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 3 1_000,
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        },
      SchedTest.TJSON
        { payload =
            SchedTest.Update
                (if changeAmount then 123 else 0)
                (Types.ContractAddress 0 0)
                "contract.upgrade"
                upgradeParameters,
          metadata = makeDummyHeader (Helpers.accountAddressFromSeed 0) 4 10_000,
          keys = [(0, [(0, Helpers.keyPairFromSeed 0)])]
        }
    ]
  where
    -- the upgrade parameters are the module to upgrade to and a tag stating whether the state
    -- should or should not be updated.
    upgradeParameters =
        BSS.toShort $
            S.runPut $
                S.put (getModuleRefFromV1File targetSourceFile) <> S.putWord8 changeState

-- Run the upgrade tests in different scenarios. The boolean flags indicate
-- whether the amount should be changed, the balance should be changed, or the
-- state should be reloaded before inspecting it.
runUpgradeTests :: Bool -> Word8 -> Bool -> Assertion
runUpgradeTests changeAmount changeState reloadState = do
    (Helpers.SchedulerResult{..}, (params, bal, newState)) <-
        Helpers.runSchedulerTestTransactionJson
            Helpers.defaultTestConfig
            initialBlockState
            extractor
            (testCase changeAmount changeState)
    let Sch.FilteredTransactions{..} = srTransactions
    forM_ (Helpers.getResults ftAdded) $ \(_, result) -> do
        case result of
            TxSuccess{} -> return ()
            TxReject{..} -> assertFailure $ "Transaction rejected: " ++ show vrRejectReason
    assertEqual
        "No entrypoints in the upgraded contract"
        Set.empty
        (Types.instanceReceiveFuns params)
    let mi = Types.instanceModuleInterface params
    assertEqual
        "Upgrade to the new module interface"
        (getModuleRefFromV1File targetSourceFile)
        (GSWasm.miModuleRef mi)
    if changeAmount
        then assertEqual "Amount was updated" 123 bal
        else assertEqual "Amount was not updated" 0 bal
    if changeState /= 0
        then assertEqual "State was updated" 1 (BS.index newState 0) -- non-empty state serialization starts with a 1 tag.
        else assertEqual "State was not updated" (BS.singleton 0) newState -- empty state serialization just puts a 0 tag.
  where
    extractor _ ubs = do
        blockState <-
            if reloadState
                then Helpers.reloadBlockState ubs
                else return ubs
        bsoGetInstance blockState (Types.ContractAddress 0 0) >>= \case
            Nothing -> error "Missing instance."
            Just (InstanceInfoV0 _) -> error "Expected V1 instance, but got V0."
            Just (InstanceInfoV1 ii) -> do
                let Instances.InstanceStateV1 s = iiState ii
                bs <- StateV1.toByteString s
                return (iiParameters ii, iiBalance ii, bs)
tests :: Spec
tests = describe "Upgrade contract cases with persistent state" $ do
    specify "V1: Just module upgrade" $ runUpgradeTests False 0 False
    specify "V1: Module + state update before" $ runUpgradeTests False 1 False
    specify "V1: Module + state update after" $ runUpgradeTests False 2 False
    specify "V1: Module + balance update" $ runUpgradeTests True 0 False
    specify "V1: Module + state before + balance" $ runUpgradeTests True 1 False
    specify "V1: Module + state after + balance" $ runUpgradeTests True 2 False
    specify "V1: Reload: Just module upgrade" $ runUpgradeTests False 0 True
    specify "V1: Reload: Module + state update before" $ runUpgradeTests False 1 True
    specify "V1: Reload: Module + state update after" $ runUpgradeTests False 2 True
    specify "V1: Reload: Module + balance update" $ runUpgradeTests True 0 True
    specify "V1: Reload: Module + state before + balance" $ runUpgradeTests True 1 True
    specify "V1: Reload: Module + state after + balance" $ runUpgradeTests True 2 True
