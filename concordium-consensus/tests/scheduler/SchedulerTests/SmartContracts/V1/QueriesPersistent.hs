{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |Tests that make sure that doing chain queries in smart contracts
-- correctly handles state changes.
module SchedulerTests.SmartContracts.V1.QueriesPersistent (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec

import Control.Monad.RWS.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize as S
import Data.Word

import qualified Concordium.Scheduler.Types as Types

import Concordium.GlobalState.BlockState
import qualified Concordium.Scheduler.Runner as SchedTest
import Concordium.Types.Execution
import qualified Concordium.Wasm as Wasm
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import qualified Concordium.GlobalState.Persistent.Instances as Instances

import Concordium.Crypto.DummyData
import Concordium.Scheduler.DummyData
import Concordium.Types.DummyData

import SchedulerTests.SmartContracts.V1.PersistentStateHelpers

-- The module which supports transfers and state updates.
testModuleSourceFile :: FilePath
testModuleSourceFile = "./testdata/contracts/v1/queries-cases.wasm"

-- Construct a basic upgrade test case.
-- Deploy two modules, initialize an instance from the module that supports an upgrade,
-- and then do the upgrade.
-- The Word8 should be 0 for no state changes, 1 for a state change **before** the upgrade,
-- and 2 for a state change **after** the upgrade
testCase :: Word8 -> [SchedTest.TransactionJSON]
testCase changeState =
    [ SchedTest.TJSON
        { payload = SchedTest.DeployModule Wasm.V1 testModuleSourceFile
        , metadata = makeDummyHeader alesAccount 1 1_000
        , keys = [(0, [(0, alesKP)])]
        }
    , SchedTest.TJSON
        { payload = SchedTest.InitContract 1_000 Wasm.V1 testModuleSourceFile "init_contract" ""
        , metadata = makeDummyHeader alesAccount 2 1_000
        , keys = [(0, [(0, alesKP)])]
        }
    , SchedTest.TJSON
        { payload = SchedTest.Update 0 (Types.ContractAddress 0 0) "contract.query_account" upgradeParameters
        , metadata = makeDummyHeader alesAccount 3 10_000
        , keys = [(0, [(0, alesKP)])]
        }
    ]
  where
    -- the upgrade parameters are the address to send to, the amount, and the tag stating whether the state should or should not be updated
    upgradeParameters = BSS.toShort (S.runPut (S.put alesAccount <> S.putWord8 changeState))

-- Run the transfer tests in different scenarios. The Word8 indicates how the state should be changed.
-- 0 for no change, 1 for change before transfer, 2 for change after transfer.
-- The boolean indicates whether to reload the state before querying the final value.
runQueryTests :: Word8 -> Bool -> Assertion
runQueryTests changeState reloadState = do
    (outcomes, (bal, newState)) <- runTest (testCase changeState) reloadState $ \ubs ->
        bsoGetInstance ubs (Types.ContractAddress 0 0) >>= \case
            Nothing -> error "Missing instance."
            Just (InstanceInfoV0 _) -> error "Expected V1 instance, but got V0."
            Just (InstanceInfoV1 ii) -> do
              let Instances.InstanceStateV1 s = iiState ii
              bs <- StateV1.toByteString s
              return (iiBalance ii, bs)
    forM_ outcomes $ \(_, summary) -> do
        case tsResult summary of
            TxSuccess{} -> return ()
            TxReject{..} -> assertFailure $ "Transaction rejected: " ++ show vrRejectReason
    assertEqual "Amount was not" 1_000 bal
    if changeState /= 0 then
      assertEqual "State was updated" 1 (BS.index newState 0) -- non-empty state serialization starts with a 1 tag.
    else
      assertEqual "State was not updated" (BS.singleton 0) newState  -- empty state serialization just puts a 0 tag.

tests :: Spec
tests = describe "Upgrade contract cases with persistent state" $ do
    specify "V1: Just query" $ runQueryTests 0 False
    specify "V1: Query + state update before" $ runQueryTests 1 False
    specify "V1: Query + state update after" $ runQueryTests 2 False
    specify "V1: Reload: Just query" $ runQueryTests 0 True
    specify "V1: Reload: Query + state update before" $ runQueryTests 1 True
    specify "V1: Reload: Query + state update after" $ runQueryTests 2 True
