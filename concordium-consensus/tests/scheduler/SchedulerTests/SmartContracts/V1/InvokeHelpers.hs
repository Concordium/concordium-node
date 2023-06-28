{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | A helper module that defines some scaffolding for running V1 contract tests via invoke.
module SchedulerTests.SmartContracts.V1.InvokeHelpers where

import Test.HUnit (assertFailure)

import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as OrdMap
import qualified Data.Set as Set

import qualified Concordium.Scheduler.Types as Types

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.GlobalState.Persistent.BlockState.Modules (PersistentInstrumentedModuleV)
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.WasmIntegration as WasmV0
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import Concordium.Wasm

import qualified SchedulerTests.Helpers as Helpers
import SchedulerTests.TestUtils

type PersistentModuleInterfaceV v = GSWasm.ModuleInterfaceA (PersistentInstrumentedModuleV v)

-- |Deploy a V1 module in the given state. The source file should be a raw Wasm file.
-- If the module is invalid this will raise an exception.
deployModuleV1 ::
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    -- |Source file.
    FilePath ->
    -- |State to add the module to.
    BS.PersistentBlockState pv ->
    Helpers.PersistentBSM pv ((PersistentModuleInterfaceV V1, WasmModuleV V1), BS.PersistentBlockState pv)
deployModuleV1 spv sourceFile bs = do
    ws <- liftIO $ BS.readFile sourceFile
    let wm = WasmModuleV (ModuleSource ws)
    case WasmV1.processModule spv wm of
        Nothing -> liftIO $ assertFailure "Invalid module."
        Just miv -> do
            (_, modState) <- BS.bsoPutNewModule bs (miv, wm)
            BS.bsoGetModule modState (GSWasm.miModuleRef miv) >>= \case
                Just (GSWasm.ModuleInterfaceV1 miv') -> return ((miv', wm), modState)
                _ -> liftIO $ assertFailure "bsoGetModule failed to return put module."

-- |Deploy a V0 module in the given state. The source file should be a raw Wasm file.
-- If the module is invalid this will raise an exception.
deployModuleV0 ::
    -- |Source file.
    FilePath ->
    -- |State to add the module to.
    BS.PersistentBlockState PV4 ->
    Helpers.PersistentBSM PV4 ((PersistentModuleInterfaceV V0, WasmModuleV V0), BS.PersistentBlockState PV4)
deployModuleV0 sourceFile bs = do
    ws <- liftIO $ BS.readFile sourceFile
    let wm = WasmModuleV (ModuleSource ws)
    case WasmV0.processModule wm of
        Nothing -> liftIO $ assertFailure "Invalid module."
        Just miv -> do
            (_, modState) <- BS.bsoPutNewModule bs (miv, wm)
            BS.bsoGetModule modState (GSWasm.miModuleRef miv) >>= \case
                Just (GSWasm.ModuleInterfaceV0 miv') -> return ((miv', wm), modState)
                _ -> liftIO $ assertFailure "bsoGetModule failed to return put module."

-- |Initialize a contract from the supplied module in the given state, and return its address.
-- The state is assumed to contain the module.
initContractV1 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    -- |Sender address
    Types.AccountAddress ->
    -- |Contract to initialize.
    InitName ->
    -- |Parameter to initialize with.
    Parameter ->
    -- |Initial balance.
    Types.Amount ->
    BS.PersistentBlockState pv ->
    (PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1) ->
    Helpers.PersistentBSM pv (Types.ContractAddress, BS.PersistentBlockState pv)
initContractV1 senderAddress initName initParam initAmount bs (miv, _) = do
    let cm = Types.ChainMetadata 0
    let initContext =
            InitContext
                { initOrigin = senderAddress,
                  icSenderPolicies = []
                }
    let initInterpreterEnergy = 1_000_000_000
    (cbk, _) <- Blob.getCallbacks
    artifact <- BS.getModuleArtifact (GSWasm.miModule miv)
    case WasmV1.applyInitFun cbk artifact cm initContext initName initParam False initAmount initInterpreterEnergy of
        Nothing ->
            -- out of energy
            liftIO $ assertFailure "Initialization ran out of energy."
        Just (Left failure, _) ->
            liftIO $ assertFailure $ "Initialization failed: " ++ show failure
        Just (Right WasmV1.InitSuccess{..}, _) -> do
            let receiveMethods = OrdMap.findWithDefault Set.empty initName (GSWasm.miExposedReceive miv)
            let ins =
                    BS.NewInstanceData
                        { nidInitName = initName,
                          nidEntrypoints = receiveMethods,
                          nidInterface = miv,
                          nidInitialState = irdNewState,
                          nidInitialAmount = initAmount,
                          nidOwner = senderAddress
                        }
            BS.bsoPutNewInstance bs ins

-- |Initialize a contract from the supplied module in the given state, and return its address.
-- The state is assumed to contain the module.
initContractV0 ::
    -- |Sender address
    Types.AccountAddress ->
    -- |Contract to initialize.
    InitName ->
    -- |Parameter to initialize with.
    Parameter ->
    -- |Initial balance.
    Types.Amount ->
    BS.PersistentBlockState PV4 ->
    (PersistentModuleInterfaceV GSWasm.V0, WasmModuleV GSWasm.V0) ->
    Helpers.PersistentBSM PV4 (Types.ContractAddress, BS.PersistentBlockState PV4)
initContractV0 senderAddress initName initParam initAmount bs (miv, _) = do
    let cm = Types.ChainMetadata 0
    let initContext =
            InitContext
                { initOrigin = senderAddress,
                  icSenderPolicies = []
                }
    let initInterpreterEnergy = 1_000_000_000
    artifact <- BS.getModuleArtifact (GSWasm.miModule miv)
    case WasmV0.applyInitFun artifact cm initContext initName initParam False initAmount initInterpreterEnergy of
        Nothing ->
            -- out of energy
            liftIO $ assertFailure "Initialization ran out of energy."
        Just (Left failure, _) ->
            liftIO $ assertFailure $ "Initialization failed: " ++ show failure
        Just (Right SuccessfulResultData{..}, _) -> do
            let receiveMethods = OrdMap.findWithDefault Set.empty initName (GSWasm.miExposedReceive miv)
            let ins =
                    BS.NewInstanceData
                        { nidInitName = initName,
                          nidEntrypoints = receiveMethods,
                          nidInterface = miv,
                          nidInitialState = newState,
                          nidInitialAmount = initAmount,
                          nidOwner = senderAddress
                        }
            BS.bsoPutNewInstance bs ins
