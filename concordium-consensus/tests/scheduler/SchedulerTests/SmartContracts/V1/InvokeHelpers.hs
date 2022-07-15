{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-| A helper module that defines some scaffolding for running V1 contract tests via invoke.
-}
module SchedulerTests.SmartContracts.V1.InvokeHelpers where

import Test.HUnit(assertFailure)

import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as OrdMap
import qualified Data.Set as Set

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Crypto.SHA256 as Hash

import Concordium.Types.SeedState (initialSeedState)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.Wasm
import qualified Concordium.Scheduler.WasmIntegration as WasmV0
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import qualified Concordium.GlobalState.Wasm as GSWasm

import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import Concordium.GlobalState.DummyData

import SchedulerTests.TestUtils

type ContextM = PersistentBlockStateMonad PV4 BlobStore BlobStoreM

-- empty state, no accounts, no modules, no instances
initialBlockState :: ContextM (HashedPersistentBlockState PV4)
initialBlockState = initialPersistentState
                    (initialSeedState (Hash.hash "") 1000)
                    dummyCryptographicParameters
                    [mkAccount alesVK alesAccount 1000]
                    dummyIdentityProviders
                    dummyArs
                    dummyKeyCollection
                    dummyChainParameters

callerSourceFile :: FilePath
callerSourceFile = "./testdata/contracts/v1/caller.wasm"

emptyContractSourceFile :: FilePath
emptyContractSourceFile = "./testdata/contracts/empty.wasm"

-- |Deploy a V1 module in the given state. The source file should be a raw Wasm file.
-- If the module is invalid this will raise an exception.
deployModuleV1 :: FilePath -- ^Source file.
              -> PersistentBlockState PV4 -- ^State to add the module to.
              -> ContextM ((GSWasm.ModuleInterfaceV V1, WasmModuleV V1), PersistentBlockState PV4)
deployModuleV1 sourceFile bs = do
  ws <- liftIO $ BS.readFile sourceFile
  let wm = WasmModuleV (ModuleSource ws)
  case WasmV1.processModule wm of
    Nothing -> liftIO $ assertFailure "Invalid module."
    Just miv -> do
      (_, modState) <- bsoPutNewModule bs (miv, wm)
      return ((miv, wm), modState)


-- |Deploy a V0 module in the given state. The source file should be a raw Wasm file.
-- If the module is invalid this will raise an exception.
deployModuleV0 :: FilePath -- ^Source file.
              -> PersistentBlockState PV4 -- ^State to add the module to.
              -> ContextM ((GSWasm.ModuleInterfaceV V0, WasmModuleV V0), PersistentBlockState PV4)
deployModuleV0 sourceFile bs = do
  ws <- liftIO $ BS.readFile sourceFile
  let wm = WasmModuleV (ModuleSource ws)
  case WasmV0.processModule wm of
    Nothing -> liftIO $ assertFailure "Invalid module."
    Just miv -> do
      (_, modState) <- bsoPutNewModule bs (miv, wm)
      return ((miv, wm), modState)

-- |Initialize a contract from the supplied module in the given state, and return its address.
-- The state is assumed to contain the module.
initContractV1 :: Types.AccountAddress -- ^Sender address
               -> InitName -- ^Contract to initialize.
               -> Parameter -- ^Parameter to initialize with.
               -> Types.Amount -- ^Initial balance.
               -> PersistentBlockState PV4
               -> (GSWasm.ModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1)
               -> ContextM (Types.ContractAddress, PersistentBlockState PV4)
initContractV1 senderAddress initName initParam initAmount bs (miv, _) = do
  let cm = Types.ChainMetadata 0
  let initContext = InitContext{
        initOrigin = senderAddress,
        icSenderPolicies = []
        }
  let initInterpreterEnergy = 1_000_000_000
  (cbk, _) <- getCallbacks
  case WasmV1.applyInitFun cbk miv cm initContext initName initParam initAmount initInterpreterEnergy of
    Nothing -> -- out of energy
      liftIO $ assertFailure "Initialization ran out of energy."
    Just (Left failure, _) ->
      liftIO $ assertFailure $ "Initialization failed: " ++ show failure
    Just (Right WasmV1.InitSuccess{..}, _) -> do
      let receiveMethods = OrdMap.findWithDefault Set.empty initName (GSWasm.miExposedReceive miv)
      let ins = NewInstanceData{
            nidInitName = initName,
            nidEntrypoints = receiveMethods,
            nidInterface = miv,
            nidInitialState = irdNewState,
            nidInitialAmount = initAmount,
            nidOwner = senderAddress
            }
      bsoPutNewInstance bs ins

-- |Initialize a contract from the supplied module in the given state, and return its address.
-- The state is assumed to contain the module.
initContractV0 :: Types.AccountAddress -- ^Sender address
               -> InitName -- ^Contract to initialize.
               -> Parameter -- ^Parameter to initialize with.
               -> Types.Amount -- ^Initial balance.
               -> PersistentBlockState PV4
               -> (GSWasm.ModuleInterfaceV GSWasm.V0, WasmModuleV GSWasm.V0)
               -> ContextM (Types.ContractAddress, PersistentBlockState PV4)
initContractV0 senderAddress initName initParam initAmount bs (miv, _) = do
  let cm = Types.ChainMetadata 0
  let initContext = InitContext{
        initOrigin = senderAddress,
        icSenderPolicies = []
        }
  let initInterpreterEnergy = 1_000_000_000
  case WasmV0.applyInitFun miv cm initContext initName initParam initAmount initInterpreterEnergy of
    Nothing -> -- out of energy
      liftIO $ assertFailure "Initialization ran out of energy."
    Just (Left failure, _) ->
      liftIO $ assertFailure $ "Initialization failed: " ++ show failure
    Just (Right SuccessfulResultData{..}, _) -> do
      let receiveMethods = OrdMap.findWithDefault Set.empty initName (GSWasm.miExposedReceive miv)
      let ins = NewInstanceData{
            nidInitName = initName,
            nidEntrypoints = receiveMethods,
            nidInterface = miv,
            nidInitialState = newState,
            nidInitialAmount = initAmount,
            nidOwner = senderAddress
            }
      bsoPutNewInstance bs ins

