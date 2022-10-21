{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Concordium.GlobalState.Basic.BlockState.Modules
  ( Module(..),
    ModuleV(..),
    Modules,
    emptyModules,
    putInterface,
    getInterface,
    getSource,
    moduleRefList,
    moduleList,
    _modulesMap,
    -- * Serialization
    putModulesV0,
    getModulesV0,
  ) where

import Concordium.Crypto.SHA256
import Concordium.GlobalState.Basic.BlockState.LFMBTree (LFMBTree)
import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as LFMB
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Wasm
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.WasmIntegration as V0
import qualified Concordium.Scheduler.WasmIntegration.V1 as V1
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Word
import Lens.Micro.Platform

-- |Index of the module in the module table. Reflects when the module was added
-- to the table.
type ModuleIndex = Word64

--------------------------------------------------------------------------------

-- |A module contains both the module interface and the raw source code of the
-- module. The module is parameterized by the wasm version, which determines the shape
-- of the module interface.
data ModuleV v = ModuleV {
  -- | The instrumented module, ready to be instantiated.
  moduleVInterface :: !(GSWasm.ModuleInterfaceV v),
  -- | The raw module binary source.
  moduleVSource :: !(WasmModuleV v)
  } deriving (Show)

-- |A module, either of version 0 or 1. This is only used when storing a module
-- independently, e.g., in the module table. When a module is referenced from a
-- contract instance we use the ModuleV type directly so we may tie the version
-- of the module to the version of the instance.
data Module where
  ModuleV0 :: !(ModuleV GSWasm.V0) -> Module
  ModuleV1 :: !(ModuleV GSWasm.V1) -> Module
  deriving(Show)

instance GSWasm.HasModuleRef Module where
  {-# INLINE moduleReference #-}
  moduleReference (ModuleV0 v) = GSWasm.moduleReference . moduleVInterface $ v
  moduleReference (ModuleV1 v) = GSWasm.moduleReference . moduleVInterface $ v

instance HashableTo Hash Module where
  getHash = moduleRef . GSWasm.moduleReference

putModule :: Module -> Put
putModule (ModuleV0 ModuleV{..}) = put moduleVSource
putModule (ModuleV1 ModuleV{..}) = put moduleVSource

deserialModule :: forall pv . SProtocolVersion pv -> Get Module
deserialModule spv = do
    wasmModule <- get
    case wasmModule of
      WasmModuleV0 moduleVSource ->
        case V0.processModule moduleVSource of
          Nothing -> fail "Invalid V0 module"
          Just moduleVInterface -> return (ModuleV0 ModuleV {..})
      WasmModuleV1 moduleVSource ->
        case V1.processModule (supportsUpgradableContracts spv) moduleVSource of
          Nothing -> fail "Invalid V1 module"
          Just moduleVInterface -> return (ModuleV1 ModuleV {..})

--------------------------------------------------------------------------------

-- |The collection of modules stored in a block state.
data Modules = Modules {
  -- |A tree of 'Module's indexed by 'ModuleIndex'
  _modulesTable :: !(LFMBTree ModuleIndex Module),
  -- |A map of ModuleRef to ModuleIndex.
  _modulesMap :: !(Map ModuleRef ModuleIndex)
  } deriving (Show)
makeLenses ''Modules

-- | The hash of the collection of modules is the hash of the tree.
instance HashableTo Hash Modules where
  getHash = getHash . _modulesTable
instance Monad m => MHashableTo m Hash Modules where

--------------------------------------------------------------------------------

-- |The empty collection of modules
emptyModules :: Modules
emptyModules = Modules LFMB.empty Map.empty

-- |Try to add interfaces to the module table. If a module with the given
-- reference exists returns @Nothing@.
putInterface :: forall v . IsWasmVersion v => (GSWasm.ModuleInterfaceV v, WasmModuleV v) -> Modules -> Maybe Modules
putInterface (moduleVInterface, moduleVSource) m =
  if Map.member mref (m ^. modulesMap)
  then Nothing
  else Just $ m & modulesTable .~ modulesTable'
                & modulesMap %~ Map.insert mref idx
 where mref = GSWasm.moduleReference moduleVInterface
       (idx, modulesTable') = LFMB.append newModule $ m ^. modulesTable

       newModule = case getWasmVersion @v of
                     SV0 -> ModuleV0 ModuleV{..}
                     SV1 -> ModuleV1 ModuleV{..}


getModule :: ModuleRef -> Modules -> Maybe Module
getModule ref mods = Map.lookup ref (mods ^. modulesMap) >>=
                       flip LFMB.lookup (mods ^. modulesTable)

-- |Get an interface by module reference.
getInterface :: ModuleRef -> Modules -> Maybe GSWasm.BasicModuleInterface
getInterface ref mods = fromModule <$> getModule ref mods
    where
      fromModule (ModuleV0 v) = GSWasm.ModuleInterfaceV0 (moduleVInterface v)
      fromModule (ModuleV1 v) = GSWasm.ModuleInterfaceV1 (moduleVInterface v)


-- |Get the source of a module by module reference.
getSource :: ModuleRef -> Modules -> Maybe WasmModule
getSource ref mods = mp <$> getModule ref mods
  where mp (ModuleV0 ModuleV{..}) = WasmModuleV0 moduleVSource
        mp (ModuleV1 ModuleV{..}) = WasmModuleV1 moduleVSource

moduleRefList :: Modules -> [ModuleRef]
moduleRefList mods = Map.keys (mods ^. modulesMap)

-- |Get the list of all currently deployed modules.
-- The order of the list is not specified.
moduleList :: Modules -> [(ModuleIndex, Module)]
moduleList mods = LFMB.toAscPairList (_modulesTable mods)

--------------------------------------------------------------------------------

-- |Serialize modules in V0 format
putModulesV0 :: Putter Modules
putModulesV0 mods = do
    putWord64be (LFMB.size (_modulesTable mods))
    mapM_ putModule (_modulesTable mods)

-- |Deserialize modules in V0 format
getModulesV0 :: SProtocolVersion pv -> Get Modules
getModulesV0 spv = do
    mtSize <- getWord64be
    _modulesTable <- LFMB.fromList <$> replicateM (fromIntegral mtSize) (deserialModule spv)
    let _modulesMap = Map.fromList
            [(GSWasm.moduleReference iface, idx)
              | (idx, iface) <- LFMB.toAscPairList _modulesTable]
    return Modules{..}
