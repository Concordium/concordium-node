{-# LANGUAGE TemplateHaskell #-}
module Concordium.GlobalState.Basic.BlockState.Modules
  ( Module(..),
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
    getModulesV0
  ) where

import Concordium.Crypto.SHA256
import Concordium.GlobalState.Basic.BlockState.LFMBTree (LFMBTree)
import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as LFMB
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Wasm
import Concordium.Scheduler.WasmIntegration
import Control.Monad
import Data.Coerce
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
-- module.
data Module = Module {
  -- | The instrumented module, ready to be instantiated.
  interface :: !ModuleInterface,
  -- | The raw module binary source.
  source :: !WasmModule
  } deriving (Show)

instance HashableTo Hash Module where
  getHash = coerce . miModuleRef . interface

instance Serialize Module where
  put = put . source
  get = do
    source <- get
    case processModule source of
      Nothing -> fail "Invalid module"
      Just interface -> return Module {..}

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
putInterface :: (ModuleInterface, WasmModule) -> Modules -> Maybe Modules
putInterface (iface, source) m =
  if Map.member mref (m ^. modulesMap)
  then Nothing
  else Just $ m & modulesTable .~ modulesTable'
                & modulesMap %~ Map.insert mref idx
 where mref = miModuleRef iface
       (idx, modulesTable') = LFMB.append (Module iface source) $ m ^. modulesTable

getModule :: ModuleRef -> Modules -> Maybe Module
getModule ref mods = Map.lookup ref (mods ^. modulesMap) >>=
                       flip LFMB.lookup (mods ^. modulesTable)

-- |Get an interface by module reference.
getInterface :: ModuleRef -> Modules -> Maybe ModuleInterface
getInterface ref mods = fmap interface $ getModule ref mods

-- |Get the source of a module by module reference.
getSource :: ModuleRef -> Modules -> Maybe WasmModule
getSource ref mods = fmap source $ getModule ref mods

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
    mapM_ put (_modulesTable mods)

-- |Deserialize modules in V0 format
getModulesV0 :: Get Modules
getModulesV0 = do
    mtSize <- getWord64be
    _modulesTable <- LFMB.fromList <$> replicateM (fromIntegral mtSize) get
    let _modulesMap = Map.fromList
            [(miModuleRef (interface iface), idx)
              | (idx, iface) <- LFMB.toAscPairList _modulesTable]
    return Modules{..}