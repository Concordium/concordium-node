{-# LANGUAGE TemplateHaskell #-}
module Concordium.GlobalState.Persistent.BlockState.Modules
  ( Module(..),
    Modules,
    emptyModules,
    getInterface,
    getSource,
    getModuleReference,
    putInterface,
    moduleRefList,
    makePersistentModules,
    -- * Serialization
    putModulesV0
  ) where

import Concordium.Crypto.SHA256
import qualified Concordium.GlobalState.Basic.BlockState.Modules as TransientModules
import Concordium.GlobalState.Wasm
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree)
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMB
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization.Put
import Concordium.Wasm
import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Word
import Lens.Micro.Platform

-- |Index of the module in the module table. Reflects when the module was added
-- to the table.
type ModuleIndex = Word64

--------------------------------------------------------------------------------

-- |A module contains both the module interface and a plain reference to where
-- the raw source code of the module is stored in the blobstore.
data Module = Module {
  -- | The instrumented module, ready to be instantiated.
  interface :: !ModuleInterface,
  -- | A plain reference to the raw module binary source.
  source :: !(BlobRef WasmModule)
  }

instance HashableTo Hash Module where
  getHash = coerce . miModuleRef . interface
instance MonadBlobStore m => MHashableTo m Hash Module

-- |This serialization is used for storing the module in the BlobStore.
-- It should not be used for other purposes.
instance Serialize Module where
  get = do
    interface <- get
    source <- get
    return Module{..}
  put Module{..} = do
    put interface
    put source

instance MonadBlobStore m => BlobStorable m Module where
instance MonadBlobStore m => Cacheable m Module where

-- |Serialize a module in V0 format.
-- This only serializes the source.
putModuleV0 :: (MonadBlobStore m, MonadPut m) => Module -> m ()
putModuleV0 = sPut <=< loadRef . source

--------------------------------------------------------------------------------

-- |The collection of modules stored in a block state.
data Modules = Modules {
  -- |A tree of 'Module's indexed by 'ModuleIndex'
  _modulesTable :: !(LFMBTree ModuleIndex HashedBufferedRef Module),
  -- |A map of ModuleRef to ModuleIndex.
  _modulesMap :: !(Map ModuleRef ModuleIndex)
  }
makeLenses ''Modules

-- | The hash of the collection of modules is the hash of the tree.
instance MonadBlobStore m => MHashableTo m Hash Modules where
  getHashM = getHashM . _modulesTable

instance MonadBlobStore m => BlobStorable m Modules where
  load = do
    table <- load
    return $ do
      _modulesTable <- table
      _modulesMap <- foldl' (\m (idx, aModule) ->
                             Map.insert (miModuleRef $ interface aModule) idx m)
                               Map.empty <$> LFMB.toAscPairList _modulesTable
      return Modules{..}
  store = fmap fst . storeUpdate
  storeUpdate m@Modules{..} = do
    (pModulesTable, _modulesTable') <- storeUpdate _modulesTable
    return (pModulesTable, m { _modulesTable = _modulesTable' })

instance MonadBlobStore m => Cacheable m Modules where
  cache Modules{..} = do
    modulesTable' <- cache _modulesTable
    return Modules { _modulesTable = modulesTable', ..}

--------------------------------------------------------------------------------

-- |The empty collection of modules
emptyModules :: Modules
emptyModules = Modules LFMB.empty Map.empty

-- |Try to add interfaces to the module table. If a module with the given
-- reference exists returns @Nothing@.
putInterface :: MonadBlobStore m
             => (ModuleInterface, WasmModule)
             -> Modules
             -> m (Maybe Modules)
putInterface (modul, src) m =
  if Map.member mref (m ^. modulesMap)
  then return Nothing
  else do
    src' <- storeRef src
    (idx, modulesTable') <- LFMB.append (Module modul src') $ m ^. modulesTable
    return $ Just $ m & modulesTable .~ modulesTable'
                      & modulesMap %~ Map.insert mref idx
 where mref = miModuleRef modul

getModule :: MonadBlobStore m => ModuleRef -> Modules -> m (Maybe Module)
getModule ref mods =
  let modIdx = Map.lookup ref (mods ^. modulesMap) in
  case modIdx of
    Nothing -> return Nothing
    Just idx -> LFMB.lookup idx (mods ^. modulesTable)

-- |Gets the buffered reference to a module as stored in the module table
-- to be given to instances when associating them with the interface.
getModuleReference :: MonadBlobStore m => ModuleRef -> Modules -> m (Maybe (BufferedRef Module))
getModuleReference ref mods =
  let modIdx = Map.lookup ref (mods ^. modulesMap) in
  case modIdx of
    Nothing -> return Nothing
    Just idx -> fmap bufferedReference <$> LFMB.lookupRef idx (mods ^. modulesTable)

-- |Get an interface by module reference.
getInterface :: MonadBlobStore m
             => ModuleRef
             -> Modules
             -> m (Maybe ModuleInterface)
getInterface ref mods = fmap interface <$> getModule ref mods

-- |Get the source of a module by module reference.
getSource :: MonadBlobStore m => ModuleRef -> Modules -> m (Maybe WasmModule)
getSource ref mods = do
  m <- getModule ref mods
  case m of
    Nothing -> return Nothing
    Just modul -> Just <$> loadRef (source modul)

-- |Get the list of all currently deployed modules.
-- The order of the list is not specified.
moduleRefList :: Modules -> [ModuleRef]
moduleRefList mods = Map.keys (mods ^. modulesMap)

--------------------------------------------------------------------------------

storePersistentModule :: MonadBlobStore m
                      => TransientModules.Module
                      -> m Module
storePersistentModule TransientModules.Module{..} = do
  source' <- storeRef source
  return Module{ source = source', ..}

makePersistentModules :: MonadBlobStore m
                       => TransientModules.Modules
                       -> m Modules
makePersistentModules mods = do
  let modlist = TransientModules.moduleList mods
  _modulesTable <- mapM (storePersistentModule . snd) modlist >>=
                     LFMB.fromAscList
  return Modules{ _modulesMap = TransientModules._modulesMap mods, ..}

--------------------------------------------------------------------------------

-- |Serialize modules in V0 format.
putModulesV0 :: (MonadBlobStore m, MonadPut m) => Modules -> m ()
putModulesV0 mods = do
    let mt = mods ^. modulesTable
    liftPut $ putWord64be $ LFMB.size mt
    LFMB.mmap_ putModuleV0 mt
