{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Concordium.GlobalState.Persistent.BlockState.Modules
  ( Module(..),
    ModuleV(..),
    Modules,
    getModuleInterface,
    emptyModules,
    getInterface,
    getSource,
    getModuleReference,
    unsafeGetModuleReferenceV0,
    unsafeGetModuleReferenceV1,
    putInterface,
    moduleRefList,
    makePersistentModules,
    -- * Serialization
    putModulesV0,
    migrateModules
  ) where

import Concordium.Crypto.SHA256
import qualified Concordium.GlobalState.Basic.BlockState.Modules as TransientModules
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree)
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMB
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization.Put
import Concordium.Wasm
import Data.Coerce
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Word
import Lens.Micro.Platform
import Control.Monad.Trans

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
  -- | A plain reference to the raw module binary source. This is generally not needed by consensus, so
  -- it is almost always simply kept on disk.
  moduleVSource :: !(BlobRef (WasmModuleV v))
  }
    deriving(Show)

-- |Helper to convert from an interface to a module.
toModule :: forall v . IsWasmVersion v => GSWasm.ModuleInterfaceV v -> BlobRef (WasmModuleV v) -> Module
toModule moduleVInterface moduleVSource =
  case getWasmVersion @v of
    SV0 -> ModuleV0 ModuleV{..}
    SV1 -> ModuleV1 ModuleV{..}

-- |A module, either of version 0 or 1. This is only used when storing a module
-- independently, e.g., in the module table. When a module is referenced from a
-- contract instance we use the ModuleV type directly so we may tie the version
-- of the module to the version of the instance.
data Module where
  ModuleV0 :: ModuleV GSWasm.V0 -> Module
  ModuleV1 :: ModuleV GSWasm.V1 -> Module
  deriving (Show)

getModuleInterface :: Module -> GSWasm.ModuleInterface
getModuleInterface (ModuleV0 m) = GSWasm.ModuleInterfaceV0 (moduleVInterface m)
getModuleInterface (ModuleV1 m) = GSWasm.ModuleInterfaceV1 (moduleVInterface m)


instance GSWasm.HasModuleRef Module where
  moduleReference (ModuleV0 m) = GSWasm.moduleReference (moduleVInterface m)
  moduleReference (ModuleV1 m) = GSWasm.moduleReference (moduleVInterface m)

-- The module reference already takes versioning into account, so this instance is reasonable.
instance HashableTo Hash Module where
  getHash = coerce . GSWasm.moduleReference

instance Monad m => MHashableTo m Hash Module

-- |This serialization is used for storing the module in the BlobStore.
-- It should not be used for other purposes.
instance Serialize Module where
  get = do
    -- interface is versioned
    get >>= \case
      GSWasm.ModuleInterfaceV0 moduleVInterface -> do
        moduleVSource <- get
        return $! toModule moduleVInterface moduleVSource
      GSWasm.ModuleInterfaceV1 moduleVInterface -> do
        moduleVSource <- get
        return $! toModule moduleVInterface moduleVSource
  put m  = do
    put (getModuleInterface m)
    case m of
      ModuleV0 ModuleV{..} -> put moduleVSource
      ModuleV1 ModuleV{..} -> put moduleVSource

-- |This serialization is used for storing the module in the BlobStore.
-- It should not be used for other purposes.
instance Serialize (ModuleV GSWasm.V0) where
  get = do
    -- interface is versioned
    moduleVInterface <- get
    moduleVSource <- get
    return $! ModuleV {..}
  put ModuleV{..} = put moduleVInterface <> put moduleVSource

-- |This serialization is used for storing the module in the BlobStore.
-- It should not be used for other purposes.
instance Serialize (ModuleV GSWasm.V1) where
  get = do
    -- interface is versioned
    moduleVInterface <- get
    moduleVSource <- get
    return $! ModuleV {..}
  put ModuleV{..} = put moduleVInterface <> put moduleVSource


instance MonadBlobStore m => BlobStorable m (ModuleV GSWasm.V0) where
instance MonadBlobStore m => BlobStorable m (ModuleV GSWasm.V1) where
instance MonadBlobStore m => BlobStorable m Module where
instance MonadBlobStore m => Cacheable m Module where

-- |Serialize a module in V0 format.
-- This only serializes the source.
putModuleV0 :: (MonadBlobStore m, MonadPut m) => Module -> m ()
putModuleV0 (ModuleV0 ModuleV{..}) = sPut =<< loadRef moduleVSource
putModuleV0 (ModuleV1 ModuleV{..}) = sPut =<< loadRef moduleVSource

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
                             Map.insert (GSWasm.moduleReference aModule) idx m)
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
putInterface :: (IsWasmVersion v, MonadBlobStore m)
             => (GSWasm.ModuleInterfaceV v, WasmModuleV v)
             -> Modules
             -> m (Maybe Modules)
putInterface (modul, src) m =
  if Map.member mref (m ^. modulesMap)
  then return Nothing
  else do
    src' <- storeRef src
    (idx, modulesTable') <- LFMB.append (toModule modul src') $ m ^. modulesTable
    return $ Just $ m & modulesTable .~ modulesTable'
                      & modulesMap %~ Map.insert mref idx
 where mref = GSWasm.moduleReference modul

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

-- |Gets the buffered reference to a module as stored in the module table assuming it is version 0.
unsafeGetModuleReferenceV0 :: MonadBlobStore m => ModuleRef -> Modules -> m (Maybe (BufferedRef (ModuleV GSWasm.V0)))
unsafeGetModuleReferenceV0 ref mods = fmap (unsafeCoerceBufferedRef extract) <$> getModuleReference ref mods 
    where extract (ModuleV0 m) = m
          extract _ = error "Precondition violation. Expected module version 0, got 1."
-- |Gets the buffered reference to a module as stored in the module table assuming it is version 1.
unsafeGetModuleReferenceV1 :: MonadBlobStore m => ModuleRef -> Modules -> m (Maybe (BufferedRef (ModuleV GSWasm.V1)))
unsafeGetModuleReferenceV1 ref mods = fmap (unsafeCoerceBufferedRef extract) <$> getModuleReference ref mods 
    where extract (ModuleV1 m) = m
          extract _ = error "Precondition violation. Expected module version 1, got 0."


-- |Get an interface by module reference.
getInterface :: MonadBlobStore m
             => ModuleRef
             -> Modules
             -> m (Maybe GSWasm.ModuleInterface)
getInterface ref mods = fmap getModuleInterface <$> getModule ref mods

-- |Get the source of a module by module reference.
getSource :: MonadBlobStore m => ModuleRef -> Modules -> m (Maybe WasmModule)
getSource ref mods = do
  m <- getModule ref mods
  case m of
    Nothing -> return Nothing
    Just (ModuleV0 ModuleV{..}) -> Just . WasmModuleV0 <$> loadRef moduleVSource
    Just (ModuleV1 ModuleV{..}) -> Just . WasmModuleV1 <$> loadRef moduleVSource

-- |Get the list of all currently deployed modules.
-- The order of the list is not specified.
moduleRefList :: Modules -> [ModuleRef]
moduleRefList mods = Map.keys (mods ^. modulesMap)

--------------------------------------------------------------------------------

storePersistentModule :: MonadBlobStore m
                      => TransientModules.Module
                      -> m Module
storePersistentModule (TransientModules.ModuleV0 TransientModules.ModuleV{..}) = do
  moduleVSource' <- storeRef moduleVSource
  return (ModuleV0 (ModuleV { moduleVSource = moduleVSource', ..}))
storePersistentModule (TransientModules.ModuleV1 TransientModules.ModuleV{..}) = do
  moduleVSource' <- storeRef moduleVSource
  return (ModuleV1 (ModuleV { moduleVSource = moduleVSource', ..}))

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

migrateModules :: SupportMigration m t => Modules -> t m Modules
migrateModules mods = do
    newModulesTable <- LFMB.migrateLFMBTree migrateModule (_modulesTable mods)
    return
        Modules
            { _modulesMap = _modulesMap mods
            , _modulesTable = newModulesTable
            }

migrateModule :: SupportMigration m t => HashedBufferedRef Module -> t m (HashedBufferedRef Module)
migrateModule mdl = do
    newModule <- lift (refLoad mdl)
    ref <- refMake newModule
    (ret, _) <- refFlush ref
    return ret
