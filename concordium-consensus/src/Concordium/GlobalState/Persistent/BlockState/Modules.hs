{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.GlobalState.Persistent.BlockState.Modules
  ( Module(..),
    ModuleV(..),
    Modules,
    ModuleCache,
    SupportsPersistentModules,
    getModuleInterface,
    emptyModules,
    getInterface,
    getSource,
    getModuleReference,
    putInterface,
    moduleRefList,
    makePersistentModules,
    newModuleCache,
    unsafeToModuleV,
    -- * Serialization
    putModulesV0
  ) where

import Concordium.Crypto.SHA256
import qualified Concordium.GlobalState.Basic.BlockState.Modules as TransientModules
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree')
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
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Persistent.CachedRef

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

-- |Coerce a module to V0. Will fail if the version is not 'V0'.
unsafeToModuleV0 :: Module -> ModuleV V0
unsafeToModuleV0 (ModuleV0 m) = m
unsafeToModuleV0 (ModuleV1 _) = error "Could not coerce module to V0."

-- |Coerce a module to V1. Will fail if the version is not 'V1'.
unsafeToModuleV1 :: Module -> ModuleV V1
unsafeToModuleV1 (ModuleV0 _) = error "Could not coerce module to V1."
unsafeToModuleV1 (ModuleV1 m) = m

-- |Coerce a 'Module' to a 'ModuleV' depending on the 'WasmVersion'.
unsafeToModuleV :: forall v. IsWasmVersion v => Module -> ModuleV v
unsafeToModuleV = case getWasmVersion @v of
  SV0 -> unsafeToModuleV0
  SV1 -> unsafeToModuleV1

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

instance CacheCleanup Module where
  cleanup = \case
      ModuleV0 m -> cleanupMod m
      ModuleV1 m -> cleanupMod m
    where
      cleanupMod = GSWasm.finalizeModuleInterfaceV . moduleVInterface

-- |Serialize a module in V0 format.
-- This only serializes the source.
putModuleV0 :: (MonadBlobStore m, MonadPut m) => Module -> m ()
putModuleV0 (ModuleV0 ModuleV{..}) = sPut =<< loadRef moduleVSource
putModuleV0 (ModuleV1 ModuleV{..}) = sPut =<< loadRef moduleVSource

--------------------------------------------------------------------------------

-- |The cache retaining 'Module's
type ModuleCache = FIFOCache Module

-- |Construct a new `ModuleCache` with the given size.
newModuleCache :: Int -> IO ModuleCache
newModuleCache = newCache

-- |Make sure that a monad supports the `MonadBlobStore` and `MonadCache`
-- for the modules cache.
type SupportsPersistentModules m = (MonadBlobStore m, MonadCache ModuleCache m)

-- |The collection of modules stored in a block state.
data Modules = Modules {
  -- |A tree of 'Module's indexed by 'ModuleIndex'
  -- The modules themselves are cached `HashedCachedRef` hence only a limited
  -- amount of modules may be retained in memory at the same time. 
  _modulesTable :: !(LFMBTree' ModuleIndex HashedBufferedRef (HashedCachedRef ModuleCache Module)),
  -- |A map of ModuleRef to ModuleIndex.
  _modulesMap :: !(Map ModuleRef ModuleIndex)
  }
makeLenses ''Modules

-- | The hash of the collection of modules is the hash of the tree.
instance SupportsPersistentModules m => MHashableTo m Hash Modules where
  getHashM = getHashM . _modulesTable

instance SupportsPersistentModules m => BlobStorable m Modules where
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

instance SupportsPersistentModules m => Cacheable m Modules where
  cache Modules{..} = do
    modulesTable' <- cache _modulesTable
    return Modules { _modulesTable = modulesTable', ..}

--------------------------------------------------------------------------------

-- |The empty collection of modules
emptyModules :: Modules
emptyModules = Modules LFMB.empty Map.empty

-- |Try to add interfaces to the module table. If a module with the given
-- reference exists returns @Nothing@.
putInterface :: (IsWasmVersion v, SupportsPersistentModules m)
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

getModule :: SupportsPersistentModules m => ModuleRef -> Modules -> m (Maybe Module)
getModule ref mods =
  let modIdx = Map.lookup ref (mods ^. modulesMap) in
  case modIdx of
    Nothing -> return Nothing
    Just idx -> LFMB.lookup idx (mods ^. modulesTable)

-- |Gets the 'HashedCachedRef' to a module as stored in the module table
-- to be given to instances when associating them with the interface.
-- The reason we return the reference here is to allow for sharing of the reference.
getModuleReference :: SupportsPersistentModules m => ModuleRef -> Modules -> m (Maybe (HashedCachedRef ModuleCache Module))
getModuleReference ref mods =
  let modIdx = Map.lookup ref (mods ^. modulesMap) in
  case modIdx of
    Nothing -> return Nothing
    Just idx -> LFMB.lookupRef idx (mods ^. modulesTable)

-- |Get an interface by module reference.
getInterface :: SupportsPersistentModules m
             => ModuleRef
             -> Modules
             -> m (Maybe GSWasm.ModuleInterface)
getInterface ref mods = fmap getModuleInterface <$> getModule ref mods

-- |Get the source of a module by module reference.
getSource :: SupportsPersistentModules m => ModuleRef -> Modules -> m (Maybe WasmModule)
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

makePersistentModules :: SupportsPersistentModules m
                       => TransientModules.Modules
                       -> m Modules
makePersistentModules mods = do
  let modlist = TransientModules.moduleList mods
  _modulesTable <- mapM (storePersistentModule . snd) modlist >>=
                     LFMB.fromAscList
  return Modules{ _modulesMap = TransientModules._modulesMap mods, ..}

--------------------------------------------------------------------------------

-- |Serialize modules in V0 format.
putModulesV0 :: (SupportsPersistentModules m, MonadPut m) => Modules -> m ()
putModulesV0 mods = do
    let mt = mods ^. modulesTable
    liftPut $ putWord64be $ LFMB.size mt
    LFMB.mmap_ putModuleV0 mt
