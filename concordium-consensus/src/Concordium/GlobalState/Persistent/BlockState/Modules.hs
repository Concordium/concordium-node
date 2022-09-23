{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.GlobalState.Persistent.BlockState.Modules
  ( Module(..),
    ModuleV(..),
    Modules,
    ModuleCache,
    SupportsPersistentModule,
    getModuleInterface,
    PersistentInstrumentedModuleV,
    loadInstrumentedModuleV,
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
    putModulesV0,
    migrateModules
  ) where

import Concordium.Crypto.SHA256
import qualified Concordium.GlobalState.Basic.BlockState.Modules as TransientModules
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Persistent.CachedRef
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree')
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMB
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils
import Concordium.Utils.Serialization
import Concordium.Utils.Serialization.Put
import Concordium.Wasm
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Word
import Control.Monad.IO.Class
import Control.Monad.Trans
import Lens.Micro.Platform

-- |Index of the module in the module table. Reflects when the module was added
-- to the table.
type ModuleIndex = Word64

--------------------------------------------------------------------------------

-- |An @InstrumentedModuleV v@ in the @PersistentBlockState@, where
-- @v@ is the @WasmVersion@.
data PersistentInstrumentedModuleV (v :: WasmVersion) =
  PIMVMem !(GSWasm.InstrumentedModuleV v)
  -- ^The instrumented module is retained in memory only.
  -- This is the case before finalization.
  | PIMVPtr !(BlobPtr (GSWasm.InstrumentedModuleV v))
  -- ^The instrumented module resides solely on disk, and thus the raw
  -- bytes can be read from the blob store via the @BlobPtr@.
  -- The @BlobPtr@ is used to reconstruct artifact on the Rust side, copying it as needed
  -- from the blob store.
  deriving (Show)

-- |Load a 'PersistentInstrumentedModuleV', retrieving the artifact.
-- If the artifact has been persisted to the blob store, this creates an (executable) copy of the
-- artifact on the Rust side.  The Rust artifact is reference counted, and Haskell retains a
-- reference with a 'ForeignPtr' that drops the reference when it is garbage collected.
loadInstrumentedModuleV :: (MonadBlobStore m, IsWasmVersion v) => PersistentInstrumentedModuleV v -> m (GSWasm.InstrumentedModuleV v)
loadInstrumentedModuleV (PIMVMem im) = return im
loadInstrumentedModuleV (PIMVPtr ptr) = do
  bs <- loadBlobPtr ptr
  liftIO $ GSWasm.instrumentedModuleVFromBytes bs
  

-- |A module contains both the module interface and the raw source code of the
-- module. The module is parameterized by the wasm version, which determines the shape
-- of the module interface.
data ModuleV v = ModuleV {
  -- | The instrumented module, ready to be instantiated.
  moduleVInterface :: !(GSWasm.ModuleInterfaceA (PersistentInstrumentedModuleV v)),
  -- | A plain reference to the raw module binary source. This is generally not needed by consensus, so
  -- it is almost always simply kept on disk.
  moduleVSource :: !(BlobRef (WasmModuleV v))
  }
    deriving(Show)

-- |Helper to convert from an interface to a module.
toModule :: forall v . IsWasmVersion v => GSWasm.ModuleInterfaceV v -> BlobRef (WasmModuleV v) -> Module
toModule mvi moduleVSource =
  case getWasmVersion @v of
    SV0 -> ModuleV0 ModuleV{..}
    SV1 -> ModuleV1 ModuleV{..}
  where
    moduleVInterface = PIMVMem <$> mvi

-- |A module, either of version 0 or 1. This is only used when storing a module
-- independently, e.g., in the module table. When a module is referenced from a
-- contract instance we use the ModuleV type directly so we may tie the version
-- of the module to the version of the instance.
data Module where
  ModuleV0 :: !(ModuleV GSWasm.V0) -> Module
  ModuleV1 :: !(ModuleV GSWasm.V1) -> Module
  deriving (Show)

getModuleInterface :: Module -> GSWasm.ModuleInterface PersistentInstrumentedModuleV
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
-- This results in an error if the module is not of the desired version.
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
instance MonadBlobStore m => Cacheable m Module where

-- |This instance is based on and should be compatible with the 'Serialize' instance
-- for 'BasicModuleInterface'.
instance MonadBlobStore m => DirectBlobStorable m Module where
  loadDirect br = do
    bs <- loadRaw br
    let getModule = do
          -- Offset of the start of the module
          startOffset <- fromIntegral <$> bytesRead
          -- Header
          miModuleRef <- get
          miExposedInit <- getSafeSetOf get
          miExposedReceive <- getSafeMapOf get (getSafeSetOf get)
          -- Artifact is serialized as @InstrumentedModule v@.
          artVersion <- get
          artLen <- getWord32be
          -- Offset of the start of the artifact
          artOffset <- fromIntegral <$> bytesRead
          -- Skip the actual body of the artifact; we deserialize as a 'BlobPtr' instead.
          skip (fromIntegral artLen)
          -- Footer
          miModuleSize <- getWord64be
          let miModule :: PersistentInstrumentedModuleV v
              miModule = PIMVPtr BlobPtr {
                  theBlobPtr =
                    -- Start of the blob ref
                    theBlobRef br
                    -- Add the size of the length field for the blob ref
                    + 8
                    -- Add the offset of the artifact
                    + artOffset
                    -- Subtract the starting offset
                    - startOffset,
                  blobPtrLen = fromIntegral artLen
                }
              moduleVInterface :: GSWasm.ModuleInterfaceA (PersistentInstrumentedModuleV v)
              moduleVInterface = GSWasm.ModuleInterface{..}
          case artVersion of
            V0 -> do
              moduleVSource <- get
              return $! ModuleV0 (ModuleV{..})
            V1 -> do
              moduleVSource <- get
              return $! ModuleV1 (ModuleV{..})
    case runGet getModule bs of
        Left e -> error (e ++ " :: " ++ show bs)
        Right !mv -> return mv

  storeUpdateDirect mdl = do
      case mdl of
        ModuleV0 mv0 -> sudV SV0 mv0
        ModuleV1 mv1 -> sudV SV1 mv1
    where
      -- NB: If this is updated the @storeV@ function in 'migrateModules' below
      -- must also be updated if any changes in the representation are made.
      sudV :: SWasmVersion v -> ModuleV v -> m (BlobRef Module, Module)
      sudV ver ModuleV{moduleVInterface = GSWasm.ModuleInterface{..}, ..} = do
          !instrumentedModuleBytes <- case miModule of
            PIMVMem instrModule -> return $ case ver of
                SV0 -> encode instrModule
                SV1 -> encode instrModule
            PIMVPtr ptr -> do
              artifact <- loadBlobPtr ptr
              return $ runPut $ do
                -- This must match the serialization of InstrumentedModuleV
                put (demoteWasmVersion ver)
                putWord32be (fromIntegral (BS.length artifact))
                putByteString artifact
          let headerBytes = runPut $ do
                put miModuleRef
                putSafeSetOf put miExposedInit
                putSafeMapOf put (putSafeSetOf put) miExposedReceive
              footerBytes = runPut $ do
                putWord64be miModuleSize
                put moduleVSource
              !headerLen = fromIntegral $ BS.length headerBytes
              !imLen = fromIntegral $ BS.length instrumentedModuleBytes
          br <- storeRaw (headerBytes <> instrumentedModuleBytes <> footerBytes)
          let !miModule' = PIMVPtr BlobPtr {
                  -- Pointer is blob ref + 8 bytes (length of blob) + header length +
                  -- 4 bytes for version + 4 bytes for length of instrumented module
                  theBlobPtr = theBlobRef br + 8 + headerLen + 8,
                  -- Length is the length of the serialized instrumented module -
                  -- 4 bytes for version - 4 bytes for length
                  blobPtrLen =  imLen - 8
                }
          let mv' = ModuleV{moduleVInterface = GSWasm.ModuleInterface{miModule = miModule', ..}, ..}
          return $!! (br, mkModule ver mv')
      mkModule :: SWasmVersion v -> ModuleV v -> Module
      mkModule SV0 = ModuleV0
      mkModule SV1 = ModuleV1

-- |Serialize a module in V0 format.
-- This only serializes the source.
putModuleV0 :: (MonadBlobStore m, MonadPut m) => Module -> m ()
putModuleV0 (ModuleV0 ModuleV{..}) = sPut =<< loadRef moduleVSource
putModuleV0 (ModuleV1 ModuleV{..}) = sPut =<< loadRef moduleVSource

--------------------------------------------------------------------------------

-- |A cached 'Module' accessed via a cached 'Reference' i.e., a 'Reference'
-- that might or might not yield the actual value (the 'Module').
-- The 'CachedModule' is further "hashed" making it suitable for storing in
-- the 'LFMBTree' which stores references to the 'Modules's in the underlying storage.
--
-- The module is cached in the 'ModuleCache' while the actual artifact is
-- loaded on demand.
type CachedModule = HashedCachedRef ModuleCache Module

-- |The cache retaining 'Module's
type ModuleCache = FIFOCache Module

-- |Construct a new `ModuleCache` with the given size.
newModuleCache :: Int -> IO ModuleCache
newModuleCache = newCache

-- |Make sure that a monad supports the `MonadBlobStore` and `MonadCache`
-- for the modules cache.
type SupportsPersistentModule m = (MonadBlobStore m, MonadCache ModuleCache m)

-- |The collection of modules stored in a block state.
data Modules = Modules {
  -- |A tree of 'Module's indexed by 'ModuleIndex'
  -- The modules themselves are cached `HashedCachedRef` hence only a limited
  -- amount of modules may be retained in memory at the same time.
  -- Modules themselves are wrapped in a @DirectBufferedRef@ which
  -- serves the purpose of not loading the artifact before it is required
  -- by the rust wasm execution engine.
  _modulesTable :: !(LFMBTree' ModuleIndex HashedBufferedRef CachedModule),
  -- |A map of ModuleRef to ModuleIndex.
  _modulesMap :: !(Map ModuleRef ModuleIndex)
  }
makeLenses ''Modules


-- | The hash of the collection of modules is the hash of the tree.
instance SupportsPersistentModule m => MHashableTo m Hash Modules where
  getHashM = getHashM . _modulesTable

instance SupportsPersistentModule m => BlobStorable m Modules where
  load = do
    table <- load
    return $ do
      _modulesTable <- table
      _modulesMap <- foldl' (\m (idx, aModule) ->
                             Map.insert (GSWasm.moduleReference aModule) idx m)
                               Map.empty <$> LFMB.toAscPairList _modulesTable
      return Modules{..}
  storeUpdate m@Modules{..} = do
    (pModulesTable, _modulesTable') <- storeUpdate _modulesTable
    return (pModulesTable, m { _modulesTable = _modulesTable' })

instance SupportsPersistentModule m => Cacheable m Modules where
  cache Modules{..} = do
    modulesTable' <- cache _modulesTable
    return Modules { _modulesTable = modulesTable', ..}

--------------------------------------------------------------------------------

-- |The empty collection of modules
emptyModules :: Modules
emptyModules = Modules LFMB.empty Map.empty

-- |Try to add interfaces to the module table. If a module with the given
-- reference exists returns @Nothing@.
putInterface :: (IsWasmVersion v, SupportsPersistentModule m)
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

getModule :: SupportsPersistentModule m => ModuleRef -> Modules -> m (Maybe Module)
getModule ref mods =
  let modIdx = Map.lookup ref (mods ^. modulesMap) in
  case modIdx of
    Nothing -> return Nothing
    Just idx -> LFMB.lookup idx (mods ^. modulesTable)

-- |Gets the 'HashedCachedRef' to a module as stored in the module table
-- to be given to instances when associating them with the interface.
-- The reason we return the reference here is to allow for sharing of the reference.
getModuleReference :: SupportsPersistentModule m => ModuleRef -> Modules -> m (Maybe CachedModule)
getModuleReference ref mods =
  let modIdx = Map.lookup ref (mods ^. modulesMap) in
  case modIdx of
    Nothing -> return Nothing
    Just idx -> LFMB.lookupRef idx (mods ^. modulesTable)

-- |Get an interface by module reference.
getInterface :: SupportsPersistentModule m
             => ModuleRef
             -> Modules
             -> m (Maybe (GSWasm.ModuleInterface PersistentInstrumentedModuleV))
getInterface ref mods = fmap getModuleInterface <$> getModule ref mods

-- |Get the source of a module by module reference.
getSource :: SupportsPersistentModule m => ModuleRef -> Modules -> m (Maybe WasmModule)
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
storePersistentModule (TransientModules.ModuleV0 TransientModules.ModuleV{ ..}) = do
  let moduleVInterface' = PIMVMem <$> moduleVInterface
  moduleVSource' <- storeRef moduleVSource
  return (ModuleV0 (ModuleV { moduleVInterface = moduleVInterface', moduleVSource = moduleVSource'}))
storePersistentModule (TransientModules.ModuleV1 TransientModules.ModuleV{..}) = do
  let moduleVInterface' = PIMVMem <$> moduleVInterface
  moduleVSource' <- storeRef moduleVSource
  return (ModuleV1 (ModuleV { moduleVInterface = moduleVInterface', moduleVSource = moduleVSource'}))

makePersistentModules :: SupportsPersistentModule m
                       => TransientModules.Modules
                       -> m Modules
makePersistentModules mods = do
  let modlist = TransientModules.moduleList mods
  _modulesTable <- mapM (storePersistentModule . snd) modlist >>=
                     LFMB.fromAscList
  return Modules{ _modulesMap = TransientModules._modulesMap mods, ..}

--------------------------------------------------------------------------------

-- |Serialize modules in V0 format.
putModulesV0 :: (SupportsPersistentModule m, MonadPut m) => Modules -> m ()
putModulesV0 mods = do
    let mt = mods ^. modulesTable
    liftPut $ putWord64be $ LFMB.size mt
    LFMB.mmap_ putModuleV0 mt

-- |Migrate smart contract modules from context @m@ to the context @t m@.
migrateModules ::
  forall t m . (SupportsPersistentModule m, SupportsPersistentModule (t m), SupportMigration m t)
  => Modules
  -> t m Modules
migrateModules mods = do
    newModulesTable <- LFMB.migrateLFMBTree migrateCachedModule (_modulesTable mods)
    return
        Modules
            { _modulesMap = _modulesMap mods
            , _modulesTable = newModulesTable
            }
    where 
      migrateCachedModule :: CachedModule -> t m CachedModule
      migrateCachedModule cm = do
        existingModule <- lift (refLoad cm)
        case existingModule of
          ModuleV0 v0 -> migrateModuleV v0
          ModuleV1 v1 -> migrateModuleV v1

      migrateModuleV :: forall v . (IsWasmVersion v) => ModuleV v -> t m CachedModule
      migrateModuleV ModuleV{..} = do
        newModuleVSource <- do
          -- Load the module source from the old context.
          s <- lift (loadRef moduleVSource)
          -- and store it in the new context, returning a reference to it.
          storeRef s
        -- then store the module itself to the new state. The main complexity
        -- here is the @BlobPtr@.
        storeV (getWasmVersion @v) newModuleVSource moduleVInterface

      -- Store the new version of the module in context @t m@.
      -- The new module is always stored on disk, and only the location in the new blob store is retained in memory.
      -- The module is not cached.
      storeV ::
        SWasmVersion v ->
        -- |The location where the module source is stored in the **new** context.
        BlobRef (WasmModuleV v) ->
        -- |The module interface that needs to be migrated.
        GSWasm.ModuleInterfaceA (PersistentInstrumentedModuleV v) ->
        t m CachedModule
      -- This function must match what the @storeUpdateDirect@ and @loadDirect@
      -- do. In order to avoid copying as much as possible we almost duplicate
      -- the implementation here, but with two differences
      -- - the artifact is loaded from the old context (monad @m@)
      -- - we always construct a 'HCRFlushed' value, and the instance is never cached.
      storeV ver newSource GSWasm.ModuleInterface{..} = do
              !instrumentedModuleBytes <- case miModule of
                PIMVMem instrModule -> return $ case ver of
                    SV0 -> encode instrModule
                    SV1 -> encode instrModule
                PIMVPtr ptr -> do
                  artifact <- lift (loadBlobPtr ptr)
                  return $ runPut $ do
                    put (demoteWasmVersion ver)
                    putWord32be (fromIntegral (BS.length artifact))
                    putByteString artifact
              let headerBytes = runPut $ do
                    put miModuleRef
                    putSafeSetOf put miExposedInit
                    putSafeMapOf put (putSafeSetOf put) miExposedReceive
                  footerBytes = runPut $ do
                    putWord64be miModuleSize
                    put newSource
                  !headerLen = fromIntegral $ BS.length headerBytes
                  !imLen = fromIntegral $ BS.length instrumentedModuleBytes
              br <- storeRaw (headerBytes <> instrumentedModuleBytes <> footerBytes)
              let !miModule' = PIMVPtr BlobPtr {
                      -- Pointer is blob ref + 8 bytes (length of blob) + header length +
                      -- 4 bytes for version + 4 bytes for length of instrumented module
                      theBlobPtr = theBlobRef br + 8 + headerLen + 8,
                      -- Length is the length of the serialized instrumented module -
                      -- 4 bytes for version - 4 bytes for length
                      blobPtrLen =  imLen - 8
                    }
              let mv' = ModuleV{moduleVInterface = GSWasm.ModuleInterface{miModule = miModule',..}, moduleVSource = newSource,..}
              -- getHash here is very cheap, in fact it just takes the @moduleReference@ field and coerces it.
              return $! HCRFlushed br (getHash (mkModule ver mv'))
      mkModule :: SWasmVersion v -> ModuleV v -> Module
      mkModule SV0 = ModuleV0
      mkModule SV1 = ModuleV1
