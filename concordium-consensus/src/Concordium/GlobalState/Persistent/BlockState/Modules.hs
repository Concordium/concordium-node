{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.GlobalState.Persistent.BlockState.Modules (
    Module (..),
    ModuleV (..),
    Modules,
    ModuleCache,
    CachedModule,
    SupportsPersistentModule,
    getModuleInterface,
    PersistentInstrumentedModuleV,
    makePersistentInstrumentedModuleV,
    loadInstrumentedModuleV,
    emptyModules,
    getInterface,
    getSource,
    getModuleReference,
    putInterface,
    moduleRefList,
    newModuleCache,
    unsafeToModuleV,
    migrateModules,
) where

import Concordium.Crypto.SHA256
import Concordium.Genesis.Data (StateMigrationParameters (..))
import Concordium.GlobalState.BlockState (ModulesHash (..))
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Persistent.CachedRef
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree')
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMB
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.Logger (LogLevel (..), LogSource (..), MonadLogger (..))
import qualified Concordium.Scheduler.WasmIntegration as WasmV0
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils
import Concordium.Utils.Serialization
import Concordium.Wasm
import Control.Monad.Trans
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Word
import Lens.Micro.Platform

-- | Index of the module in the module table. Reflects when the module was added
--  to the table.
type ModuleIndex = Word64

--------------------------------------------------------------------------------

-- | An @InstrumentedModuleV v@ in the @PersistentBlockState@, where
--  @v@ is the @WasmVersion@.
data PersistentInstrumentedModuleV (v :: WasmVersion)
    = -- | The instrumented module is retained in memory only.
      --  This is the case before finalization.
      PIMVMem !(GSWasm.InstrumentedModuleV v)
    | -- | The instrumented module resides solely on disk, and thus the raw
      --  bytes can be read from the blob store via the @BlobPtr@.
      --  The @BlobPtr@ is used to reconstruct artifact on the Rust side, copying it as needed
      --  from the blob store.
      PIMVPtr !(BlobPtr (GSWasm.InstrumentedModuleV v))
    deriving (Show)

-- | Make a 'PersistentInstrumentedModuleV' from a 'GSWasm.InstrumentedModuleV', retaining it in
-- memory only.
makePersistentInstrumentedModuleV :: GSWasm.InstrumentedModuleV v -> PersistentInstrumentedModuleV v
makePersistentInstrumentedModuleV = PIMVMem

-- | Load a 'PersistentInstrumentedModuleV', retrieving the artifact.
--  If the artifact has been persisted to the blob store, the artifact will wrap a pointer into
--  the memory-mapped blob store.
loadInstrumentedModuleV :: forall m v. (MonadBlobStore m, IsWasmVersion v) => PersistentInstrumentedModuleV v -> m (GSWasm.InstrumentedModuleV v)
loadInstrumentedModuleV (PIMVMem im) = return im
loadInstrumentedModuleV (PIMVPtr ptr) = do
    bs <- loadBlobPtr ptr
    return $! GSWasm.instrumentedModuleFromBytes getWasmVersion bs

-- | A module contains both the module interface and the raw source code of the
--  module. The module is parameterized by the wasm version, which determines the shape
--  of the module interface.
data ModuleV v = ModuleV
    { -- | The instrumented module, ready to be instantiated.
      moduleVInterface :: !(GSWasm.ModuleInterfaceA (PersistentInstrumentedModuleV v)),
      -- | A plain reference to the raw module binary source. This is generally not needed by consensus, so
      -- it is almost always simply kept on disk.
      moduleVSource :: !(BlobRef (WasmModuleV v))
    }
    deriving (Show)

-- | Helper to convert from an interface to a module.
toModule :: forall v. (IsWasmVersion v) => GSWasm.ModuleInterfaceV v -> BlobRef (WasmModuleV v) -> Module
toModule mvi moduleVSource =
    case getWasmVersion @v of
        SV0 -> ModuleV0 ModuleV{..}
        SV1 -> ModuleV1 ModuleV{..}
  where
    moduleVInterface = PIMVMem <$> mvi

-- | A module, either of version 0 or 1. This is only used when storing a module
--  independently, e.g., in the module table. When a module is referenced from a
--  contract instance we use the ModuleV type directly so we may tie the version
--  of the module to the version of the instance.
data Module where
    ModuleV0 :: !(ModuleV GSWasm.V0) -> Module
    ModuleV1 :: !(ModuleV GSWasm.V1) -> Module
    deriving (Show)

getModuleInterface :: Module -> GSWasm.ModuleInterface PersistentInstrumentedModuleV
getModuleInterface (ModuleV0 m) = GSWasm.ModuleInterfaceV0 (moduleVInterface m)
getModuleInterface (ModuleV1 m) = GSWasm.ModuleInterfaceV1 (moduleVInterface m)

-- | Coerce a module to V0. Will fail if the version is not 'V0'.
unsafeToModuleV0 :: Module -> ModuleV V0
unsafeToModuleV0 (ModuleV0 m) = m
unsafeToModuleV0 (ModuleV1 _) = error "Could not coerce module to V0."

-- | Coerce a module to V1. Will fail if the version is not 'V1'.
unsafeToModuleV1 :: Module -> ModuleV V1
unsafeToModuleV1 (ModuleV0 _) = error "Could not coerce module to V1."
unsafeToModuleV1 (ModuleV1 m) = m

-- | Coerce a 'Module' to a 'ModuleV' depending on the 'WasmVersion'.
--  This results in an error if the module is not of the desired version.
unsafeToModuleV :: forall v. (IsWasmVersion v) => Module -> ModuleV v
unsafeToModuleV = case getWasmVersion @v of
    SV0 -> unsafeToModuleV0
    SV1 -> unsafeToModuleV1

instance GSWasm.HasModuleRef Module where
    moduleReference (ModuleV0 m) = GSWasm.moduleReference (moduleVInterface m)
    moduleReference (ModuleV1 m) = GSWasm.moduleReference (moduleVInterface m)

-- The module reference already takes versioning into account, so this instance is reasonable.
instance HashableTo Hash Module where
    getHash = coerce . GSWasm.moduleReference

instance (Monad m) => MHashableTo m Hash Module
instance (MonadBlobStore m) => Cacheable m Module

-- | This instance is based on and should be compatible with the 'Serialize' instance
--  for 'BasicModuleInterface'.
instance (MonadLogger m, MonadBlobStore m, MonadProtocolVersion m) => DirectBlobStorable m Module where
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
                    miModule =
                        PIMVPtr
                            BlobPtr
                                { theBlobPtr =
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
            Right mv@(ModuleV0 mv0@(ModuleV{moduleVInterface = GSWasm.ModuleInterface{miModule = PIMVPtr artPtr, ..}, ..})) | potentialLegacyArtifacts -> do
                artBS <- loadBlobPtr artPtr
                if GSWasm.isV0LegacyArtifact artBS
                    then do
                        logEvent GlobalState LLTrace $ "Recompiling V0 module " ++ show miModuleRef
                        source <- loadRef moduleVSource
                        case WasmV0.compileModule CSV0 source of
                            Nothing -> error "Stored module that is not valid."
                            Just (_, compiled) -> do
                                return $! ModuleV0 mv0{moduleVInterface = (moduleVInterface mv0){GSWasm.miModule = PIMVMem compiled}}
                    else return mv
            Right mv@(ModuleV1 mv1@(ModuleV{moduleVInterface = GSWasm.ModuleInterface{miModule = PIMVPtr artPtr, ..}, ..})) | potentialLegacyArtifacts -> do
                artBS <- loadBlobPtr artPtr
                if GSWasm.isV0LegacyArtifact artBS
                    then do
                        logEvent GlobalState LLTrace $ "Recompiling V1 module " ++ show miModuleRef
                        source <- loadRef moduleVSource
                        case WasmV1.compileModule (WasmV1.validationConfigAllowP1P6 CSV0) source of
                            Nothing -> error "Stored module that is not valid."
                            Just (_, compiled) -> do
                                return $! ModuleV1 mv1{moduleVInterface = (moduleVInterface mv1){GSWasm.miModule = PIMVMem compiled}}
                    else return mv
            Right mv -> return mv
      where
        -- When a node is running protocol 6 or lower it might have been started prior to the new notion of Wasm
        -- artifacts, which needs to be recompiled on load.
        potentialLegacyArtifacts = demoteProtocolVersion (protocolVersion @(MPV m)) <= P6

    storeUpdateDirect mdl = do
        case mdl of
            ModuleV0 mv0 -> sudV SV0 mv0
            ModuleV1 mv1 -> sudV SV1 mv1
      where
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
            let !miModule' =
                    PIMVPtr
                        BlobPtr
                            { -- Pointer is blob ref + 8 bytes (length of blob) + header length +
                              -- 4 bytes for version + 4 bytes for length of instrumented module
                              theBlobPtr = theBlobRef br + 8 + headerLen + 8,
                              -- Length is the length of the serialized instrumented module -
                              -- 4 bytes for version - 4 bytes for length
                              blobPtrLen = imLen - 8
                            }
            let mv' = ModuleV{moduleVInterface = GSWasm.ModuleInterface{miModule = miModule', ..}, ..}
            return $!! (br, mkModule ver mv')
        mkModule :: SWasmVersion v -> ModuleV v -> Module
        mkModule SV0 = ModuleV0
        mkModule SV1 = ModuleV1

--------------------------------------------------------------------------------

-- | A cached 'Module' accessed via a cached 'Reference' i.e., a 'Reference'
--  that might or might not yield the actual value (the 'Module').
--  The 'CachedModule' is further "hashed" making it suitable for storing in
--  the 'LFMBTree' which stores references to the 'Modules's in the underlying storage.
--
--  The module is cached in the 'ModuleCache' while the actual artifact is
--  loaded on demand.
type CachedModule = HashedCachedRef ModuleCache Module

-- | The cache retaining 'Module's
type ModuleCache = FIFOCache Module

-- | Construct a new `ModuleCache` with the given size.
newModuleCache :: Int -> IO ModuleCache
newModuleCache = newCache

-- | Make sure that a monad supports the `MonadBlobStore` and `MonadCache`
--  for the modules cache.
type SupportsPersistentModule m = (MonadLogger m, MonadBlobStore m, MonadCache ModuleCache m)

-- | The collection of modules stored in a block state.
data Modules = Modules
    { -- | A tree of 'Module's indexed by 'ModuleIndex'
      --  The modules themselves are cached `HashedCachedRef` hence only a limited
      --  amount of modules may be retained in memory at the same time.
      --  Modules themselves are wrapped in a @DirectBufferedRef@ which
      --  serves the purpose of not loading the artifact before it is required
      --  by the rust wasm execution engine.
      _modulesTable :: !(LFMBTree' ModuleIndex HashedBufferedRef CachedModule),
      -- | A map of ModuleRef to ModuleIndex.
      _modulesMap :: !(Map ModuleRef ModuleIndex)
    }

makeLenses ''Modules

-- | The hash of the collection of modules is the hash of the tree.
instance (MonadProtocolVersion m, SupportsPersistentModule m, IsBlockHashVersion (BlockHashVersionFor pv)) => MHashableTo m (ModulesHash pv) Modules where
    getHashM =
        fmap (ModulesHash . LFMB.theLFMBTreeHash @(BlockHashVersionFor pv))
            . getHashM
            . _modulesTable

instance (MonadProtocolVersion m, SupportsPersistentModule m) => BlobStorable m Modules where
    load = do
        table <- load
        return $ do
            _modulesTable <- table
            _modulesMap <-
                foldl'
                    ( \m (idx, aModule) ->
                        Map.insert (GSWasm.moduleReference aModule) idx m
                    )
                    Map.empty
                    <$> LFMB.toAscPairList _modulesTable
            return Modules{..}
    storeUpdate m@Modules{..} = do
        (pModulesTable, _modulesTable') <- storeUpdate _modulesTable
        return (pModulesTable, m{_modulesTable = _modulesTable'})

instance (MonadProtocolVersion m, SupportsPersistentModule m) => Cacheable m Modules where
    cache Modules{..} = do
        modulesTable' <- cache _modulesTable
        return Modules{_modulesTable = modulesTable', ..}

--------------------------------------------------------------------------------

-- | The empty collection of modules
emptyModules :: Modules
emptyModules = Modules LFMB.empty Map.empty

-- | Try to add interfaces to the module table. If a module with the given
--  reference exists returns @Nothing@.
putInterface ::
    (MonadProtocolVersion m, IsWasmVersion v, SupportsPersistentModule m) =>
    (GSWasm.ModuleInterfaceV v, WasmModuleV v) ->
    Modules ->
    m (Maybe Modules)
putInterface (modul, src) m =
    if Map.member mref (m ^. modulesMap)
        then return Nothing
        else do
            src' <- storeRef src
            (idx, modulesTable') <- LFMB.append (toModule modul src') $ m ^. modulesTable
            return $
                Just $
                    m
                        & modulesTable .~ modulesTable'
                        & modulesMap %~ Map.insert mref idx
  where
    mref = GSWasm.moduleReference modul

getModule :: (MonadProtocolVersion m, SupportsPersistentModule m) => ModuleRef -> Modules -> m (Maybe Module)
getModule ref mods =
    let modIdx = Map.lookup ref (mods ^. modulesMap)
    in  case modIdx of
            Nothing -> return Nothing
            Just idx -> LFMB.lookup idx (mods ^. modulesTable)

-- | Gets the 'HashedCachedRef' to a module as stored in the module table
--  to be given to instances when associating them with the interface.
--  The reason we return the reference here is to allow for sharing of the reference.
getModuleReference :: (MonadProtocolVersion m, SupportsPersistentModule m) => ModuleRef -> Modules -> m (Maybe CachedModule)
getModuleReference ref mods =
    let modIdx = Map.lookup ref (mods ^. modulesMap)
    in  case modIdx of
            Nothing -> return Nothing
            Just idx -> LFMB.lookupRef idx (mods ^. modulesTable)

-- | Get an interface by module reference.
getInterface ::
    (MonadProtocolVersion m, SupportsPersistentModule m) =>
    ModuleRef ->
    Modules ->
    m (Maybe (GSWasm.ModuleInterface PersistentInstrumentedModuleV))
getInterface ref mods = fmap getModuleInterface <$> getModule ref mods

-- | Get the source of a module by module reference.
getSource :: (MonadProtocolVersion m, SupportsPersistentModule m) => ModuleRef -> Modules -> m (Maybe WasmModule)
getSource ref mods = do
    m <- getModule ref mods
    case m of
        Nothing -> return Nothing
        Just (ModuleV0 ModuleV{..}) -> Just . WasmModuleV0 <$> loadRef moduleVSource
        Just (ModuleV1 ModuleV{..}) -> Just . WasmModuleV1 <$> loadRef moduleVSource

-- | Get the list of all currently deployed modules.
--  The order of the list is not specified.
moduleRefList :: Modules -> [ModuleRef]
moduleRefList mods = Map.keys (mods ^. modulesMap)

--------------------------------------------------------------------------------

-- | Migrate smart contract modules from context @m@ to the context @t m@.
migrateModules ::
    forall t m.
    ( MonadProtocolVersion m,
      MonadProtocolVersion (t m),
      SupportsPersistentModule m,
      SupportsPersistentModule (t m),
      SupportMigration m t
    ) =>
    StateMigrationParameters (MPV m) (MPV (t m)) ->
    Modules ->
    t m Modules
migrateModules migration mods = do
    newModulesTable <- LFMB.migrateLFMBTree migrateCachedModule (_modulesTable mods)
    return
        Modules
            { _modulesMap = _modulesMap mods,
              _modulesTable = newModulesTable
            }
  where
    migrateCachedModule :: CachedModule -> t m CachedModule
    migrateCachedModule cm = do
        existingModule <- lift (refLoad cm)
        case existingModule of
            ModuleV0 v0 -> migrateModuleV v0
            ModuleV1 v1 -> migrateModuleV v1

    migrateModuleV :: forall v. (IsWasmVersion v) => ModuleV v -> t m CachedModule
    migrateModuleV ModuleV{..} = do
        (newModuleVSource, wasmMod) <- do
            -- Load the module source from the old context.
            s <- lift (loadRef moduleVSource)
            -- and store it in the new context, returning a reference to it.
            (,s) <$> storeRef s
        -- load the module artifact into memory from the old state. This is
        -- cheap since the artifact, which is the big part, is neither copied,
        -- nor deserialized.

        artifact <- lift (loadInstrumentedModuleV (GSWasm.miModule moduleVInterface))
        newModuleVInterface <-
            -- If it is a legacy artifact then we want to migrate it over to the new
            -- version by recompiling since execution no longer supports the old format.
            if GSWasm.isV0LegacyArtifact (GSWasm.imWasmArtifactBytes artifact)
                then recompileArtifact @v wasmMod moduleVInterface
                else -- If it is not a legacy module then we don't have to recompile
                -- unless we're migrating from P6 to P7 where the new reduced
                -- execution costs were introduced.
                case migration of
                    StateMigrationParametersTrivial -> return $! moduleVInterface{GSWasm.miModule = PIMVMem artifact}
                    StateMigrationParametersP1P2 -> return $! moduleVInterface{GSWasm.miModule = PIMVMem artifact}
                    StateMigrationParametersP2P3 -> return $! moduleVInterface{GSWasm.miModule = PIMVMem artifact}
                    StateMigrationParametersP3ToP4{} -> return $! moduleVInterface{GSWasm.miModule = PIMVMem artifact}
                    StateMigrationParametersP4ToP5{} -> return $! moduleVInterface{GSWasm.miModule = PIMVMem artifact}
                    StateMigrationParametersP5ToP6{} -> return $! moduleVInterface{GSWasm.miModule = PIMVMem artifact}
                    StateMigrationParametersP6ToP7{} -> migrateToP7 @v wasmMod -- always recompile to lower transaction costs.
                    StateMigrationParametersP7ToP8{} -> return $! moduleVInterface{GSWasm.miModule = PIMVMem artifact}

        -- store the module into the new state, and remove it from memory
        makeFlushedHashedCachedRef $!
            mkModule (getWasmVersion @v) $!
                ModuleV
                    { moduleVInterface = newModuleVInterface,
                      moduleVSource = newModuleVSource
                    }

    mkModule :: SWasmVersion v -> ModuleV v -> Module
    mkModule SV0 = ModuleV0
    mkModule SV1 = ModuleV1

    -- Recompile a wasm module from the given source for protocols 1-6.
    -- This does not change the semantics, but does convert the artifact into the new format.
    recompileArtifact :: forall v iface. (IsWasmVersion v) => WasmModuleV v -> GSWasm.ModuleInterfaceA iface -> t m (GSWasm.ModuleInterfaceA (PersistentInstrumentedModuleV v))
    recompileArtifact wasmMod oldIface = do
        case getWasmVersion @v of
            SV0 ->
                case WasmV0.compileModule CSV0 wasmMod of
                    Nothing -> error "Stored V0 module that is not valid."
                    Just (_, compiled) -> do
                        return $! oldIface{GSWasm.miModule = PIMVMem compiled}
            SV1 ->
                case WasmV1.compileModule (WasmV1.validationConfigAllowP1P6 CSV0) wasmMod of
                    Nothing -> error "Stored V1 module that is not valid."
                    Just (_, compiled) -> do
                        return $! oldIface{GSWasm.miModule = PIMVMem compiled}

    -- Recompile a wasm module from the given source for protocol 7
    -- cost semantics (i.e., the protocol version of @t m@).
    migrateToP7 :: forall v. (MPV (t m) ~ P7, IsWasmVersion v) => WasmModuleV v -> t m (GSWasm.ModuleInterfaceA (PersistentInstrumentedModuleV v))
    migrateToP7 wasmMod = do
        case getWasmVersion @v of
            SV0 ->
                case WasmV0.processModule (protocolVersion @(MPV (t m))) wasmMod of
                    Nothing -> error "Stored V0 module that is not valid."
                    Just iface -> do
                        return $! makePersistentInstrumentedModuleV <$> iface
            SV1 ->
                case WasmV1.processModuleConfig WasmV1.processingConfigRecompileForP7 wasmMod of
                    Nothing -> error "Stored V1 module that is not valid."
                    Just iface -> do
                        return $! makePersistentInstrumentedModuleV <$> iface
