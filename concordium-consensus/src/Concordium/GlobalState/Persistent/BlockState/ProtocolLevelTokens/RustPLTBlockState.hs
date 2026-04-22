{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Bindings to the Rust PLT block state implementation.
--
-- Each foreign imported function must match the signature of functions found on the Rust side.
module Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState (
    RustPLTBlockState,
    ForeignPLTBlockStatePtr,
    wrapFFIPtr,
    empty,
    withPLTBlockState,
    migrate,
    ProtocolLevelTokensHash (..),
) where

import Control.Monad.Trans (lift, liftIO)
import qualified Data.Serialize as S
import qualified Foreign as FFI

import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.Types as Types
import qualified Concordium.Types.HashableTo as Hashable
import qualified Control.Monad as Monad
import qualified Data.FixedByteString as FixedByteString

import qualified Concordium.GlobalState.ContractStateFFIHelpers as FFI
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore

-- | Opaque type representing a Rust maintained PLT state.
-- The value is allocated in Rust and must be deallocated in Rust.
data RustPLTBlockState

-- | Opaque pointer to a immutable PLT block state save-point managed by the rust library.
--
-- Memory is deallocated using a finalizer.
newtype ForeignPLTBlockStatePtr = ForeignPLTBlockStatePtr (FFI.ForeignPtr RustPLTBlockState)

-- | Helper function to convert a raw pointer passed by the Rust library into a `PLTBlockState` object.
wrapFFIPtr :: FFI.Ptr RustPLTBlockState -> IO ForeignPLTBlockStatePtr
wrapFFIPtr blockStatePtr = ForeignPLTBlockStatePtr <$> FFI.newForeignPtr ffiFreePLTBlockState blockStatePtr

-- | Deallocate a pointer to `PLTBlockState`.
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall unsafe "&ffi_free_plt_block_state"
    ffiFreePLTBlockState :: FFI.FinalizerPtr RustPLTBlockState

-- | Get temporary access to the block state pointer. The pointer should not be
-- leaked from the computation.
--
-- This ensures the finalizer is not called until the computation is over.
withPLTBlockState :: ForeignPLTBlockStatePtr -> (FFI.Ptr RustPLTBlockState -> IO a) -> IO a
withPLTBlockState (ForeignPLTBlockStatePtr foreignPtr) = FFI.withForeignPtr foreignPtr

-- | Allocate new empty block state.
empty ::
    forall m.
    (BlobStore.MonadBlobStore m, Types.MonadProtocolVersion m) =>
    m ForeignPLTBlockStatePtr
empty = liftIO $ do
    state <- ffiEmptyPLTBlockState (sProtocolVersionToWord64 $ Types.protocolVersion @(Types.MPV m))
    wrapFFIPtr state

sProtocolVersionToWord64 ::
    Types.SProtocolVersion pv ->
    FFI.Word64
sProtocolVersionToWord64 spv = Types.protocolVersionToWord64 $ Types.demoteProtocolVersion spv

-- | Allocate new empty block state.
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall "ffi_empty_plt_block_state"
    ffiEmptyPLTBlockState ::
        -- | Protocol version of the block.
        FFI.Word64 ->
        -- | New block state
        IO (FFI.Ptr RustPLTBlockState)

instance (BlobStore.MonadBlobStore m, Types.MonadProtocolVersion m) => BlobStore.BlobStorable m ForeignPLTBlockStatePtr where
    load = do
        blobRef <- S.get
        pure $! do
            loadCallback <- fst <$> BlobStore.getCallbacks
            liftIO $! do
                FFI.alloca $ \blockStateDestPtr -> do
                    status <-
                        ffiLoadPLTBlockState
                            loadCallback
                            blobRef
                            (sProtocolVersionToWord64 $ Types.protocolVersion @(Types.MPV m))
                            blockStateDestPtr
                    Monad.unless (status == 0) $ error "Unexpected panic when loading a block state"
                    blockState <- FFI.peek blockStateDestPtr
                    wrapFFIPtr blockState
    storeUpdate pltBlockState = do
        storeCallback <- snd <$> BlobStore.getCallbacks
        blobRef <- liftIO $ FFI.alloca $ \blobRefDestPtr -> do
            status <- withPLTBlockState pltBlockState $ ffiStorePLTBlockState storeCallback blobRefDestPtr
            Monad.unless (status == 0) $ error "Unexpected panic when storing a block state"
            BlobStore.BlobRef @RustPLTBlockState <$> FFI.peek blobRefDestPtr
        return (S.put blobRef, pltBlockState)

-- | Load PLT block state from the given disk reference.
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall "ffi_load_plt_block_state"
    ffiLoadPLTBlockState ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Reference in the blob store.
        BlobStore.BlobRef RustPLTBlockState ->
        -- | Protocol version of the block.
        FFI.Word64 ->
        -- | Destination pointer for the loaded block state.
        FFI.Ptr (FFI.Ptr RustPLTBlockState) ->
        -- | Status code
        IO FFI.Word8

-- | Write out the block state using the provided callback, and return a `BlobRef`.
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall "ffi_store_plt_block_state"
    ffiStorePLTBlockState ::
        -- | The provided closure is called to write data to blob store.
        FFI.StoreCallback ->
        -- | Destination for the new reference in the blob store.
        FFI.Ptr FFI.Word64 ->
        -- | Pointer to the block state to write.
        FFI.Ptr RustPLTBlockState ->
        -- | Status code
        IO FFI.Word8

instance (BlobStore.MonadBlobStore m) => BlobStore.Cacheable m ForeignPLTBlockStatePtr where
    cache blockState = do
        loadCallback <- fst <$> BlobStore.getCallbacks
        status <- liftIO $! withPLTBlockState blockState (ffiCachePLTBlockState loadCallback)
        Monad.unless (status == 0) $ error "Unexpected panic when caching a block state"
        return blockState

-- | Cache block state into memory.
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall "ffi_cache_plt_block_state"
    ffiCachePLTBlockState ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Pointer to the block state to cache into memory.
        FFI.Ptr RustPLTBlockState ->
        IO FFI.Word8

-- | The hash of protocol-levels tokens state.
newtype ProtocolLevelTokensHash = ProtocolLevelTokensHash {theProtocolLevelTokensHash :: SHA256.Hash}
    deriving newtype (Eq, Ord, Show, S.Serialize)

instance
    (BlobStore.MonadBlobStore m) =>
    Hashable.MHashableTo m ProtocolLevelTokensHash ForeignPLTBlockStatePtr
    where
    getHashM blockState = do
        loadCallback <- fst <$> BlobStore.getCallbacks
        ((), hash) <-
            liftIO $
                withPLTBlockState blockState $ \blockStatePtr ->
                    FixedByteString.createWith $ \hashDestPtr -> do
                        status <- ffiHashPLTBlockState loadCallback blockStatePtr hashDestPtr
                        Monad.unless (status == 0) $ error "Unexpected panic when hashing a block state"
        return $ ProtocolLevelTokensHash (SHA256.Hash hash)

-- | Compute the hash of the block state.
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall "ffi_hash_plt_block_state"
    ffiHashPLTBlockState ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Pointer to the block state to hash.
        FFI.Ptr RustPLTBlockState ->
        -- | Pointer to write destination of the hash
        FFI.Ptr FFI.Word8 ->
        -- | Status code
        IO FFI.Word8

-- | Run migration during a protocol update.
migrate ::
    forall m t.
    ( BlobStore.SupportMigration m t,
      Types.MonadProtocolVersion (t m)
    ) =>
    -- | Current block state
    ForeignPLTBlockStatePtr ->
    -- | New migrated block state
    t m ForeignPLTBlockStatePtr
migrate currentState = do
    oldLoadCallback <- fst <$> lift BlobStore.getCallbacks
    newStoreCallback <- snd <$> BlobStore.getCallbacks
    let newSProtocolVersion = Types.protocolVersion @(Types.MPV (t m))
    liftIO $ FFI.alloca $ \newStateDestPtr -> do
        status <-
            withPLTBlockState currentState $
                ffiMigratePLTBlockState
                    oldLoadCallback
                    newStoreCallback
                    (sProtocolVersionToWord64 newSProtocolVersion)
                    newStateDestPtr
        Monad.unless (status == 0) $ error "Unexpected panic when migrating a block state"
        newState <- FFI.peek newStateDestPtr
        wrapFFIPtr newState

-- | Migrate PLT block state from one blob store to another.
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall "ffi_migrate_plt_block_state"
    ffiMigratePLTBlockState ::
        -- | Called to read data from the blob store being migrated from.
        FFI.LoadCallback ->
        -- | Called to write data to the blob store being migrated to.
        FFI.StoreCallback ->
        -- | Protocol version of the block being migrated to.
        FFI.Word64 ->
        -- | Pointer to the new block state.
        FFI.Ptr (FFI.Ptr RustPLTBlockState) ->
        -- | Block state to migrate
        FFI.Ptr RustPLTBlockState ->
        -- | Status code
        IO FFI.Word8
