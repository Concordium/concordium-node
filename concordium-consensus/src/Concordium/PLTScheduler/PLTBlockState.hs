{-# LANGUAGE DerivingVia #-}

-- | Bindings to the PLT block state implementation found in @plt-scheduler/block_state.rs@.
module Concordium.PLTScheduler.PLTBlockState (
    PLTBlockState,
    empty,
    wrapFFIPtr,
    withPLTBlockState,
    migrate,
    Hash,
    -- | Get the inner @SHA256.Hash@.
    innerSha256Hash,
) where

import qualified Data.Serialize as Serialize
import qualified Foreign as FFI

import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.GlobalState.ContractStateFFIHelpers as FFI
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore
import qualified Concordium.Types.HashableTo as Hashable
import Control.Monad.Trans (lift, liftIO)
import qualified Data.FixedByteString as FixedByteString

-- | Opaque pointer to a immutable PLT block state save-point managed by the rust library.
--
-- Memory is deallocated using a finalizer.
newtype PLTBlockState = PLTBlockState (FFI.ForeignPtr PLTBlockState)

-- | Helper function to convert a raw pointer passed by the Rust library into a `PLTBlockState` object.
wrapFFIPtr :: FFI.Ptr PLTBlockState -> IO PLTBlockState
wrapFFIPtr blockStatePtr = PLTBlockState <$> FFI.newForeignPtr ffiFreePLTBlockState blockStatePtr

-- | Deallocate a pointer to `PLTBlockState`.
foreign import ccall unsafe "&ffi_free_plt_block_state"
    ffiFreePLTBlockState :: FFI.FinalizerPtr PLTBlockState

-- | Get temporary access to the block state pointer. The pointer should not be
-- leaked from the computation.
--
-- This ensures the finalizer is not called until the computation is over.
withPLTBlockState :: PLTBlockState -> (FFI.Ptr PLTBlockState -> IO a) -> IO a
withPLTBlockState (PLTBlockState foreignPtr) = FFI.withForeignPtr foreignPtr

-- | Allocate new empty block state
empty :: (BlobStore.MonadBlobStore m) => m PLTBlockState
empty = liftIO $ do
    state <- ffiEmptyPLTBlockState
    wrapFFIPtr state

foreign import ccall "ffi_empty_plt_block_state"
    ffiEmptyPLTBlockState :: IO (FFI.Ptr PLTBlockState)

instance (BlobStore.MonadBlobStore m) => BlobStore.BlobStorable m PLTBlockState where
    load = do
        blobRef <- Serialize.get
        pure $! do
            loadCallback <- fst <$> BlobStore.getCallbacks
            liftIO $! do
                blockState <- ffiLoadPLTBlockState loadCallback blobRef
                wrapFFIPtr blockState
    storeUpdate pltBlockState = do
        storeCallback <- snd <$> BlobStore.getCallbacks
        blobRef <- liftIO $ withPLTBlockState pltBlockState $ ffiStorePLTBlockState storeCallback
        return (Serialize.put blobRef, pltBlockState)

-- | Load PLT block state from the given disk reference.
foreign import ccall "ffi_load_plt_block_state"
    ffiLoadPLTBlockState ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Reference in the blob store.
        BlobStore.BlobRef PLTBlockState ->
        -- | Pointer to the loaded block state.
        IO (FFI.Ptr PLTBlockState)

-- | Write out the block state using the provided callback, and return a `BlobRef`.
foreign import ccall "ffi_store_plt_block_state"
    ffiStorePLTBlockState ::
        -- | The provided closure is called to write data to blob store.
        FFI.StoreCallback ->
        -- | Pointer to the block state to write.
        FFI.Ptr PLTBlockState ->
        -- | New reference in the blob store.
        IO (BlobStore.BlobRef PLTBlockState)

instance (BlobStore.MonadBlobStore m) => BlobStore.Cacheable m PLTBlockState where
    cache blockState = do
        loadCallback <- fst <$> BlobStore.getCallbacks
        liftIO $! withPLTBlockState blockState (ffiCachePLTBlockState loadCallback)
        return blockState

-- | Cache block state into memory.
foreign import ccall "ffi_cache_plt_block_state"
    ffiCachePLTBlockState ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Pointer to the block state to cache into memory.
        FFI.Ptr PLTBlockState ->
        IO ()

-- | The hash of some `PLTBlockState`.
newtype Hash = Hash {innerSha256Hash :: SHA256.Hash}
    deriving newtype (Eq, Ord, Show, Serialize.Serialize)

instance (BlobStore.MonadBlobStore m) => Hashable.MHashableTo m Hash PLTBlockState where
    getHashM blockState = do
        loadCallback <- fst <$> BlobStore.getCallbacks
        ((), hash) <-
            liftIO $
                withPLTBlockState blockState $
                    FixedByteString.createWith . ffiHashPLTBlockState loadCallback
        return $ Hash (SHA256.Hash hash)

-- | Compute the hash of the block state.
foreign import ccall "ffi_hash_plt_block_state"
    ffiHashPLTBlockState ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Pointer to the block state to write.
        FFI.Ptr PLTBlockState ->
        -- | Pointer to write destination of the hash
        FFI.Ptr FFI.Word8 ->
        IO ()

-- | Run migration during a protocol update.
migrate ::
    (BlobStore.SupportMigration m t) =>
    -- | Current block state
    PLTBlockState ->
    -- | New migrated block state
    t m PLTBlockState
migrate currentState = do
    loadCallback <- fst <$> lift BlobStore.getCallbacks
    storeCallback <- snd <$> BlobStore.getCallbacks
    newState <- liftIO $ withPLTBlockState currentState $ ffiMigratePLTBlockState loadCallback storeCallback
    liftIO $ PLTBlockState <$> FFI.newForeignPtr ffiFreePLTBlockState newState

-- | Migrate PLT block state from one blob store to another.
foreign import ccall "ffi_migrate_plt_block_state"
    ffiMigratePLTBlockState ::
        -- | Called to read data from the old blob store.
        FFI.LoadCallback ->
        -- | Called to write data to the new blob store.
        FFI.StoreCallback ->
        -- | Pointer to the old block state.
        FFI.Ptr PLTBlockState ->
        -- | Pointer to the new block state.
        IO (FFI.Ptr PLTBlockState)
