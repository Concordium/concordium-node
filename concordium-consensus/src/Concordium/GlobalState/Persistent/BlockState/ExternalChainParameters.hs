{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Bindings to the Rust external chain-parameters implementation.
--
-- The component stores node-internal chain-parameter state whose public query
-- representation is assembled by the node. It is deliberately separate from
-- the public/wire chain-parameter types in @concordium-base@.
module Concordium.GlobalState.Persistent.BlockState.ExternalChainParameters (
    RustExternalChainParameters,
    ForeignExternalChainParametersPtr,
    wrapFFIPtr,
    empty,
    withExternalChainParameters,
    ExternalChainParametersHash (..),
    getMaxLockDuration,
) where

import Control.Monad.Trans (liftIO)
import qualified Data.Serialize as S
import qualified Foreign as FFI

import Concordium.Common.Time (Duration (..))
import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.Types as Types
import qualified Concordium.Types.HashableTo as Hashable
import qualified Control.Monad as Monad
import qualified Data.FixedByteString as FixedByteString

import qualified Concordium.GlobalState.ContractStateFFIHelpers as FFI
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore

-- | Opaque type representing Rust-maintained external chain parameters.
-- The value is allocated and deallocated in Rust.
data RustExternalChainParameters

-- | Opaque pointer to immutable external chain parameters managed by Rust.
--
-- Memory is deallocated using a finalizer.
newtype ForeignExternalChainParametersPtr = ForeignExternalChainParametersPtr (FFI.ForeignPtr RustExternalChainParameters)

-- | Convert a raw pointer returned by Rust into a managed pointer.
wrapFFIPtr :: FFI.Ptr RustExternalChainParameters -> IO ForeignExternalChainParametersPtr
wrapFFIPtr paramsPtr = ForeignExternalChainParametersPtr <$> FFI.newForeignPtr ffiFreeExternalChainParameters paramsPtr

-- | Deallocate a pointer to external chain parameters.
foreign import ccall unsafe "&ffi_free_external_chain_parameters"
    ffiFreeExternalChainParameters :: FFI.FinalizerPtr RustExternalChainParameters

-- | Get temporary access to the external chain-parameters pointer.
--
-- The pointer must not be leaked from the computation.
withExternalChainParameters :: ForeignExternalChainParametersPtr -> (FFI.Ptr RustExternalChainParameters -> IO a) -> IO a
withExternalChainParameters (ForeignExternalChainParametersPtr foreignPtr) = FFI.withForeignPtr foreignPtr

p11ProtocolVersion :: FFI.Word64
p11ProtocolVersion = Types.protocolVersionToWord64 Types.P11

-- | Allocate new empty external chain parameters.
empty :: (BlobStore.MonadBlobStore m) => m ForeignExternalChainParametersPtr
empty = liftIO $ do
    FFI.alloca $ \paramsDestPtr -> do
        status <- ffiEmptyExternalChainParameters p11ProtocolVersion paramsDestPtr
        Monad.unless (status == 0) $ error "Unexpected panic when creating external chain parameters"
        params <- FFI.peek paramsDestPtr
        wrapFFIPtr params

foreign import ccall "ffi_empty_external_chain_parameters"
    ffiEmptyExternalChainParameters ::
        FFI.Word64 ->
        FFI.Ptr (FFI.Ptr RustExternalChainParameters) ->
        IO FFI.Word8

instance (BlobStore.MonadBlobStore m) => BlobStore.BlobStorable m ForeignExternalChainParametersPtr where
    load = do
        blobRef <- S.get
        pure $! do
            loadCallback <- fst <$> BlobStore.getCallbacks
            liftIO $! do
                FFI.alloca $ \paramsDestPtr -> do
                    status <- ffiLoadExternalChainParameters loadCallback blobRef p11ProtocolVersion paramsDestPtr
                    Monad.unless (status == 0) $ error "Unexpected panic when loading external chain parameters"
                    params <- FFI.peek paramsDestPtr
                    wrapFFIPtr params
    storeUpdate params = do
        storeCallback <- snd <$> BlobStore.getCallbacks
        blobRef <- liftIO $ FFI.alloca $ \blobRefDestPtr -> do
            status <- withExternalChainParameters params $ ffiStoreExternalChainParameters storeCallback blobRefDestPtr
            Monad.unless (status == 0) $ error "Unexpected panic when storing external chain parameters"
            BlobStore.BlobRef @RustExternalChainParameters <$> FFI.peek blobRefDestPtr
        return (S.put blobRef, params)

foreign import ccall "ffi_load_external_chain_parameters"
    ffiLoadExternalChainParameters ::
        FFI.LoadCallback ->
        BlobStore.BlobRef RustExternalChainParameters ->
        FFI.Word64 ->
        FFI.Ptr (FFI.Ptr RustExternalChainParameters) ->
        IO FFI.Word8

foreign import ccall "ffi_store_external_chain_parameters"
    ffiStoreExternalChainParameters ::
        FFI.StoreCallback ->
        FFI.Ptr FFI.Word64 ->
        FFI.Ptr RustExternalChainParameters ->
        IO FFI.Word8

instance (BlobStore.MonadBlobStore m) => BlobStore.Cacheable m ForeignExternalChainParametersPtr where
    cache params = do
        loadCallback <- fst <$> BlobStore.getCallbacks
        status <- liftIO $! withExternalChainParameters params (ffiCacheExternalChainParameters loadCallback)
        Monad.unless (status == 0) $ error "Unexpected panic when caching external chain parameters"
        return params

foreign import ccall "ffi_cache_external_chain_parameters"
    ffiCacheExternalChainParameters ::
        FFI.LoadCallback ->
        FFI.Ptr RustExternalChainParameters ->
        IO FFI.Word8

-- | The hash of external chain parameters.
newtype ExternalChainParametersHash = ExternalChainParametersHash {theExternalChainParametersHash :: SHA256.Hash}
    deriving newtype (Eq, Ord, Show, S.Serialize)

instance (BlobStore.MonadBlobStore m) => Hashable.MHashableTo m ExternalChainParametersHash ForeignExternalChainParametersPtr where
    getHashM params = do
        loadCallback <- fst <$> BlobStore.getCallbacks
        ((), hash) <-
            liftIO $
                withExternalChainParameters params $ \paramsPtr ->
                    FixedByteString.createWith $ \hashDestPtr -> do
                        status <- ffiHashExternalChainParameters loadCallback paramsPtr hashDestPtr
                        Monad.unless (status == 0) $ error "Unexpected panic when hashing external chain parameters"
        return $ ExternalChainParametersHash (SHA256.Hash hash)

foreign import ccall "ffi_hash_external_chain_parameters"
    ffiHashExternalChainParameters ::
        FFI.LoadCallback ->
        FFI.Ptr RustExternalChainParameters ->
        FFI.Ptr FFI.Word8 ->
        IO FFI.Word8

-- | Read the current maximum lock duration from external chain parameters.
getMaxLockDuration :: ForeignExternalChainParametersPtr -> IO Duration
getMaxLockDuration params =
    withExternalChainParameters params $ \paramsPtr ->
        FFI.alloca $ \durationPtr -> do
            status <- ffiGetExternalChainParametersMaxLockDuration paramsPtr durationPtr
            Monad.unless (status == 0) $ error "Unexpected panic when reading max lock duration"
            Duration <$> FFI.peek durationPtr

foreign import ccall "ffi_get_external_chain_parameters_max_lock_duration"
    ffiGetExternalChainParametersMaxLockDuration ::
        FFI.Ptr RustExternalChainParameters ->
        FFI.Ptr FFI.Word64 ->
        IO FFI.Word8
