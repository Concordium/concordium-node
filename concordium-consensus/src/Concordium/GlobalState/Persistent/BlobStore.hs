{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- |
--    Module      : Concordium.GlobalState.Persistent.BlobStore
--    Description : A generic storage implementation using fixed points of functors
--
--    An implementation of a generic storage interface using fixed points of functors,
--    inspired by this paper: https://www.andres-loeh.de/GenericStorage/wgp10-genstorage.pdf
--
--    This module provides a `BlobStore` type that represents the handle used for
--    reading and writing into the store that is managed using the `MonadBlobStore` typeclass.
--    Values are storable if they are instances of `BlobStorable` and they can be stored
--    on references of various kinds.
--
--    Simple references (`BufferedRef`) and fixed point references (`BufferedBlobbed`) are
--    provided, the latter ones requiring to be used together with a Functor that will
--    instantiate the recursive data type definition.
module Concordium.GlobalState.Persistent.BlobStore(
    -- * Blob store
    BlobRef(..),
    BlobHandle,
    BlobStoreAccess,
    BlobStore(..),
    HasBlobStore(..),
    createBlobStore,
    loadBlobStore,
    closeBlobStore,
    destroyBlobStore,
    runBlobStoreTemp,
    truncateBlobStore,
    isValidBlobRef,
    BlobPtr(..),
    MonadBlobStore(..),
    BlobStoreT(..),
    alterBlobStoreT,
    BlobStoreM',
    BlobStoreM,
    runBlobStoreM,
    SupportMigration,
    -- * Storage classes
    -- $storageClasses
    BlobStorable(..),
    storeRef,
    storeUpdateRef,
    loadRef,
    DirectBlobStorable(..),
    -- * Nullable
    Nullable(..),
    HasNull(..),
    -- * Reference types
    Reference(..),
    migrateReference,
    -- ** 'BufferedRef'
    BufferedRef,
    makeBufferedRef,
    blobRefToBufferedRef,
    loadBufferedRef,
    cacheBufferedRef,
    flushBufferedRef,
    uncacheBufferedRef,
    -- ** 'EagerBufferedRef'
    EagerBufferedRef,
    eagerBufferedDeref,
    eagerBufferedRefFromBufferedRef,
    migrateEagerBufferedRef,
    -- ** 'HashedBufferedRef'
    HashedBufferedRef',
    HashedBufferedRef,
    bufferHashed,
    makeHashedBufferedRef,
    migrateHashedBufferedRef,
    migrateHashedBufferedRefKeepHash,
    HashedBufferedRefForCPV1,
    -- ** 'EagerlyHashedBufferedRef'
    EagerlyHashedBufferedRef',
    EagerlyHashedBufferedRef,
    migrateEagerlyHashedBufferedRefKeepHash,
    -- ** 'UnbufferedRef'
    UnbufferedRef,
    makeUnbufferedRef,
    makeFlushedUnbufferedRef,
    blobRefToUnbufferedRef,
    -- * Fixpoint references
    BufferedFix(..),
    UnbufferedFix(..),
    FixShowable(..),
    StoreSerialized(..),
    -- * Caching
    Cacheable(..),
    Cacheable1(..),
) where

import Control.Concurrent.MVar
import System.IO
import Data.Kind (Type)
import Data.Serialize
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSInternal
import qualified Data.ByteString.Unsafe as BSUnsafe
import Control.Exception
import Data.Functor.Foldable
import Control.Monad
import qualified Control.Monad.Catch as MonadCatch
import Control.Monad.Reader.Class
import Control.Monad.Trans.Writer.Strict (WriterT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans
import System.Directory
import Data.IORef
import Concordium.Crypto.EncryptedTransfers
import Data.Map (Map)
import Foreign.Ptr
import Foreign.ForeignPtr (finalizeForeignPtr)
import System.IO.MMap

import Concordium.GlobalState.Persistent.MonadicRecursive
import Concordium.GlobalState.ContractStateFFIHelpers

-- Imports for providing instances
import Concordium.GlobalState.Account
import qualified Concordium.Types.IdentityProviders as IPS
import qualified Concordium.Types.AnonymityRevokers as ARS
import qualified Concordium.GlobalState.Parameters as Parameters
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.Basic.BlockState.PoolRewards
import Concordium.GlobalState.CapitalDistribution
import Concordium.Logger (MonadLogger)
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Updates
import Concordium.Wasm

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo


-- | A @BlobRef a@ represents an offset on a file, at
-- which a value of type @a@ is stored.
newtype BlobRef a = BlobRef { theBlobRef :: Word64 }
    deriving (Eq, Ord, Serialize)

instance Show (BlobRef a) where
    show (BlobRef v) = '@' : show v

-- | The handler for the BlobStore file
data BlobHandle = BlobHandle{
  -- |File handle that should be opened in read/write mode.
  bhHandle :: !Handle,
  -- |Whether we are already at the end of the file, to avoid the need to seek on writes.
  bhAtEnd :: !Bool,
  -- |Current size of the file.
  bhSize :: !Int
  }

-- | The storage context
data BlobStoreAccess = BlobStoreAccess {
    blobStoreFile :: !(MVar BlobHandle),
    blobStoreFilePath :: !FilePath,
    -- |The blob store file memory-mapped into a (read-only) 'ByteString'.
    -- At any given time, this may be smaller than the file, requiring remapping to read later
    -- parts. It will always map from the start of the file.
    blobStoreMMap :: !(IORef BS.ByteString)
}

-- |Context needed to operate on the blob store.
data BlobStore = BlobStore {
  -- |A handle to the underlying storage.
  bscBlobStore :: !BlobStoreAccess,
  -- |Callbacks for loading parts of the state. This is needed by V1 contract
  -- state implementation.
  bscLoadCallback :: !LoadCallback,
  -- |Callbacks for storing new state. This is needed by V1 contract state
  -- implementation.
  bscStoreCallback :: !StoreCallback
  }

class HasBlobStore a where
    -- |A handle to access the underlying storage.
    blobStore :: a -> BlobStoreAccess
    -- |Callbacks for loading parts of the state. This is needed by V1 contract
    -- state implementation, but should otherwise not be used by Haskell code directly.
    blobLoadCallback :: a -> LoadCallback
    -- |Callbacks for storing new state. This is needed by V1 contract state
    -- implementation, but should otherwise not be used by Haskell code directly.
    blobStoreCallback :: a -> StoreCallback

-- |Construct callbacks for accessing the blob store.
-- These callbacks must be freed in order that memory is not leaked.
mkCallbacksFromBlobStore :: BlobStoreAccess -> IO (LoadCallback, StoreCallback)
mkCallbacksFromBlobStore bstore = do
    storeCallback <- createStoreCallback (\ptr size -> theBlobRef <$> (writeBlobBS bstore =<< BSUnsafe.unsafePackCStringLen (castPtr ptr, fromIntegral size)))
    loadCallback <- createLoadCallback $ \location -> do
        bs <- readBlobBS bstore (BlobRef location)
        BSUnsafe.unsafeUseAsCStringLen bs $ \(sourcePtr, len) -> 
          copyToRustVec (castPtr sourcePtr) (fromIntegral len)
    return (loadCallback, storeCallback)

-- |Free callbacks constructed with 'mkCallbacksFromBlobStore'. This can only be
-- called once for each constructed callback.
freeCallbacks :: LoadCallback -> StoreCallback -> IO ()
freeCallbacks fp1 fp2 = do
    freeHaskellFunPtr fp1
    freeHaskellFunPtr fp2

-- |Create a new blob store at a given location.
-- Fails if a file or directory at that location already exists.
createBlobStore :: FilePath -> IO BlobStore
createBlobStore blobStoreFilePath = do
    pathEx <- doesPathExist blobStoreFilePath
    when pathEx $ throwIO (userError $ "Blob store path already exists: " ++ blobStoreFilePath)
    bhHandle <- openBinaryFile blobStoreFilePath ReadWriteMode
    blobStoreFile <- newMVar BlobHandle{bhSize=0, bhAtEnd=True,..}
    blobStoreMMap <- newIORef BS.empty
    let bscBlobStore = BlobStoreAccess{..}
    (bscLoadCallback, bscStoreCallback) <- mkCallbacksFromBlobStore bscBlobStore
    return BlobStore{..}

-- |Load an existing blob store from a file.
-- The file must be readable and writable, but this is not checked here.
loadBlobStore :: FilePath -> IO BlobStore
loadBlobStore blobStoreFilePath = do
  bhHandle <- openBinaryFile blobStoreFilePath ReadWriteMode
  bhSize <- fromIntegral <$> hFileSize bhHandle
  blobStoreFile <- newMVar BlobHandle{bhAtEnd=bhSize==0,..}
  blobStoreMMap <- newIORef =<< mmapFileByteString blobStoreFilePath Nothing
  let bscBlobStore = BlobStoreAccess{..}
  (bscLoadCallback, bscStoreCallback) <- mkCallbacksFromBlobStore bscBlobStore
  return BlobStore{..}

-- |Flush all buffers associated with the blob store,
-- ensuring all the contents is written out.
flushBlobStore :: BlobStoreAccess -> IO ()
flushBlobStore BlobStoreAccess{..} =
    withMVar blobStoreFile (hFlush . bhHandle)

-- |Close all references to the blob store, flushing it
-- in the process.
closeBlobStore :: BlobStore -> IO ()
closeBlobStore BlobStore{..} = do
    BlobHandle{..} <- takeMVar (blobStoreFile bscBlobStore)
    writeIORef (blobStoreMMap bscBlobStore) BS.empty
    hClose bhHandle
    freeCallbacks bscLoadCallback bscStoreCallback

-- |Close all references to the blob store and delete the backing file.
destroyBlobStore :: BlobStore -> IO ()
destroyBlobStore bs@BlobStore{..} = do
    closeBlobStore bs
    -- Removing the file may fail (e.g. on Windows) if the handle is kept open.
    -- This could be due to the finalizer on the memory map not being run, but my attempts
    -- to force it have not proved successful.
    removeFile (blobStoreFilePath bscBlobStore) `catch` (\(_ :: IOException) -> return ())

-- |Run a computation with temporary access to the blob store.
-- The given FilePath is a directory where the temporary blob
-- store will be created.
-- The blob store file is deleted afterwards.
runBlobStoreTemp :: FilePath -> BlobStoreM a -> IO a
runBlobStoreTemp dir a = bracket openf closef usef
    where
        openf = openBinaryTempFile dir "blb.dat"
        closef (tempFP, h) = do
            hClose h
            removeFile tempFP
        usef (fp, h) = do
            mv <- newMVar (BlobHandle h True 0)
            mmap <- newIORef BS.empty
            let bscBlobStore = BlobStoreAccess mv fp mmap
            (bscLoadCallback, bscStoreCallback) <- mkCallbacksFromBlobStore bscBlobStore
            res <- runBlobStoreM a BlobStore{..}
            _ <- takeMVar mv
            freeCallbacks bscLoadCallback bscStoreCallback
            return res

-- | Truncate the blob store after the blob stored at the given offset. The blob should not be
-- corrupted (i.e., its size header should be readable, and its size should match the size header).
--
-- **Note**: This function must not be called after any writes to the blob store. It assumes that
-- the current 'blobStoreMMap' is the only mapping of the file (which can be relied on if no writes
-- have occurred). Since the existing memory map is invalidated, any other references to it will
-- also be invalidated, such as 'BS.ByteString's returned by 'loadBlobPtr'.
truncateBlobStore :: BlobStoreAccess -> BlobRef a -> IO ()
truncateBlobStore BlobStoreAccess{..} (BlobRef offset) = do
  bh@BlobHandle{..} <- takeMVar blobStoreFile
  eres <- try $ do
    hSeek bhHandle AbsoluteSeek (fromIntegral offset)
    esize <- decode <$> BS.hGet bhHandle 8
    case esize :: Either String Word64 of
      Right size -> do
        let newSize = offset + 8 + size
        unless (bhSize == fromIntegral newSize) $ do
          -- unmap the current memory mapped file since on some platforms the file cannot be
          -- truncated if it is memory mapped.
          oldMmap <- readIORef blobStoreMMap
          -- write a temporary empty string.
          writeIORef blobStoreMMap BS.empty
          let (fp, _, _) = BSInternal.toForeignPtr oldMmap
          -- unmap the old mapping.
          finalizeForeignPtr fp
          -- truncate the file
          hSetFileSize bhHandle $ fromIntegral newSize
          -- and map it again.
          mmapFileByteString blobStoreFilePath Nothing >>= writeIORef blobStoreMMap
        putMVar blobStoreFile bh{bhSize = fromIntegral newSize, bhAtEnd=False}
      _ -> throwIO $ userError "Cannot truncate the blob store: cannot obtain the last blob size"
  case eres :: Either SomeException () of
    Left e -> do
      putMVar blobStoreFile bh
      throwIO e
    Right () -> return ()

-- | Read a bytestring from the blob store at the given offset using the file handle.
readBlobBSFromHandle :: BlobStoreAccess -> BlobRef a -> IO BS.ByteString
readBlobBSFromHandle BlobStoreAccess{..} (BlobRef offset) = mask $ \restore -> do
        bh@BlobHandle{..} <- takeMVar blobStoreFile
        eres <- try $ restore $ do
            hSeek bhHandle AbsoluteSeek (fromIntegral offset)
            esize <- decode <$> BS.hGet bhHandle 8
            case esize :: Either String Word64 of
                -- there should be at least `size` bytes left to read after `offset + 8`, where 8 is
                -- the size of the blob size header
                Right size | offset + 8 + size <= fromIntegral bhSize ->
                             BS.hGet bhHandle (fromIntegral size)
                _ ->  throwIO $ userError "Attempted to read beyond the blob store end"
        putMVar blobStoreFile bh{bhAtEnd=False}
        case eres :: Either SomeException BS.ByteString of
            Left e -> throwIO e
            Right bs -> return bs

-- | Read a bytestring from the blob store at the given offset using the memory map.
-- The file handle is used as a backstop if the data to be read would be outside the memory map
-- even after re-mapping.
readBlobBS :: BlobStoreAccess -> BlobRef a -> IO BS.ByteString
readBlobBS bs@BlobStoreAccess{..} br@(BlobRef offset) = do
        let ioffset = fromIntegral offset
        let dataOffset = ioffset + 8
        mmap0 <- readIORef blobStoreMMap
        mmap <- if dataOffset > BS.length mmap0 then do
                -- Remap the file
                mmap <- mmapFileByteString blobStoreFilePath Nothing
                writeIORef blobStoreMMap mmap
                return mmap
            else return mmap0
        let mmapLength = BS.length mmap
        if dataOffset > mmapLength then
            readBlobBSFromHandle bs br
        else do
            let lengthStart = BS.drop ioffset mmap
            case decode (BS.take 8 lengthStart) of
                Left e -> error e
                Right (size :: Word64) -> do
                    if dataOffset + fromIntegral size > mmapLength then
                        readBlobBSFromHandle bs br
                    else
                        return
                            $ BS.take (fromIntegral size)
                            $ BS.drop dataOffset mmap

-- |Check if a given 'BlobRef' is valid in the blob store. This attempts to
-- load the value from the given location and handles the following two failures
-- - there is not enough data to read from the blob store, i.e., the file ends prematurely
-- - the value cannot be loaded/deserialized.
--
-- These two together handle the common case of corruption where data at the end
-- of the blob store is not written properly. The second test is needed since at
-- least on some platforms the blob store sometimes ends up with enough trailing
-- zeros that the first check above succeeds, but those zeros are not valid
-- data. This happens if the node is killed at the right time.
isValidBlobRef :: (MonadCatch.MonadCatch m, BlobStorable m a) => BlobRef a -> m Bool
isValidBlobRef br = (True <$ loadRef br) `MonadCatch.catch` (\(_ :: SomeException) -> return False)

-- | Write a bytestring into the blob store and return the offset
writeBlobBS :: BlobStoreAccess -> BS.ByteString -> IO (BlobRef a)
writeBlobBS BlobStoreAccess{..} bs = mask $ \restore -> do
        bh@BlobHandle{bhHandle=writeHandle,bhAtEnd=atEnd} <- takeMVar blobStoreFile
        eres <- try $ restore $ do
            unless atEnd (hSeek writeHandle SeekFromEnd 0)
            BS.hPut writeHandle size
            BS.hPut writeHandle bs
        case eres :: Either SomeException () of
            Left e -> do
                -- In case of an exception, query for the size and assume we are not at the end.
                fSize <- hFileSize writeHandle
                putMVar blobStoreFile bh{bhSize = fromInteger fSize, bhAtEnd=False}
                throwIO e
            Right _ -> do
                putMVar blobStoreFile bh{bhSize = bhSize bh + 8 + BS.length bs, bhAtEnd=True}
                return (BlobRef (fromIntegral (bhSize bh)))
    where
        size = encode (fromIntegral (BS.length bs) :: Word64)

-- |A pointer into the blob store that can be used for direct memory access.
-- In particular the 'BlobPtr' can be used for 'custom' access into the 'BlobStore'
-- as it does not depend on serialization of @a@, instead it is used for retrieving
-- a raw 'ByteString' from the 'BlobStore'.
--
-- This is used for providing the Rust side with raw bytes of an 'Artifact' without having to
-- deserialize it beforehand.
data BlobPtr a = BlobPtr {
    theBlobPtr :: !Word64,
    -- ^The offset of @a@ in the 'BlobStore'.
    blobPtrLen :: !Word64
    -- ^The length to read from the offset.
  }
    deriving (Eq, Show)

-- | Read a bytestring from the blob store at the given offset and length using the file handle.
readBlobPtrBSFromHandle :: BlobStoreAccess -> BlobPtr a -> IO BS.ByteString
readBlobPtrBSFromHandle BlobStoreAccess{..} BlobPtr{..} = mask $ \restore -> do
        bh@BlobHandle{..} <- takeMVar blobStoreFile
        eres <- try $ restore $ do
            hSeek bhHandle AbsoluteSeek (fromIntegral theBlobPtr)
            BS.hGet bhHandle (fromIntegral blobPtrLen)
        putMVar blobStoreFile bh{bhAtEnd=False}
        case eres :: Either SomeException BS.ByteString of
            Left e -> throwIO e
            Right bs -> return bs

-- | Read a bytestring from the blob store at the given offset and length using the memory map.
-- The file handle is used as a backstop if the data to be read would be outside the memory map
-- even after re-mapping.
readBlobPtrBS :: BlobStoreAccess -> BlobPtr a -> IO BS.ByteString
readBlobPtrBS bs@BlobStoreAccess{..} bptr@BlobPtr{..} = do
        let ptrEnd = fromIntegral $ theBlobPtr + blobPtrLen
        mmap0 <- readIORef blobStoreMMap
        mmap <- if ptrEnd > BS.length mmap0 then do
                -- Remap the file
                mmap <- mmapFileByteString blobStoreFilePath Nothing
                writeIORef blobStoreMMap mmap
                return mmap
            else return mmap0
        let mmapLength = BS.length mmap
        if ptrEnd > mmapLength then
            readBlobPtrBSFromHandle bs bptr
        else 
            return $! BS.take (fromIntegral blobPtrLen) (BS.drop (fromIntegral theBlobPtr) mmap)

-- |Typeclass for a monad to be equipped with a blob store.
-- This allows a 'BS.ByteString' to be written to the store,
-- obtaining a 'BlobRef', and a 'BlobRef' to be read back as
-- a 'BS.ByteStrings'.
--
-- Default implementations are provided for a monad @m@ that
-- is a reader monad (@MonadReader r m@) for a type @r@
-- that can be projected to a 'BlobStore' (@HasBlobStore r@).
class MonadIO m => MonadBlobStore m where
    -- |Store a 'BS.ByteString' and return a reference to it.
    storeRaw :: BS.ByteString -> m (BlobRef a)
    default storeRaw :: (MonadReader r m, HasBlobStore r) => BS.ByteString -> m (BlobRef a)
    storeRaw b = do
        bs <- blobStore <$> ask
        liftIO $ writeBlobBS bs b
    -- |Load a 'BS.ByteString' from a reference.
    loadRaw :: BlobRef a -> m BS.ByteString
    default loadRaw :: (MonadReader r m, HasBlobStore r) => BlobRef a -> m BS.ByteString
    loadRaw r = do
        bs <- blobStore <$> ask
        liftIO $ readBlobBS bs r
    -- |Flush all writes to disk. This should ensure synchronization:
    -- any 'BlobRef's that are stored before a call to @flushStore@
    -- should be reliably written, and available if the file is
    -- subsequently loaded.
    flushStore :: m ()
    default flushStore :: (MonadReader r m, HasBlobStore r) => m ()
    flushStore = do
        bs <- blobStore <$> ask
        liftIO $ flushBlobStore bs

    -- |Get callbacks that can be given to foreign code (i.e., passed via FFI)
    -- to access the blob store.
    getCallbacks :: m (LoadCallback, StoreCallback)
    default getCallbacks :: (MonadReader r m, HasBlobStore r) => m (LoadCallback, StoreCallback)
    getCallbacks = do
      r <- ask
      return (blobLoadCallback r, blobStoreCallback r)

    -- |Access a blob pointer directly. The function should ONLY READ the memory at the pointer,
    -- and not read beyond the length of the 'BlobPtr'.
    withBlobPtr :: BlobPtr a -> (Ptr a -> IO b) -> m b
    withBlobPtr bptr f = do
        bytes <- loadBlobPtr bptr
        liftIO $ BSUnsafe.unsafeUseAsCString bytes (f . castPtr)

    -- |Access a blob pointer directly as a 'BS.ByteString'.
    loadBlobPtr :: BlobPtr a -> m BS.ByteString
    default loadBlobPtr :: (MonadReader r m, HasBlobStore r) => BlobPtr a -> m BS.ByteString
    loadBlobPtr bptr = do
        bs <- blobStore <$> ask
        liftIO $ readBlobPtrBS bs bptr

    {-# INLINE storeRaw #-}
    {-# INLINE loadRaw #-}
    {-# INLINE flushStore #-}

instance HasBlobStore BlobStore where
  blobStore = bscBlobStore
  blobLoadCallback = bscLoadCallback
  blobStoreCallback = bscStoreCallback

-- |An auxiliary constraint needed by all functions that migrate state from one
-- blob store to another. The intended reading of this is that @m@ and @t@
-- support migration from the context @m@ to the context @t m@. The context in
-- this case is essentially access to a block state database.
type SupportMigration m t = (MonadBlobStore m, MonadTrans t, MonadBlobStore (t m))

-- |A monad transformer that is equivalent to 'ReaderT' but provides a 'MonadBlobStore' instance
-- based on the context (rather than lifting).
newtype BlobStoreT r m a = BlobStoreT {runBlobStoreT :: r -> m a}
    deriving
        (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadFail, MonadLogger, MonadCatch.MonadThrow, MonadCatch.MonadCatch)
        via (ReaderT r m)
    deriving
        (MonadTrans)
        via (ReaderT r)

instance (HasBlobStore r, MonadIO m) => MonadBlobStore (BlobStoreT r m)

-- |Apply a given function to modify the context of a 'BlobStoreT' operation.
alterBlobStoreT :: (r1 -> r2) -> BlobStoreT r2 m a -> BlobStoreT r1 m a
alterBlobStoreT f (BlobStoreT a) = BlobStoreT (a . f)

-- |A simple monad implementing 'MonadBlobStore' that is equivalent to
-- @ReaderT r IO@.
type BlobStoreM' r = BlobStoreT r IO

-- |A simple monad implementing 'MonadBlobStore' that is equivalent to
-- @ReaderT BlobStore IO@.
type BlobStoreM = BlobStoreM' BlobStore

-- |Run a 'BlobStoreM'' operation with the supplied context.
runBlobStoreM :: BlobStoreM' r a -> r -> IO a
runBlobStoreM = runBlobStoreT

-- |A wrapper type for lifting 'MonadBlobStore' instances over monad transformers.
-- Lifted instances are provided for 'WriterT', 'StateT' and 'ExceptT', which are used for
-- conveniently implementing some block state operations.
newtype LiftMonadBlobStore t (m :: Type -> Type) a = LiftMonadBlobStore (t m a)
    deriving (MonadTrans, Functor, Applicative, Monad, MonadIO)

instance (MonadTrans t, MonadBlobStore m, MonadIO (t m)) => MonadBlobStore (LiftMonadBlobStore t m) where
    storeRaw = lift . storeRaw
    {-# INLINE storeRaw #-}
    loadRaw = lift . loadRaw
    {-# INLINE loadRaw #-}
    flushStore = lift flushStore
    {-# INLINE flushStore #-}
    getCallbacks = lift getCallbacks
    {-# INLINE getCallbacks #-}
    withBlobPtr bp = lift . withBlobPtr bp
    {-# INLINE withBlobPtr #-}
    loadBlobPtr = lift . loadBlobPtr
    {-# INLINE loadBlobPtr #-}

deriving via (LiftMonadBlobStore (WriterT w) m)
    instance (Monoid w, MonadBlobStore m) => MonadBlobStore (WriterT w m)

deriving via (LiftMonadBlobStore (StateT s) m)
    instance MonadBlobStore m => MonadBlobStore (StateT s m)

deriving via (LiftMonadBlobStore (ExceptT e) m)
    instance MonadBlobStore m => MonadBlobStore (ExceptT e m)

-- |This instance lifts a 'MonadBlobStore' over a 'ReaderT' transformer.
-- This is used to implement a 'Cacheable' instance for the persistent
-- 'Concordium.GlobalState.Persistent.Instances' type, which requires
-- makes use of a context to achieve sharing.
deriving via (LiftMonadBlobStore (ReaderT r) m)
    instance MonadBlobStore m => MonadBlobStore (ReaderT r m)

-- * Storage classes
--
-- $storageClasses
-- For a type to be storable in the blob store, it must define exactly how it is stored.
-- Ultimately, this comes down to having a 'DirectBlobStorable' instance for the type.
-- However, an implementation can instead provide a 'BlobStorable' instance from which a
-- 'DirectBlobStorable' instance is automatically provided.  For types that implement
-- 'Serialize', the 'BlobStorable' class provides default implementations of the functions.
--
-- Although not explicit in the class constraints, there is an implicit hierarchy between
-- 'DirectBlobStorable' (most general), 'BlobStorable' and 'Serializable' (most specific).
--
-- * A 'Serializable' type can be stored by simply serializing it to a flat 'ByteString'.
--
-- * A 'BlobStorable' type can be stored by serializing it with possible side-effects that
--   can involve storing components. This is used for complex or nested structures, and
--   allows for sharing between stored values.
--
-- * A 'DirectBlobStorable' type provides direct operations for its storage in the blob store.
--   This allows the most flexibility of implementation, and in particular is used to
--   implement types that may maintain 'BlobPtr's that allow for direct data access in the blob
--   store.
--
-- Note that a 'BlobRef' is expected to be an offset in the blob store to a length (64-bit, big
-- endian) followed by a byte-string (of the given length) that represents the value under the
-- reference.  This is implicitly the case for types that implement 'BlobStorable', however,
-- it could be violated by implementations of 'DirectBlobStorable'.
--
-- Where possible, the 'DirectBlobStorable' constraint should be used over 'BlobStorable', and
-- conversely 'BlobStorable' should be implemented in preference to 'DirectBlobStorable'.

-- |The @BlobStorable m a@ class defines how a value
-- of type @a@ may be stored in monad @m@.
--
-- Where @a@ is an instance of 'Serialize', default implementations
-- are provided for 'store' and 'load' that simply (de)serialize
-- the value.  For a complex datatype that uses internal pointers,
-- 'store' and 'load' are expected to translate between such pointers
-- and references in the underlying store.
--
-- Note that the functions `store` and `load` are somewhat equivalent to
-- `put` and `get` but working on references so that they can be written
-- to the disk.
class MonadBlobStore m => BlobStorable m a where
    -- |Deserialize a value of type @a@ from storage.
    load :: Get (m a)
    default load :: (Serialize a) => Get (m a)
    load = pure <$> get
    -- |Store a value of type @a@, possibly updating its representation.
    -- This is used when the value's representation includes pointers that
    -- may be replaced or supplemented with blob references.
    storeUpdate :: a -> m (Put, a)
    default storeUpdate :: (Serialize a) => a -> m (Put, a)
    storeUpdate v = return (put v, v)
    {-# INLINE load #-}
    {-# INLINE storeUpdate #-}

-- |Store a value in the blob store and return a reference to it.
storeRef :: BlobStorable m a => a -> m (BlobRef a)
storeRef v = do
    p <- runPut . fst <$> storeUpdate v
    storeRaw p
{-# INLINE storeRef #-}

-- |Store a value in the blob store, returning a reference to it and
-- an updated value.  (See 'storeUpdate'.)
storeUpdateRef :: BlobStorable m a => a -> m (BlobRef a, a)
storeUpdateRef v = do
    (!p, !v') <- storeUpdate v
    (, v') <$> storeRaw (runPut p)
{-# INLINE storeUpdateRef #-}

-- |Load a value from a reference.
loadRef :: (BlobStorable m a) => BlobRef a -> m a
loadRef ref = do
    bs <- loadRaw ref
    case runGet load bs of
        Left e -> liftIO (throwIO (userError (e ++ " :: " ++ show bs)))
        Right !mv -> mv
{-# INLINE loadRef #-}

-- |The @DirectBlobStorable m a@ class defines how a value
-- of type @a@ may be stored directly in monad @m@.
--
-- The 'DirectBlobStorable' interface allows for more direct control of how
-- data is stored and loaded from the blob store than does 'BlobStorable'.
-- It is expected that this class should only be implemented directly where
-- implementing 'BlobStorable' instead would not be possible.
--
-- A blanket, overlappable instance of 'DirectBlobStorable' is provided for
-- types that implement 'BlobStorable'.  Thus, if both 'BlobStorable' and
-- 'DirectBlobStorable' are implemented for a type, then it MUST be that:
--
-- * 'storeUpdateDirect' is functionally equivalent to 'storeUpdateRef'; and
--
-- * 'loadDirect' is functionally equivalent to 'loadRef'.
class MonadBlobStore m => DirectBlobStorable m a where
    -- |Store a value of type @a@, possibly updating its representation.
    storeUpdateDirect :: a -> m (BlobRef a, a)
    -- |Load a value of type @a@ from the underlying storage.
    loadDirect :: BlobRef a -> m a

instance {-# OVERLAPPABLE #-} (MonadBlobStore m, BlobStorable m a) => DirectBlobStorable m a where
    storeUpdateDirect = storeUpdateRef
    loadDirect = loadRef
    {-# INLINE storeUpdateDirect #-}
    {-# INLINE loadDirect #-}

instance (BlobStorable m a, BlobStorable m b) => BlobStorable m (a, b) where

  storeUpdate (a, b) = do
    (!pa, !a') <- storeUpdate a
    (!pb, !b') <- storeUpdate b
    let pab = pa >> pb
    return (pab, (a', b'))

  load = do
    ma <- load
    mb <- load
    return $ do
      a <- ma
      b <- mb
      return (a, b)
  {-# INLINE load #-}
  {-# INLINE storeUpdate #-}

-- | A value that can be empty or contain another value. It is equivalent to `Maybe` but
-- strict on its constructors and its `Serialize` instance depends on the inner type having
-- a special @null@ value.
data Nullable v = Null | Some !v
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A type that is an instance of @HasNull@ has a distinguished value that is considered a Null value.
class HasNull ref where
    refNull :: ref
    isNull :: ref -> Bool
    isNotNull :: ref -> Bool
    isNotNull = not . isNull

instance HasNull (BlobRef a) where
    refNull = BlobRef maxBound
    isNull = (== refNull)

instance HasNull (Nullable a) where
    refNull = Null
    isNull Null = True
    isNull _ = False
    isNotNull Null = False
    isNotNull _ = True

-- | Serialization is equivalent to that of the @ref@ as there
-- is a special value for a null reference, i.e. @ref@ is @HasNull@
instance (HasNull ref, Serialize ref) => Serialize (Nullable ref) where
  put Null = put (refNull :: ref)
  put (Some v) = put v
  get = do
      r <- get
      return $! if isNull r then Null else Some r

instance MonadBlobStore m => BlobStorable m (Nullable (BlobRef a))

instance MonadBlobStore m => BlobStorable m (BlobRef a)

-- This instance has to follow the instance for HashableTo H.Hash (Maybe v), see
-- Concordium.Types.HashableTo
instance MHashableTo m H.Hash v => MHashableTo m H.Hash (Nullable v) where
  getHashM Null = return $ H.hash "Nothing"
  getHashM (Some v) = (\h -> H.hash ("Just" <> H.hashToByteString h)) <$> getHashM v

-- NOTE: As we have several "simple" reference types, we need a way to abstract over them. This is the purpose of the @Reference@ class.
-- | An instance @Reference m ref a@ specifies how a value of type @a@ can be stored and retrieved over a reference type
-- @ref@ in the monad @m@. The constraints on this typeclass are specially permissive and it is responsibility of the
-- instances to refine those. This typeclass is specifically designed to be used by BufferedRef and HashedBufferedRef.
class Monad m => Reference m ref a where
  -- |Given a reference, write it to the disk and return the updated reference and the generated offset in the store
  refFlush :: ref a -> m (ref a, BlobRef a)
  -- |Given a reference, read the value and return the possibly updated reference (that now holds the value in memory)
  refCache :: ref a -> m (a, ref a)
  -- |Read the value from a given reference either accessing the store or returning it from memory.
  refLoad :: ref a -> m a
  -- |Create a reference to a value. This does not guarantee that the value will be written to the store, and most probably
  -- it will just be stored in memory as cached.
  refMake :: a -> m (ref a)
  -- |Given a reference, flush the data and return an uncached reference.
  refUncache :: ref a -> m (ref a)

-- |Migrate a reference from one context to another, using the provided callback
-- to migrate the value. This is a general construction that applies to all
-- references, but it might not be the most efficient option. In particular, for
-- hashed references if the hash is unchanged by migration a more efficient
-- implementation is possible that either retains the original hash if this is
-- applicable, or one that computes the hash at the most opportune time, when
-- the value is known to be in memory.
--
-- The old (input) reference is uncached, and the new reference is flushed.
-- Typically this will mean that the value is retained in memory with a pointer
-- to a disk value. But this may be different for some references such as
-- 'CachedRef'.
migrateReference ::
    forall t m ref1 ref2 a b.
    (MonadTrans t, Reference m ref1 a, Reference (t m) ref2 b) =>
    (a -> t m b) ->
    ref1 a ->
    t m (ref2 b)
migrateReference f hb = do
    a <- f =<< lift (refLoad hb)
    newRef <- refMake a
    (!newFlushedRef, _) <- refFlush newRef
    !_ <- lift (refUncache hb)
    return $! newFlushedRef

-- |A value that may exists purely on disk ('BRBlobbed'), purely in memory
-- ('BRMemory' with @brIORef = Null@), or both in memory and on disk. When the
-- value is both on disk and in memory the two values must match.
-- 
-- The 'BRMemory' case uses an 'IORef' that either stores 'Null' if the reference has not already
-- been written to the disk, or @Some BRBoth{..}@ if it has.  When 'storeUpdate' is called, this
-- 'IORef' is checked, and it is used in the @Some@ case; in the @Null@ case, the updated reference
-- (a @BRBoth@) is written into the 'IORef'.  This ensures that if the reference is shared then
-- we: 1) don't write it multiple times; and 2) also share the updated value after it is written.
-- (Note, we could just store the contents of the @BRBoth@ instead of the @BRBoth@ itself in the
-- 'IORef'.  Our implementation choice avoids some extra constructors and definitions, although
-- it does introduce an invariant that is not enforced by the type system itself.  However, this
-- implementation detail should not leak outside the present module.)
--
-- In a previous version, the 'IORef' only held the 'BlobRef', and 'storeUpdate' would use that
-- and the existing value. This was problematic, because in cases of sharing it is important to
-- also share the updated value.  In particular, it caused cases where a module was retained
-- in memory as a result of a shared 'BRMemory' not being updated.  (The module reference is
-- shared by the modules table and instances of the module.  The module table reference would be
-- flushed, and updated with a version that did not retain the module in memory.  However, the
-- instance reference would just be updated with the new 'BlobRef', but keep the old value, which
-- retained the module in memory.)
--
-- An alternative implementation choice would be to have 'BRMemory' always store the value under
-- the 'IORef'.  This approach was not taken in order to avoid the additional indirection this
-- introduces.
--
-- The choice to have 'BRBoth' at all (and not just use 'BRMemory') was to reduce the indirection
-- for the common case where the data is both in memory and on disk.
data BufferedRef a
    = BRBlobbed {brRef :: !(BlobRef a)}
    -- ^Value stored on disk
    | BRMemory {brIORef :: !(IORef (Nullable (BufferedRef a))), brValue :: !a}
    -- ^Value stored in memory and possibly on disk.
    -- 'brIORef' contains 'Null' if the value has never been written to the blob store, and
    -- otherwise @Some BRBoth{..}@ with the reference and updated value.
    | BRBoth {brRef :: !(BlobRef a), brValue :: !a}
    -- ^Value stored in memory and on disk.

-- | Create a @BRMemory@ value with a null reference (so the value is just in memory)
makeBufferedRef :: MonadIO m => a -> m (BufferedRef a)
makeBufferedRef v = liftIO $ do
    ref <- newIORef Null
    return $ BRMemory ref v

-- | Create a 'BufferedRef' from a 'BlobRef', without loading anything.
blobRefToBufferedRef :: BlobRef a -> BufferedRef a
blobRefToBufferedRef = BRBlobbed

instance Show a => Show (BufferedRef a) where
  show (BRBlobbed r) = show r
  show (BRMemory _ v) = "{" ++ show v ++ "}"
  show (BRBoth r v) = "{" ++ show v ++ "}@" ++ show r

instance DirectBlobStorable m a => BlobStorable m (BufferedRef a) where
    load = fmap BRBlobbed <$> load
    storeUpdate (BRMemory ref v) = liftIO (readIORef ref) >>= \case
        Null -> do
            (r' :: BlobRef a, v') <- storeUpdateDirect v
            let br' = BRBoth r' v'
            liftIO . writeIORef ref $! Some br'
            return (put r', br')
        Some br' -> storeUpdate br'
    storeUpdate x = do
        ref <- getBRRef x
        return (put ref, x)

-- |Stores in-memory data to disk if it has not been stored yet and returns pointer to saved data
getBRRef :: DirectBlobStorable m a => BufferedRef a -> m (BlobRef a)
getBRRef (BRMemory ref v) = liftIO (readIORef ref) >>= \case
    Null -> do
        (r' :: BlobRef a, v') <- storeUpdateDirect v
        liftIO . writeIORef ref $! Some (BRBoth r' v')
        return r'
    Some br' -> getBRRef br'
getBRRef (BRBoth r _) = return r
getBRRef (BRBlobbed r) = return r

instance DirectBlobStorable m a => BlobStorable m (Nullable (BufferedRef a)) where
    load = do
        (r :: BlobRef a) <- get
        if isNull r then
            return (pure Null)
        else
            return $ pure $ Some $ BRBlobbed r
    storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
    storeUpdate (Some v) = do
        (!r, !v') <- storeUpdate v
        return (r, Some v')

-- |Load the value from a @BufferedRef@ not caching it.
loadBufferedRef :: DirectBlobStorable m a => BufferedRef a -> m a
loadBufferedRef = refLoad

-- |Load a 'BufferedRef' and cache it if it wasn't already in memory.
cacheBufferedRef :: DirectBlobStorable m a => BufferedRef a -> m (a, BufferedRef a)
cacheBufferedRef = refCache

-- |If given a Blobbed reference, do nothing. Otherwise if needed store the value.
flushBufferedRef :: DirectBlobStorable m a => BufferedRef a -> m (BufferedRef a, BlobRef a)
flushBufferedRef = refFlush

-- |Convert a Cached reference into a Blobbed one storing the data if needed.
uncacheBufferedRef :: DirectBlobStorable m a => BufferedRef a -> m (BufferedRef a)
uncacheBufferedRef = refUncache

instance DirectBlobStorable m a => Reference m BufferedRef a where
  refMake = makeBufferedRef

  refLoad (BRBlobbed ref) = loadDirect ref
  refLoad (BRMemory _ v) = return v
  refLoad (BRBoth _ v) = return v

  refCache (BRBlobbed ref) = do
    v <- loadDirect ref
    return (v, BRBoth ref v)
  refCache r@(BRMemory _ v) = return (v, r)
  refCache r@(BRBoth _ v) = return (v, r)

  refFlush (BRMemory ref v) = liftIO (readIORef ref) >>= \case
    Null -> do
        (!r' :: BlobRef a, !v') <- storeUpdateDirect v
        let br' = BRBoth r' v'
        liftIO . writeIORef ref $! Some br'
        return (br', r')
    Some brm' -> refFlush brm'
  refFlush b = return (b, brRef b)

  refUncache v@(BRMemory _ _) = BRBlobbed <$> getBRRef v
  refUncache (BRBoth r _) = return $ BRBlobbed r
  refUncache b = return b
  {-# INLINE refFlush #-}
  {-# INLINE refLoad #-}
  {-# INLINE refMake #-}
  {-# INLINE refCache #-}
  {-# INLINE refUncache #-}

instance (DirectBlobStorable m a, MHashableTo m h a) => MHashableTo m h (BufferedRef a) where
  getHashM ref = getHashM =<< refLoad ref

instance (DirectBlobStorable m a, BlobStorable m b) => BlobStorable m (Nullable (BufferedRef a, b)) where
  load = do
    (r :: BlobRef a) <- get
    if isNull r
      then return (pure Null)
      else do
        bval <- load
        return $ do
          binner <- bval
          pure $ Some (BRBlobbed r, binner)
  storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
  storeUpdate (Some v) = do
    (!r, !v') <- storeUpdate v
    return (r, Some v')


instance (DirectBlobStorable m a, BlobStorable m b) => BlobStorable m (Nullable (HashedBufferedRef a, b)) where
  load = do
    (r :: BlobRef a) <- get
    if isNull r
      then return (pure Null)
      else do
        bval <- load
        return $ do
          binner <- bval
          hshRef <- liftIO $ newIORef Null
          pure $ Some (HashedBufferedRef (BRBlobbed r) hshRef, binner)
  storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
  storeUpdate (Some v) = do
    (!r, !v') <- storeUpdate v
    return (r, Some v')

-- |A value that always exists in memory but may also exist on disk.
-- The disk reference is shared via an 'IORef', which ensures that copies created before
-- the value is flushed to disk will share the same underlying reference.
data EagerBufferedRef a = EagerBufferedRef
    { ebrIORef :: !(IORef (BlobRef a)),
      ebrValue :: !a
    }

-- |Directly get the value in an 'EagerBufferedRef'.
eagerBufferedDeref :: EagerBufferedRef a -> a
{-# INLINE eagerBufferedDeref #-}
eagerBufferedDeref = ebrValue

-- |Make an 'EagerBufferedRef' from a 'BufferedRef'.
-- Note: if the 'BufferedRef' is not already flushed to the disk, then the association between the
-- old and new reference is lost, so they will not share the same underlying 'BlobRef' once flushed.
eagerBufferedRefFromBufferedRef :: (DirectBlobStorable m a) => BufferedRef a -> m (EagerBufferedRef a)
eagerBufferedRefFromBufferedRef (BRBlobbed r) = do
    v <- loadDirect r
    makeEagerBufferedRef r v
eagerBufferedRefFromBufferedRef (BRMemory ior v) = do
    liftIO (readIORef ior) >>= \case
        Null -> refMake v
        Some br' -> eagerBufferedRefFromBufferedRef br'
eagerBufferedRefFromBufferedRef (BRBoth r v) = makeEagerBufferedRef r v

-- |Migrate the reference from one context to another, using the provided
-- callback to migrate the value.
migrateEagerBufferedRef ::
    (BlobStorable m a, BlobStorable (t m) b, MonadTrans t) =>
    (a -> t m b) ->
    EagerBufferedRef a ->
    t m (EagerBufferedRef b)
migrateEagerBufferedRef = migrateReference 

instance Show a => Show (EagerBufferedRef a) where
    show = show . ebrValue

makeEagerBufferedRef :: MonadIO m => BlobRef a -> a -> m (EagerBufferedRef a)
makeEagerBufferedRef r a = do
    ref <- liftIO $ newIORef r
    return $ EagerBufferedRef ref a

flushEagerBufferedRef :: DirectBlobStorable m a => EagerBufferedRef a -> m (BlobRef a)
flushEagerBufferedRef EagerBufferedRef{..} = do
    r <- liftIO $ readIORef ebrIORef
    if isNull r
    then do
        (r' :: BlobRef a) <- fst <$> storeUpdateDirect ebrValue
        liftIO . writeIORef ebrIORef $! r'
        return r'
    else
        return r

instance DirectBlobStorable m a => BlobStorable m (EagerBufferedRef a) where
    load = do
        br <- get
        return $ do
            val <- loadDirect br
            makeEagerBufferedRef br val
    storeUpdate ebr@(EagerBufferedRef ref v) = do
        r <- liftIO $ readIORef ref
        if isNull r
        then do
            (!r' :: BlobRef a, !v') <- storeUpdateDirect v
            liftIO $ writeIORef ref $! r'
            return (put r', EagerBufferedRef ref v')
        else do
            r' <- flushEagerBufferedRef ebr
            return (put r', ebr)


instance (Monad m, DirectBlobStorable m a) => Reference m EagerBufferedRef a where
    refMake = makeEagerBufferedRef refNull
    refLoad EagerBufferedRef{..} = return ebrValue
    refCache ebr = return (ebrValue ebr, ebr)
    refFlush ebr@(EagerBufferedRef ref v) = do
        r <- liftIO $ readIORef ref
        if isNull r
        then do
            (!r' :: BlobRef a, !v') <- storeUpdateDirect v
            liftIO $ writeIORef ref $! r'
            return (EagerBufferedRef ref v', r')
        else return (ebr, r)
    refUncache = pure
    {-# INLINE refFlush #-}
    {-# INLINE refLoad #-}
    {-# INLINE refMake #-}
    {-# INLINE refCache #-}
    {-# INLINE refUncache #-}

instance (HashableTo h a) => HashableTo h (EagerBufferedRef a) where
    getHash = getHash . ebrValue

instance (MHashableTo m h a) => MHashableTo m h (EagerBufferedRef a) where
    getHashM = getHashM . ebrValue

instance DirectBlobStorable m a => BlobStorable m (Nullable (EagerBufferedRef a)) where
    load = do
        (r :: BlobRef a) <- get
        if isNull r then
            return (pure Null)
        else return $ do
            val <- loadDirect r
            Some <$> makeEagerBufferedRef r val
    storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
    storeUpdate (Some v) = do
        (!r, !v') <- storeUpdate v
        return (r, Some v')

instance (Applicative m, Cacheable m a) => Cacheable m (EagerBufferedRef a) where
    cache (EagerBufferedRef ioref v) = EagerBufferedRef ioref <$> cache v

-- |A reference that is generally not retained in memory once it has been written to disk, unless
-- it is retained by a copy of the reference. (Once all copies are flushed or dropped, the in-memory
-- value will no longer be retained. The reference is only written once, even if it is copied.)
--
-- This is essentially a simplified version of 'BufferedRef', where @BRBoth ref v@ is simply
-- replaced with @URBlobbed ref@.
data UnbufferedRef a
    = URBlobbed !(BlobRef a)
    -- ^A reference that is already on disk
    | URMemory {urIORef :: !(IORef (BlobRef a)), urValue :: !a}
    -- ^A reference that is in memory and may have been written to disk.
    -- If the reference has not been written, the 'urIORef' will contain 'refNull'.
    -- If it has been written, the 'BlobRef' will be a valid (non-null) reference to the
    -- stored value.

instance Show a => Show (UnbufferedRef a) where
    show (URBlobbed r) = show r
    show (URMemory _ v) = "{" ++ show v ++ "}"

instance DirectBlobStorable m a => BlobStorable m (UnbufferedRef a) where
    load = fmap URBlobbed <$> load
    storeUpdate ur = do
        (ur', !r) <- refFlush ur
        return (put r, ur')

instance DirectBlobStorable m a => BlobStorable m (Nullable (UnbufferedRef a)) where
    load = do
        r <- get
        if isNull r then
            return (pure Null)
        else
            return (pure (Some (URBlobbed r)))
    storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
    storeUpdate (Some v) = do
        (!r, !v') <- storeUpdate v
        return (r, Some v')

-- |Make an 'UnbufferedRef' from a value.
makeUnbufferedRef :: MonadIO m => a -> m (UnbufferedRef a)
makeUnbufferedRef urValue = do
        urIORef <- liftIO $ newIORef refNull
        return $! URMemory{..}

-- |Make an 'UnbufferedRef' from a value that is immediately flushed to the blob store.
-- This is equivalent to @refFlush <=< refMake@.
makeFlushedUnbufferedRef :: DirectBlobStorable m a => a -> m (UnbufferedRef a)
makeFlushedUnbufferedRef val = do
    (r, _) <- storeUpdateDirect val
    return $! URBlobbed r

-- |Create an 'UnbufferedRef' from a 'BlobRef'.
blobRefToUnbufferedRef :: BlobRef a -> UnbufferedRef a
blobRefToUnbufferedRef = URBlobbed

instance DirectBlobStorable m a => Reference m UnbufferedRef a where
    refMake = makeUnbufferedRef

    refLoad (URBlobbed ref) = loadDirect ref
    refLoad (URMemory _ v) = return v

    refCache ur@(URBlobbed ref) = do
        v <- loadDirect ref
        return (v, ur)
    refCache ur@(URMemory _ v) = return (v, ur)

    refFlush (URMemory ref v) = do
        r <- liftIO (readIORef ref)
        if isNull r then do
            (!r', _) <- storeUpdateDirect v
            liftIO $ writeIORef ref r'
            let !ur' = URBlobbed r'
            return (ur', r')
        else
            return (URBlobbed r, r)
    refFlush ur@(URBlobbed r) = return (ur, r)

    refUncache = fmap fst <$> refFlush

instance (DirectBlobStorable m a, MHashableTo m h a) => MHashableTo m h (UnbufferedRef a) where
    getHashM ref = getHashM =<< refLoad ref


-- |'BufferedFix' is a fixed-point combinator that uses a 'BufferedRef'.
-- This is used for constructing a recursive type from a type constructor.
-- For instance, given
--
-- > data BinaryTree r = Branch r r | Leaf
--
-- the type @BufferedFix BinaryTree@ describes is effectively equivalent to
-- @BufferedRef BinaryTree'@ with @BinaryTree'@ defined as
--
-- > data BinaryTree' = Branch (BufferedRef BinaryTree') (BufferedRef BinaryTree') | Leaf
--
-- The use of fixed point combinators such as this allows us to implement recursive
-- data-structures independently of how the recursion is handled (e.g. via 'BufferedRef'
-- as in this case, or without references as with 'Fix').
newtype BufferedFix f = BufferedFix {unBF :: BufferedRef (f (BufferedFix f))}

type instance Base (BufferedFix f) = f

instance (MonadBlobStore m, DirectBlobStorable m (f (BufferedFix f))) => BlobStorable m (BufferedFix f) where
    load = fmap BufferedFix <$> load
    storeUpdate bf = do
        (!p, !r) <- storeUpdate (unBF bf)
        return (p, BufferedFix r)

instance (MonadBlobStore m, DirectBlobStorable m (f (BufferedFix f))) => BlobStorable m (Nullable (BufferedFix f)) where
    load = fmap (fmap BufferedFix) <$> load
    storeUpdate bf = do
        (!p, !r) <- storeUpdate (fmap unBF bf)
        return (p, BufferedFix <$> r)

instance (Monad m, DirectBlobStorable m (f (BufferedFix f))) => MRecursive m (BufferedFix f) where
    mproject = refLoad . unBF
    {-# INLINE mproject #-}

instance (MonadIO m) => MCorecursive m (BufferedFix f) where
    membed = fmap BufferedFix . makeBufferedRef
    {-# INLINE membed #-}

-- |The 'FixShowable' class provides a means for rendering an instance of a
-- fixed-point of a functor (given by the provided combinator) as a 'String'.
class FixShowable fix where
    showFix ::
        Functor f =>
        -- |Render the functor with the recursive references rendered
        (f String -> String) ->
        -- |Value to render
        fix f ->
        String

instance FixShowable BufferedFix where
    showFix _ (BufferedFix (BRBlobbed r)) = show r
    showFix sh (BufferedFix (BRMemory _ v)) = sh (showFix sh <$> v)
    showFix sh (BufferedFix (BRBoth _ v)) = sh (showFix sh <$> v)

instance (Functor m, DirectBlobStorable m (f (BufferedFix f)), Cacheable m (f (BufferedFix f))) => Cacheable m (BufferedFix f) where
    cache = fmap BufferedFix . cache . unBF

-- |'UnbufferedFix' is a fixed-point combinator that uses an 'UnbufferedRef'.
-- (See also 'BufferedFix'.)
newtype UnbufferedFix f = UnbufferedFix {unUF :: UnbufferedRef (f (UnbufferedFix f))}

type instance Base (UnbufferedFix f) = f

instance (MonadBlobStore m, DirectBlobStorable m (f (UnbufferedFix f))) => BlobStorable m (UnbufferedFix f) where
    load = fmap UnbufferedFix <$> load
    storeUpdate uf = do
        (!p, !r) <- storeUpdate (unUF uf)
        return (p, UnbufferedFix r)

instance (MonadBlobStore m, DirectBlobStorable m (f (UnbufferedFix f))) => BlobStorable m (Nullable (UnbufferedFix f)) where
    load = fmap (fmap UnbufferedFix) <$> load
    storeUpdate bf = do
        (!p, !r) <- storeUpdate (fmap unUF bf)
        return (p, UnbufferedFix <$> r)


instance (MonadBlobStore m, DirectBlobStorable m (f (UnbufferedFix f))) => MRecursive m (UnbufferedFix f) where
    mproject = refLoad . unUF

instance (MonadIO m) => MCorecursive m (UnbufferedFix f) where
    membed = fmap UnbufferedFix . makeUnbufferedRef

instance FixShowable UnbufferedFix where
    showFix _ (UnbufferedFix (URBlobbed r)) = show r
    showFix sh (UnbufferedFix (URMemory _ v)) = sh (showFix sh <$> v)

-- BlobStorable instances
instance MonadBlobStore m => BlobStorable m IPS.IpInfo
instance MonadBlobStore m => BlobStorable m IPS.IdentityProviders
instance MonadBlobStore m => BlobStorable m ARS.ArInfo
instance MonadBlobStore m => BlobStorable m ARS.AnonymityRevokers
instance MonadBlobStore m => BlobStorable m Parameters.CryptographicParameters
instance MonadBlobStore m => BlobStorable m Amount
instance MonadBlobStore m => BlobStorable m BakerId
instance MonadBlobStore m => BlobStorable m BakerInfo
instance MonadBlobStore m => BlobStorable m BakerPoolInfo
instance (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (BakerInfoEx av)
instance MonadBlobStore m => BlobStorable m AccountIndex
instance MonadBlobStore m => BlobStorable m BS.ByteString
instance MonadBlobStore m => BlobStorable m EncryptedAmount
instance MonadBlobStore m => BlobStorable m TransactionHash
instance MonadBlobStore m => BlobStorable m ()
instance MonadBlobStore m => BlobStorable m Word8
instance MonadBlobStore m => BlobStorable m Word32
instance MonadBlobStore m => BlobStorable m Word64

instance MonadBlobStore m => BlobStorable m AccountEncryptedAmount
instance MonadBlobStore m => BlobStorable m PersistingAccountData
instance (MonadBlobStore m, IsChainParametersVersion cpv) => BlobStorable m (Authorizations cpv)
instance (MonadBlobStore m, IsAccountVersion av, AVSupportsDelegation av) => BlobStorable m (AccountDelegation av)
instance MonadBlobStore m => BlobStorable m (HigherLevelKeys a)
instance MonadBlobStore m => BlobStorable m ProtocolUpdate
instance MonadBlobStore m => BlobStorable m ExchangeRate
instance MonadBlobStore m => BlobStorable m ElectionDifficulty
instance MonadBlobStore m => BlobStorable m AccountReleaseSchedule
instance MonadBlobStore m => BlobStorable m MintRate
instance (MonadBlobStore m, IsChainParametersVersion cpv) => BlobStorable m (Parameters.MintDistribution cpv)
instance MonadBlobStore m => BlobStorable m Parameters.TransactionFeeDistribution
instance MonadBlobStore m => BlobStorable m Parameters.GASRewards
instance (MonadBlobStore m, IsChainParametersVersion cpv) => BlobStorable m (Parameters.PoolParameters cpv)
instance (MonadBlobStore m, IsChainParametersVersion cpv) => BlobStorable m (Parameters.CooldownParameters cpv)
instance (MonadBlobStore m, IsChainParametersVersion cpv) => BlobStorable m (Parameters.TimeParameters cpv)
instance MonadBlobStore m => BlobStorable m (Map AccountAddress Timestamp)
instance MonadBlobStore m => BlobStorable m WasmModule
instance (IsWasmVersion v, MonadBlobStore m) => BlobStorable m (WasmModuleV v)
instance MonadBlobStore m => BlobStorable m BakerPoolRewardDetails
instance MonadBlobStore m => BlobStorable m DelegatorCapital
instance MonadBlobStore m => BlobStorable m BakerCapital
instance MonadBlobStore m => BlobStorable m CapitalDistribution

newtype StoreSerialized a = StoreSerialized { unStoreSerialized :: a }
    deriving newtype (Serialize)
instance (MonadBlobStore m, Serialize a) => BlobStorable m (StoreSerialized a)
deriving newtype instance HashableTo h a => HashableTo h (StoreSerialized a)
deriving newtype instance MHashableTo m h a => MHashableTo m h (StoreSerialized a)

-- |A 'BufferedRef' accompanied with a hash.  The hash may be lazily computed.
--
-- The hash is computed and retained in the following circumstances:
--
-- * The hash is requested via 'getHashM'.
-- * The 'HashedBufferedRef' is constructed from an already-hashed value via 'bufferHashed'.
-- * The 'HashedBufferedRef'' is cached via 'cache' or 'refCache'.
--
-- Note, the hash is not computed when the reference is loaded with 'load', or dereferenced
-- with 'refLoad'.  None of the operations cause the hash to be dropped.
data HashedBufferedRef' h a
  = HashedBufferedRef
      { bufferedReference :: !(BufferedRef a),
        bufferedHash :: !(IORef (Nullable h))
      }

-- |Migrate a 'HashedBufferedRef' assuming that neither the value nor its hash
-- change. The input reference is uncached, and the new references is flushed to
-- disk, as well as cached in memory.
migrateHashedBufferedRefKeepHash ::
    (MonadTrans t, BlobStorable m a, BlobStorable (t m) a) =>
    HashedBufferedRef' h a ->
    t m (HashedBufferedRef' h a)
migrateHashedBufferedRefKeepHash hb = do
    !newRef <- refMake =<< lift (refLoad (bufferedReference hb))
    -- carry over the hash
    (!b, _) <- refFlush newRef
    !_ <- lift (refUncache (bufferedReference hb))
    return $!
        HashedBufferedRef
            { bufferedReference = b
            , bufferedHash = bufferedHash hb
            }

-- |Migrate a 'HashedBufferedRef'. The returned reference has a hash computed
-- already. The input reference is uncached, and the new references is flushed
-- to disk, as well as cached in memory.
migrateHashedBufferedRef ::
  (MonadTrans t, MHashableTo (t m) h b, BlobStorable m a, BlobStorable (t m) b) =>
  (a -> t m b) ->
  HashedBufferedRef' h a ->
  t m (HashedBufferedRef' h b)
migrateHashedBufferedRef f hb = do
    !newRef <- refMake =<< f =<< lift (refLoad (bufferedReference hb))
    -- compute the hash while the data is in memory.
    !h <- getHashM (bufferedReference newRef)
    liftIO . writeIORef (bufferedHash newRef) $! Some h
    (!b, _) <- refFlush newRef
    !_ <- lift (refUncache (bufferedReference hb))
    liftIO $! writeIORef (bufferedHash hb) Null
    return b

-- |A specialisation of 'HashedBufferedRef'' to the hash type 'H.Hash'.
type HashedBufferedRef = HashedBufferedRef' H.Hash

-- |Created a 'HashedBufferedRef' value from a 'Hashed' value, retaining the hash.
bufferHashed :: MonadIO m => Hashed a -> m (HashedBufferedRef a)
bufferHashed (Hashed !val !h) = do
  br <- makeBufferedRef val
  hashRef <- liftIO $ newIORef (Some h)
  return $ HashedBufferedRef br hashRef

-- |Make a 'HashedBufferedRef'' to a given value. This does not compute the hash, which is done
-- on demand.
makeHashedBufferedRef :: (MonadIO m) => a -> m (HashedBufferedRef' h a)
makeHashedBufferedRef val = do
  br <- makeBufferedRef val
  hashRef <- liftIO $ newIORef Null
  return $ HashedBufferedRef br hashRef

instance (DirectBlobStorable m a, MHashableTo m h a) => MHashableTo m h (HashedBufferedRef' h a) where
  getHashM HashedBufferedRef{..} = liftIO (readIORef bufferedHash) >>= \case
    Null -> do
        !h <- getHashM bufferedReference
        liftIO $ writeIORef bufferedHash (Some h)
        return h
    Some h -> return h

instance Show a => Show (HashedBufferedRef a) where
  show ref = show (bufferedReference ref)

instance (DirectBlobStorable m a) => BlobStorable m (HashedBufferedRef' h a) where
  load = do
    -- deserialize the reference and keep it as blobbed
    mbufferedReference <- load
    return $ do
        bufferedReference <- mbufferedReference
        bufferedHash <- liftIO $ newIORef Null
        return HashedBufferedRef{..}
  storeUpdate (HashedBufferedRef brm hRef) = do
    (!pt, !br) <- storeUpdate brm
    return (pt, HashedBufferedRef br hRef)

instance (Monad m, DirectBlobStorable m a, MHashableTo m h a) => Reference m (HashedBufferedRef' h) a where
  refFlush ref = do
    (!br, !r) <- flushBufferedRef (bufferedReference ref)
    return (HashedBufferedRef br (bufferedHash ref), r)

  refLoad = loadBufferedRef . bufferedReference

  refMake val = do
    br <- makeBufferedRef val
    hashRef <- liftIO $ newIORef Null
    return $ HashedBufferedRef br hashRef

  refCache ref = do
    (!val, !br) <- cacheBufferedRef (bufferedReference ref)
    currentHash <- liftIO (readIORef (bufferedHash ref))
    when (isNull currentHash) $ do
        h <- getHashM val
        liftIO $ writeIORef (bufferedHash ref) $! Some h
    let !ref' = ref {bufferedReference = br}
    return (val, ref')

  refUncache ref = do
    br <- uncacheBufferedRef (bufferedReference ref)
    return $ ref {bufferedReference = br}
  {-# INLINE refFlush #-}
  {-# INLINE refLoad #-}
  {-# INLINE refMake #-}
  {-# INLINE refCache #-}
  {-# INLINE refUncache #-}

instance (DirectBlobStorable m a) => BlobStorable m (Nullable (HashedBufferedRef' h a)) where
    load = do
        (r :: BlobRef a) <- get
        if isNull r then
            return (pure Null)
        else
            return $ do
                hashRef <- liftIO $ newIORef Null
                pure $ Some $ HashedBufferedRef (BRBlobbed r) hashRef
    storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
    storeUpdate (Some v) = do
        (!r, !v') <- storeUpdate v
        return (r, Some v')

type HashedBufferedRefForCPV1 (cpv :: ChainParametersVersion) a =
    JustForCPV1 cpv (HashedBufferedRef a)

instance
    (DirectBlobStorable m a, IsChainParametersVersion cpv) =>
    BlobStorable m (HashedBufferedRefForCPV1 cpv a)
    where
    load = case chainParametersVersion @cpv of
            SCPV0 -> return (pure NothingForCPV1)
            SCPV1 -> fmap (fmap JustForCPV1) load
    storeUpdate NothingForCPV1 = return (pure (), NothingForCPV1)
    storeUpdate (JustForCPV1 v) = do
        (!r, !v') <- storeUpdate v
        return (r, JustForCPV1 v')

-- |An 'EagerBufferedRef' accompanied by a hash.
-- Both the value and the hash are retained in memory by this reference.
data EagerlyHashedBufferedRef' h a = EagerlyHashedBufferedRef
    { ehbrReference :: {-# UNPACK #-} !(EagerBufferedRef a),
      ehbrHash :: !h
    }

type EagerlyHashedBufferedRef = EagerlyHashedBufferedRef' H.Hash


-- |Migrate an 'EagerlyHashedBufferedRef' **assuming the migration does not
-- change the hash**. The hash is carried over and not recomputed.
migrateEagerlyHashedBufferedRefKeepHash ::
    (BlobStorable m a, BlobStorable (t m) a, MonadTrans t) =>
    (a -> t m a) ->
    EagerlyHashedBufferedRef' h a ->
    t m (EagerlyHashedBufferedRef' h a)
migrateEagerlyHashedBufferedRefKeepHash f r = do
    ehbrReference <- migrateEagerBufferedRef f (ehbrReference r)
    return $! r { ehbrReference = ehbrReference }


instance HashableTo h (EagerlyHashedBufferedRef' h a) where
    getHash = ehbrHash

instance (Monad m) => MHashableTo m h (EagerlyHashedBufferedRef' h a)

instance (BlobStorable m a, MHashableTo m h a) => BlobStorable m (EagerlyHashedBufferedRef' h a) where
    load = do
        mref <- load
        return $ do
            ref <- mref
            (!a, !r) <- refCache ref
            h <- getHashM a
            return $ EagerlyHashedBufferedRef r h
    storeUpdate (EagerlyHashedBufferedRef br0 hsh) = do
        (!pt, !br1) <- storeUpdate br0
        return (pt, EagerlyHashedBufferedRef br1 hsh)
    {-# INLINE load #-}
    {-# INLINE storeUpdate #-}

-- |Convert a 'BlobRef' to an 'EagerlyHashedBufferedRef'.
blobRefToEagerlyHashedBufferedRef :: (BlobStorable m a, MHashableTo m h a) => BlobRef a -> m (EagerlyHashedBufferedRef' h a)
blobRefToEagerlyHashedBufferedRef ref = do
    val <- loadRef ref
    ehbrReference <- makeEagerBufferedRef ref val
    ehbrHash <- getHashM val
    return $! EagerlyHashedBufferedRef{..}

instance (BlobStorable m a, MHashableTo m h a) => BlobStorable m (Nullable (EagerlyHashedBufferedRef' h a)) where
    load = do
        (r :: BlobRef a) <- get
        if isNull r then
            return (pure Null)
        else
            return $ Some <$> blobRefToEagerlyHashedBufferedRef r
    storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
    storeUpdate (Some v) = do
        (!r, !v') <- storeUpdate v
        return (r, Some v')

instance (BlobStorable m a, MHashableTo m h a, BlobStorable m b) => BlobStorable m (Nullable (EagerlyHashedBufferedRef' h a, b)) where
  load = do
    (r :: BlobRef a) <- get
    if isNull r
      then return (pure Null)
      else do
        bval <- load
        return $ do
          binner <- bval
          ehbr <- blobRefToEagerlyHashedBufferedRef r
          pure $ Some (ehbr, binner)
  storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
  storeUpdate (Some v) = do
    (!r, !v') <- storeUpdate v
    return (r, Some v')

instance (Monad m, BlobStorable m a, MHashableTo m h a) => Reference m (EagerlyHashedBufferedRef' h) a where
    refFlush ref = do
        (!br, !r) <- refFlush (ehbrReference ref)
        return (EagerlyHashedBufferedRef br (ehbrHash ref), r)
    
    refLoad = refLoad . ehbrReference

    refMake val = do
        br <- refMake val
        h <- getHashM val
        return $ EagerlyHashedBufferedRef br h
    
    refCache ref = do
        (!v, !r) <- refCache (ehbrReference ref)
        return (v, ref{ehbrReference = r})
    
    refUncache ref = do
        br <- refUncache (ehbrReference ref)
        return $! ref{ehbrReference = br}
    {-# INLINE refFlush #-}
    {-# INLINE refLoad #-}
    {-# INLINE refMake #-}
    {-# INLINE refCache #-}
    {-# INLINE refUncache #-}

instance (Show h, Show a) => Show (EagerlyHashedBufferedRef' h a) where
  show ref = show (ehbrReference ref) ++ " with hash: " ++ show (ehbrHash ref)

-- |This class abstracts values that can be cached in some monad.
class Cacheable m a where
    -- |Recursively cache a value of type @a@.
    cache :: a -> m a
    default cache :: (Applicative m) => a -> m a
    cache = pure

instance (Applicative m, Cacheable m a) => Cacheable m (Nullable a) where
    cache Null = pure Null
    cache (Some v) = Some <$> cache v

instance (Applicative m, Cacheable m a) => Cacheable m (JustForCPV1 cpv a) where
    cache = traverse cache

instance (DirectBlobStorable m a, Cacheable m a) => Cacheable m (BufferedRef a) where
    cache BRBlobbed{..} = do
        brValue <- cache =<< loadDirect brRef
        return BRBoth{..}
    cache br@BRMemory{..} = do
        cachedVal <- cache brValue
        return $! br{brValue = cachedVal}
    cache br@BRBoth{..} = do
        cachedVal <- cache brValue
        return $! br{brValue = cachedVal}

instance (MHashableTo m h a, DirectBlobStorable m a, Cacheable m a) => Cacheable m (HashedBufferedRef' h a) where
  cache (HashedBufferedRef ref hshRef) = do
    ref' <- cache ref
    currentHash <- liftIO (readIORef hshRef)
    when (isNull currentHash) $ do
        h <- getHashM ref'
        liftIO $ writeIORef hshRef $! Some h
    return (HashedBufferedRef ref' hshRef)

instance (BlobStorable m a, Cacheable m a) => Cacheable m (EagerlyHashedBufferedRef' h a) where
    cache r = do
        ref' <- cache (ehbrReference r)
        return $! r {ehbrReference = ref'}

instance (Applicative m, Cacheable m a, Cacheable m b) => Cacheable m (a, b) where
    cache (x, y) = (,) <$> cache x <*> cache y

instance Applicative m => Cacheable m () where
    cache _ = pure ()

-- Required for caching PersistentAccount
instance (Applicative m) => Cacheable m EncryptedAmount
instance (Applicative m) => Cacheable m AccountReleaseSchedule
instance (Applicative m) => Cacheable m (Map AccountAddress Timestamp)
instance (Applicative m) => Cacheable m WasmModule
instance (Applicative m) => Cacheable m PersistingAccountData
instance (IsAccountVersion av, Applicative m) => Cacheable m (BakerInfoEx av)
instance (IsAccountVersion av, Applicative m) => Cacheable m (AccountDelegation av)
-- Required for caching AccountIndexes
instance (Applicative m) => Cacheable m AccountIndex
-- Required for caching BlockStatePointers
instance (Applicative m) => Cacheable m IPS.IdentityProviders
instance (Applicative m) => Cacheable m ARS.AnonymityRevokers
instance (Applicative m) => Cacheable m Parameters.CryptographicParameters
-- Required for caching Bakers
instance (Applicative m) => Cacheable m BakerInfo
instance (Applicative m) => Cacheable m BakerPoolInfo
instance (Applicative m) => Cacheable m Amount
-- Required for caching Updates
instance (Applicative m) => Cacheable m (StoreSerialized a)
instance (Applicative m) => Cacheable m (Parameters.PoolParameters cpv)
instance (Applicative m) => Cacheable m (Parameters.CooldownParameters cpv)
instance (Applicative m) => Cacheable m (Parameters.TimeParameters cpv)
instance (Applicative m) => Cacheable m BakerPoolRewardDetails
instance (Applicative m) => Cacheable m DelegatorCapital
instance (Applicative m) => Cacheable m BakerCapital
instance (Applicative m) => Cacheable m CapitalDistribution

-- |Typeclass for caching a container type, given a function for caching the
-- contained type. The caching operation should not affect the value up to an equivalence
-- appropriate to the type in question.
class Cacheable1 m c a where
    -- |Lift a caching operation on the contained type to a caching operation on the container
    -- type.
    liftCache :: (a -> m a) -> c -> m c

instance DirectBlobStorable m a => Cacheable1 m (BufferedRef a) a where
    liftCache cch BRBlobbed{..} = do
        brValue <- cch =<< loadDirect brRef
        return BRBoth{..}
    liftCache cch br@BRMemory{..} = do
        cachedVal <- cch brValue
        return $! br{brValue = cachedVal}
    liftCache cch br@BRBoth{..} = do
        cachedVal <- cch brValue
        return $! br{brValue = cachedVal}

instance (MHashableTo m h a, DirectBlobStorable m a) => Cacheable1 m (HashedBufferedRef' h a) a where
    liftCache cch (HashedBufferedRef ref hshRef) = do
        ref' <- liftCache cch ref
        currentHash <- liftIO (readIORef hshRef)
        when (isNull currentHash) $ do
            h <- getHashM ref'
            liftIO $ writeIORef hshRef $! Some h
        return (HashedBufferedRef ref' hshRef)
