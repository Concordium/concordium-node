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
module Concordium.GlobalState.Persistent.BlobStore where

import Control.Concurrent.MVar
import System.IO
import Data.Kind (Type)
import Data.Serialize
import Data.Coerce
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSUnsafe
import Control.Exception
import Data.Functor.Foldable
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans.Writer.Strict (WriterT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans
import System.Directory
import GHC.Stack
import Data.IORef
import Concordium.Crypto.EncryptedTransfers
import Data.Map (Map)
import Foreign.Ptr
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
truncateBlobStore :: BlobStoreAccess -> BlobRef a -> IO ()
truncateBlobStore BlobStoreAccess{..} (BlobRef offset) = mask $ \restore -> do
  bh@BlobHandle{..} <- takeMVar blobStoreFile
  eres <- try $ restore $ do
    hSeek bhHandle AbsoluteSeek (fromIntegral offset)
    esize <- decode <$> BS.hGet bhHandle 8
    case esize :: Either String Word64 of
      Right size -> do
        let newSize = offset + 8 + size
        hSetFileSize bhHandle $ fromIntegral newSize
        putMVar blobStoreFile bh{bhSize = fromIntegral newSize, bhAtEnd=False}
      _ -> throwIO $ userError "Cannot truncate the blob store: cannot obtain the last blob size"
  case eres :: Either SomeException () of
    Left e -> throwIO e
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
    {-# INLINE storeRaw #-}
    {-# INLINE loadRaw #-}
    {-# INLINE flushStore #-}

instance HasBlobStore BlobStore where
  blobStore = bscBlobStore
  blobLoadCallback = bscLoadCallback
  blobStoreCallback = bscStoreCallback

-- |A monad transformer that is equivalent to 'ReaderT' but provides a 'MonadBlobStore' instance
-- based on the context (rather than lifting).
newtype BlobStoreT r m a = BlobStoreT {runBlobStoreT :: r -> m a}
    deriving
        (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadFail, MonadLogger)
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
    -- |Serialize a value of type @a@ for storage.
    store :: a -> m Put
    default store :: (Serialize a) => a -> m Put
    store = pure . put
    -- |Deserialize a value of type @a@ from storage.
    load :: Get (m a)
    default load :: (Serialize a) => Get (m a)
    load = pure <$> get
    -- |Store a value of type @a@, possibly updating its representation.
    -- This is used when the value's representation includes pointers that
    -- may be replaced or supplemented with blob references.
    storeUpdate :: a -> m (Put, a)
    storeUpdate v = (,v) <$> store v
    {-# INLINE store #-}
    {-# INLINE load #-}
    {-# INLINE storeUpdate #-}

-- |Store a value in the blob store and return a reference to it.
storeRef :: BlobStorable m a => a -> m (BlobRef a)
storeRef v = do
    p <- runPut <$> store v
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
loadRef :: (HasCallStack, BlobStorable m a) => BlobRef a -> m a
loadRef ref = do
    bs <- loadRaw ref
    case runGet load bs of
        Left e -> error (e ++ " :: " ++ show bs)
        Right !mv -> mv
{-# INLINE loadRef #-}

instance (MonadIO m, BlobStorable m a, BlobStorable m b) => BlobStorable m (a, b) where

  storeUpdate (a, b) = do
    (!pa, !a') <- storeUpdate a
    (!pb, !b') <- storeUpdate b
    let pab = pa >> pb
    return (pab, (a', b'))

  store v = fst <$> storeUpdate v

  load = do
    ma <- load
    mb <- load
    return $ do
      a <- ma
      b <- mb
      return (a, b)
  {-# INLINE store #-}
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

instance HasNull (BlobRef a) where
    refNull = BlobRef maxBound
    isNull = (== refNull)

instance HasNull (Nullable a) where
    refNull = Null
    isNull Null = True
    isNull _ = False

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

-- |A value that may exists purely on disk ('BRBlobbed'), purely in memory
-- ('BRMemory' with @brIORef = null@), or both in memory and on disk. When the
-- value is both on disk and in memory the two values must match.
data BufferedRef a
    = BRBlobbed {brRef :: !(BlobRef a)}
    -- ^Value stored on disk
    | BRMemory {brIORef :: !(IORef (BlobRef a)), brValue :: !a}
    -- ^Value stored in memory and possibly on disk.
    -- When a new 'BRMemory' instance is created, we initialize 'brIORef' to 'refNull'.
    -- When we store the instance in persistent storage, we update 'brIORef' with the corresponding pointer.
    -- That way, when we store the same instance again on disk (this could be, e.g., a child block
    -- that inherited its parent's state) we can store the pointer to the 'brValue' data rather than
    -- storing all of the data again.
    | BRBoth {brRef :: !(BlobRef a), brValue :: !a}
    -- ^Value stored in memory and on disk.

-- |Coerce one buffered ref to another. This is unsafe unless a and b have compatible
-- blobstorable instances.
unsafeCoerceBufferedRef :: (a -> b) -> BufferedRef a -> BufferedRef b
unsafeCoerceBufferedRef _ (BRBlobbed br) = BRBlobbed (coerce br)
unsafeCoerceBufferedRef f (BRMemory ioref val) = BRMemory (coerce ioref) (f val)
unsafeCoerceBufferedRef f (BRBoth br val) = BRBoth (coerce br) (f val)

-- | Create a @BRMemory@ value in a @MonadIO@ context with the provided values
makeBRMemory :: MonadIO m => (BlobRef a) -> a -> m (BufferedRef a)
makeBRMemory r a = liftIO $ do
    ref <- newIORef r
    return $ BRMemory ref a

-- | Create a @BRMemory@ value with a null reference (so the value is just in memory)
makeBufferedRef :: MonadIO m => a -> m (BufferedRef a)
makeBufferedRef = makeBRMemory refNull

instance Show a => Show (BufferedRef a) where
  show (BRBlobbed r) = show r
  show (BRMemory _ v) = "{" ++ show v ++ "}"
  show (BRBoth r v) = "{" ++ show v ++ "}@" ++ show r

instance BlobStorable m a => BlobStorable m (BufferedRef a) where
    store b = getBRRef b >>= store
    load = fmap BRBlobbed <$> load
    storeUpdate brm@(BRMemory ref v) = do
        r <- liftIO $ readIORef ref
        if isNull r
        then do
            (r' :: BlobRef a, v') <- storeUpdateRef v
            liftIO . writeIORef ref $! r'
            (,BRBoth r' v') <$> store r'
        else (,brm) <$> store brm
    storeUpdate x = (,x) <$> store x

-- |Stores in-memory data to disk if it has not been stored yet and returns pointer to saved data
getBRRef :: BlobStorable m a => BufferedRef a -> m (BlobRef a)
getBRRef (BRMemory ref v) = do
    r <- liftIO $ readIORef ref
    if isNull r
    then do
        (r' :: BlobRef a) <- storeRef v
        liftIO . writeIORef ref $! r'
        return r'
    else
        return r
getBRRef (BRBoth r _) = return r
getBRRef (BRBlobbed r) = return r

instance BlobStorable m a => BlobStorable m (Nullable (BufferedRef a)) where
    store Null = return $ put (refNull :: BlobRef a)
    store (Some v) = store v
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
loadBufferedRef :: BlobStorable m a => BufferedRef a -> m a
loadBufferedRef = refLoad

-- |Load a 'BufferedRef' and cache it if it wasn't already in memory.
cacheBufferedRef :: BlobStorable m a => BufferedRef a -> m (a, BufferedRef a)
cacheBufferedRef = refCache

-- |If given a Blobbed reference, do nothing. Otherwise if needed store the value.
flushBufferedRef :: BlobStorable m a => BufferedRef a -> m (BufferedRef a, BlobRef a)
flushBufferedRef = refFlush

-- |Convert a Cached reference into a Blobbed one storing the data if needed.
uncacheBuffered :: BlobStorable m a => BufferedRef a -> m (BufferedRef a)
uncacheBuffered = refUncache

instance (Monad m, BlobStorable m a) => Reference m BufferedRef a where
  refMake = makeBRMemory refNull

  refLoad (BRBlobbed ref) = loadRef ref
  refLoad (BRMemory _ v) = return v
  refLoad (BRBoth _ v) = return v

  refCache (BRBlobbed ref) = do
    v <- loadRef ref
    return (v, BRBoth ref v)
  refCache r@(BRMemory _ v) = return (v, r)
  refCache r@(BRBoth _ v) = return (v, r)

  refFlush brm@(BRMemory ref v) = do
    r <- liftIO $ readIORef ref
    if isNull r
      then do
        (!r' :: BlobRef a, !v') <- storeUpdateRef v
        liftIO . writeIORef ref $! r'
        return (BRBoth r' v', r')
      else return (brm, r)
  refFlush b = return (b, brRef b)

  refUncache v@(BRMemory _ _) = BRBlobbed <$> getBRRef v
  refUncache (BRBoth r _) = return $ BRBlobbed r
  refUncache b = return b
  {-# INLINE refFlush #-}
  {-# INLINE refLoad #-}
  {-# INLINE refMake #-}
  {-# INLINE refCache #-}
  {-# INLINE refUncache #-}

instance (BlobStorable m a, MHashableTo m h a) => MHashableTo m h (BufferedRef a) where
  getHashM ref = getHashM =<< refLoad ref

instance (Serialize a, Serialize b, BlobStorable m a) => MHashableTo m H.Hash (BufferedRef a, b) where
  getHashM (a, b) = do
    val <- encode <$> refLoad a
    return $ H.hash (val <> encode b)

instance (BlobStorable m a, BlobStorable m b) => BlobStorable m (Nullable (BufferedRef a, b)) where
  store Null = return $ put (refNull :: BlobRef a)
  store (Some v) = store v
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

instance (BlobStorable m a, BlobStorable m b) => BlobStorable m (Nullable (HashedBufferedRef a, b)) where
  store Null = return $ put (refNull :: BlobRef a)
  store (Some v) = store v
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

instance Show a => Show (EagerBufferedRef a) where
    show = show . ebrValue

makeEagerBufferedRef :: MonadIO m => BlobRef a -> a -> m (EagerBufferedRef a)
makeEagerBufferedRef r a = do
    ref <- liftIO $ newIORef r
    return $ EagerBufferedRef ref a

flushEagerBufferedRef :: BlobStorable m a => EagerBufferedRef a -> m (BlobRef a)
flushEagerBufferedRef EagerBufferedRef{..} = do
    r <- liftIO $ readIORef ebrIORef
    if isNull r
    then do
        (r' :: BlobRef a) <- storeRef ebrValue
        liftIO . writeIORef ebrIORef $! r'
        return r'
    else
        return r

instance BlobStorable m a => BlobStorable m (EagerBufferedRef a) where
    store b = flushEagerBufferedRef b >>= store
    load = do
        br <- get
        return $ do
            val <- loadRef br
            makeEagerBufferedRef br val
    storeUpdate ebr@(EagerBufferedRef ref v) = do
        r <- liftIO $ readIORef ref
        if isNull r
        then do
            (!r' :: BlobRef a, !v') <- storeUpdateRef v
            liftIO $ writeIORef ref $! r'
            (, EagerBufferedRef ref v') <$> store r'
        else (,ebr) <$> store ebr


instance (Monad m, BlobStorable m a) => Reference m EagerBufferedRef a where
    refMake = makeEagerBufferedRef refNull
    refLoad EagerBufferedRef{..} = return ebrValue
    refCache ebr = return (ebrValue ebr, ebr)
    refFlush ebr@(EagerBufferedRef ref v) = do
        r <- liftIO $ readIORef ref
        if isNull r
        then do
            (!r' :: BlobRef a, !v') <- storeUpdateRef v
            liftIO $ writeIORef ref $! r'
            return (EagerBufferedRef ref v', r')
        else return (ebr, r)
    refUncache = pure
    {-# INLINE refFlush #-}
    {-# INLINE refLoad #-}
    {-# INLINE refMake #-}
    {-# INLINE refCache #-}
    {-# INLINE refUncache #-}

instance (MHashableTo m h a) => MHashableTo m h (EagerBufferedRef a) where
    getHashM = getHashM . ebrValue

instance BlobStorable m a => BlobStorable m (Nullable (EagerBufferedRef a)) where
    store Null = return $ put (refNull :: BlobRef a)
    store (Some v) = store v
    load = do
        (r :: BlobRef a) <- get
        if isNull r then
            return (pure Null)
        else return $ do
            val <- loadRef r
            Some <$> makeEagerBufferedRef r val
    storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
    storeUpdate (Some v) = do
        (!r, !v') <- storeUpdate v
        return (r, Some v')

instance (Applicative m, Cacheable m a) => Cacheable m (EagerBufferedRef a) where
    cache (EagerBufferedRef ioref v) = EagerBufferedRef ioref <$> cache v

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
-- datastructures independently of how the recursion is handled (e.g. via 'BufferedRef'
-- as in this case, or without refrences as with 'Fix').
newtype BufferedFix f = BufferedFix {unBF :: BufferedRef (f (BufferedFix f))}

type instance Base (BufferedFix f) = f

instance (MonadBlobStore m, BlobStorable m (f (BufferedFix f))) => BlobStorable m (BufferedFix f) where
    store = store . unBF
    load = fmap BufferedFix <$> load
    storeUpdate bf = do
        (!p, !r) <- storeUpdate (unBF bf)
        return (p, BufferedFix r)

instance (MonadBlobStore m, BlobStorable m (f (BufferedFix f))) => BlobStorable m (Nullable (BufferedFix f)) where
    store = store . fmap unBF
    load = fmap (fmap BufferedFix) <$> load
    storeUpdate bf = do
        (!p, !r) <- storeUpdate (fmap unBF bf)
        return (p, BufferedFix <$> r)

instance (Monad m, BlobStorable m (f (BufferedFix f))) => MRecursive m (BufferedFix f) where
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

instance (Functor m, BlobStorable m (f (BufferedFix f)), Cacheable m (f (BufferedFix f))) => Cacheable m (BufferedFix f) where
    cache = fmap BufferedFix . cache . unBF

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
instance (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (AccountDelegation av)
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

-- |A specialisation of 'HashedBufferedRef'' to the hash type 'H.Hash'.
type HashedBufferedRef = HashedBufferedRef' H.Hash

-- |Created a 'HashedBufferedRef' value from a 'Hashed' value, retaining the hash.
bufferHashed :: MonadIO m => Hashed a -> m (HashedBufferedRef a)
bufferHashed (Hashed !val !h) = do
  br <- makeBRMemory refNull val
  hashRef <- liftIO $ newIORef (Some h)
  return $ HashedBufferedRef br hashRef

-- |Make a 'HashedBufferedRef'' to a given value. This does not compute the hash, which is done
-- on demand.
makeHashedBufferedRef :: (MonadIO m) => a -> m (HashedBufferedRef' h a)
makeHashedBufferedRef val = do
  br <- makeBRMemory refNull val
  hashRef <- liftIO $ newIORef Null
  return $ HashedBufferedRef br hashRef

instance (BlobStorable m a, MHashableTo m h a) => MHashableTo m h (HashedBufferedRef' h a) where
  getHashM HashedBufferedRef{..} = liftIO (readIORef bufferedHash) >>= \case
    Null -> do
        !h <- getHashM bufferedReference
        liftIO $ writeIORef bufferedHash (Some h)
        return h
    Some h -> return h

instance Show a => Show (HashedBufferedRef a) where
  show ref = show (bufferedReference ref)

instance (BlobStorable m a) => BlobStorable m (HashedBufferedRef' h a) where
  store b =
    -- store the value if needed and then serialize the returned reference.
    store (bufferedReference b)
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

instance (Monad m, BlobStorable m a, MHashableTo m h a) => Reference m (HashedBufferedRef' h) a where
  refFlush ref = do
    (!br, !r) <- flushBufferedRef (bufferedReference ref)
    return (HashedBufferedRef br (bufferedHash ref), r)

  refLoad = loadBufferedRef . bufferedReference

  refMake val = do
    br <- makeBRMemory refNull val
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
    br <- uncacheBuffered (bufferedReference ref)
    return $ ref {bufferedReference = br}
  {-# INLINE refFlush #-}
  {-# INLINE refLoad #-}
  {-# INLINE refMake #-}
  {-# INLINE refCache #-}
  {-# INLINE refUncache #-}

instance (BlobStorable m a) => BlobStorable m (Nullable (HashedBufferedRef' h a)) where
    store Null = return $ put (refNull :: BlobRef a)
    store (Some v) = store v
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
    (BlobStorable m a, IsChainParametersVersion cpv) =>
    BlobStorable m (HashedBufferedRefForCPV1 cpv a)
    where
    store NothingForCPV1 = return (pure ())
    store (JustForCPV1 v) = store v
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

instance HashableTo h (EagerlyHashedBufferedRef' h a) where
    getHash = ehbrHash

instance (Monad m) => MHashableTo m h (EagerlyHashedBufferedRef' h a)

instance (BlobStorable m a, MHashableTo m h a) => BlobStorable m (EagerlyHashedBufferedRef' h a) where
    store b = store (ehbrReference b)
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
    {-# INLINE store #-}
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
    store Null = return $ put (refNull :: BlobRef a)
    store (Some v) = store v
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
  store Null = return $ put (refNull :: BlobRef a)
  store (Some v) = store v
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

instance Show a => Show (EagerlyHashedBufferedRef a) where
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

instance (BlobStorable m a, Cacheable m a) => Cacheable m (BufferedRef a) where
    cache BRBlobbed{..} = do
        brValue <- cache =<< loadRef brRef
        return BRBoth{..}
    cache br@BRMemory{..} = do
        cachedVal <- cache brValue
        return $! br{brValue = cachedVal}
    cache br@BRBoth{..} = do
        cachedVal <- cache brValue
        return $! br{brValue = cachedVal}

instance (MHashableTo m h a, BlobStorable m a, Cacheable m a) => Cacheable m (HashedBufferedRef' h a) where
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

instance BlobStorable m a => Cacheable1 m (BufferedRef a) a where
    liftCache cch BRBlobbed{..} = do
        brValue <- cch =<< loadRef brRef
        return BRBoth{..}
    liftCache cch br@BRMemory{..} = do
        cachedVal <- cch brValue
        return $! br{brValue = cachedVal}
    liftCache cch br@BRBoth{..} = do
        cachedVal <- cch brValue
        return $! br{brValue = cachedVal}

instance (MHashableTo m h a, BlobStorable m a) => Cacheable1 m (HashedBufferedRef' h a) a where
    liftCache cch (HashedBufferedRef ref hshRef) = do
        ref' <- liftCache cch ref
        currentHash <- liftIO (readIORef hshRef)
        when (isNull currentHash) $ do
            h <- getHashM ref'
            liftIO $ writeIORef hshRef $! Some h
        return (HashedBufferedRef ref' hshRef)
