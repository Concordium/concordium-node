{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

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
import Data.Serialize
import Data.Word
import qualified Data.ByteString as BS
import Control.Exception
import Data.Functor.Foldable
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class
import System.Directory
import GHC.Stack
import Data.IORef
import Concordium.Crypto.EncryptedTransfers
import Data.Map (Map)

import Concordium.GlobalState.Persistent.MonadicRecursive

-- Imports for providing instances
import Concordium.GlobalState.Account
import Concordium.GlobalState.BakerInfo
import qualified Concordium.Types.IdentityProviders as IPS
import qualified Concordium.Types.AnonymityRevokers as ARS
import qualified Concordium.GlobalState.Parameters as Parameters
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.Persistent.BlobStore.Flush
import Concordium.Types
import Concordium.Types.Updates
import Concordium.Wasm

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo
import Control.Monad

-- | A @BlobRef a@ represents an offset on a file, at
-- which a value of type @a@ is stored.
newtype BlobRef a = BlobRef Word64
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
data BlobStore = BlobStore {
    blobStoreFile :: !(MVar BlobHandle),
    blobStoreFilePath :: !FilePath
}

class HasBlobStore a where
    blobStore :: a -> BlobStore

instance HasBlobStore BlobStore where
    blobStore = id

-- |Create a new blob store at a given location.
-- Fails if a file or directory at that location already exists.
createBlobStore :: FilePath -> IO BlobStore
createBlobStore blobStoreFilePath = do
    pathEx <- doesPathExist blobStoreFilePath
    when pathEx $ throwIO (userError $ "Blob store path already exists: " ++ blobStoreFilePath)
    bhHandle <- openBinaryFile blobStoreFilePath ReadWriteMode
    blobStoreFile <- newMVar BlobHandle{bhSize=0, bhAtEnd=True,..}
    return BlobStore{..}

-- |Load an existing blob store from a file.
-- The file must be readable and writable, but this is not checked here.
loadBlobStore :: FilePath -> IO BlobStore
loadBlobStore blobStoreFilePath = do
  bhHandle <- openBinaryFile blobStoreFilePath ReadWriteMode
  bhSize <- fromIntegral <$> hFileSize bhHandle
  blobStoreFile <- newMVar BlobHandle{bhAtEnd=bhSize==0,..}
  return BlobStore{..}

-- |Flush all buffers associated with the blob store,
-- ensuring all the contents is written out.
flushBlobStore :: BlobStore -> IO ()
flushBlobStore BlobStore{..} =
    withMVar blobStoreFile (hFlushOS . bhHandle)

-- |Close all references to the blob store, flushing it
-- in the process.
closeBlobStore :: BlobStore -> IO ()
closeBlobStore BlobStore{..} = do
    BlobHandle{..} <- takeMVar blobStoreFile
    hClose bhHandle

-- |Close all references to the blob store and delete the backing file.
destroyBlobStore :: BlobStore -> IO ()
destroyBlobStore bs@BlobStore{..} = do
    closeBlobStore bs
    removeFile blobStoreFilePath

-- |Run a computation with temporary access to the blob store.
-- The given FilePath is a directory where the temporary blob
-- store will be created.
-- The blob store file is deleted afterwards.
runBlobStoreTemp :: FilePath -> ReaderT BlobStore IO a -> IO a
runBlobStoreTemp dir a = bracket openf closef usef
    where
        openf = openBinaryTempFile dir "blb.dat"
        closef (tempFP, h) = do
            hClose h
            removeFile tempFP
        usef (fp, h) = do
            mv <- newMVar (BlobHandle h True 0)
            res <- runReaderT a (BlobStore mv fp)
            _ <- takeMVar mv
            return res

-- | Read a bytestring from the blob store at the given offset
readBlobBS :: BlobStore -> BlobRef a -> IO BS.ByteString
readBlobBS BlobStore{..} (BlobRef offset) = mask $ \restore -> do
        bh@BlobHandle{..} <- takeMVar blobStoreFile
        eres <- try $ restore $ do
            hSeek bhHandle AbsoluteSeek (fromIntegral offset)
            esize <- decode <$> BS.hGet bhHandle 8
            case esize :: Either String Word64 of
                Left e -> error e
                Right size -> BS.hGet bhHandle (fromIntegral size)
        putMVar blobStoreFile bh{bhAtEnd=False}
        case eres :: Either SomeException BS.ByteString of
            Left e -> throwIO e
            Right bs -> return bs

-- | Write a bytestring into the blob store and return the offset
writeBlobBS :: BlobStore -> BS.ByteString -> IO (BlobRef a)
writeBlobBS BlobStore{..} bs = mask $ \restore -> do
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
    {-# INLINE storeRaw #-}
    {-# INLINE loadRaw #-}
    {-# INLINE flushStore #-}

instance MonadBlobStore (ReaderT BlobStore IO)

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
    (p, v') <- storeUpdate v
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
    (pa, a') <- storeUpdate a
    (pb, b') <- storeUpdate b
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

-- |A value that may exists purely on disk ('BRBlobbed'), purely in memory ('BRMemory'), or both ('BRCached').
-- When the value is cached, the cached value must match the value stored on disk.
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

instance BlobStorable m a => BlobStorable m (BufferedRef a) where
    store b = getBRRef b >>= store
    load = fmap BRBlobbed <$> load
    storeUpdate brm@(BRMemory ref v) = do
        r <- liftIO $ readIORef ref
        if isNull r
        then do
            (r' :: BlobRef a, v') <- storeUpdateRef v
            liftIO . writeIORef ref $! r'
            (,BRMemory ref v') <$> store r'
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
        (r, v') <- storeUpdate v
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

  refCache (BRBlobbed ref) = do
    v <- loadRef ref
    (v,) <$> makeBRMemory ref v
  refCache r@(BRMemory _ v) = return (v, r)

  refFlush brm@(BRMemory ref v) = do
    r <- liftIO $ readIORef ref
    if isNull r
      then do
        (r' :: BlobRef a, v') <- storeUpdateRef v
        liftIO . writeIORef ref $! r'
        return (BRMemory ref v', r')
      else return (brm, r)
  refFlush b = return (b, brRef b)

  refUncache v@(BRMemory _ _) = BRBlobbed <$> getBRRef v
  refUncache b = return b
  {-# INLINE refFlush #-}
  {-# INLINE refLoad #-}
  {-# INLINE refMake #-}
  {-# INLINE refCache #-}
  {-# INLINE refUncache #-}

instance (BlobStorable m a, MHashableTo m H.Hash a) => MHashableTo m H.Hash (BufferedRef a) where
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
    (r, v') <- storeUpdate v
    return (r, Some v')

instance (BlobStorable m a, BlobStorable m b, MHashableTo m H.Hash a) => BlobStorable m (Nullable (HashedBufferedRef a, b)) where
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
          pure $ Some (HashedBufferedRef (BRBlobbed r) Nothing, binner)
  storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
  storeUpdate (Some v) = do
    (r, v') <- storeUpdate v
    return (r, Some v')


-- | Blobbed is a fixed point of the functor `f` wrapped in references of type @ref@
newtype Blobbed ref f = Blobbed {unblobbed :: ref (f (Blobbed ref f))}

-- Serialize instances, just wrap the Serialize instances of the underlying reference
deriving instance (forall a. Serialize (ref a)) => Serialize (Blobbed ref f)

-- If a monad can manage references of type @ref@ then it can store values of type
-- @Blobbed ref f@ (just by serializing the inner references) into references of type
-- @ref@
instance (MonadBlobStore m, forall a. Serialize (ref a)) => BlobStorable m (Blobbed ref f)

-- If a monad can store references of type @ref@ and a reference is serializable and nullable,
-- then it can store values of type @Nullable (Blobbed ref f)@ into references of type @ref@
instance (MonadBlobStore m, forall a. HasNull (ref a), forall a. Serialize (ref a)) => BlobStorable m (Nullable (Blobbed ref f))

type instance Base (Blobbed ref f) = f

instance (Monad m, BlobStorable m (f (Blobbed BlobRef f))) => MRecursive m (Blobbed BlobRef f) where
    -- Projecting the blobbed reference boils down to load the value it contains
    mproject (Blobbed r) = loadRef r

instance (Monad m, BlobStorable m (f (Blobbed BlobRef f))) => MCorecursive m (Blobbed BlobRef f) where
    -- Embedding a reference into a Blobbed ref boils down to storing the reference
    membed r = Blobbed <$> storeRef r

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

instance (forall b. HasNull (ref b)) => HasNull (Blobbed ref a) where
    refNull = Blobbed refNull
    isNull (Blobbed r) = isNull r

-- | The CachedBlobbed type is equivalent to @BufferedRef@ but defined as a fixed point over `f`
--
-- A value can either be only on disk (`CBUncached`), or cached in memory (`CBCached`).
data CachedBlobbed ref f
    = CBUncached (Blobbed ref f)
    | CBCached (Blobbed ref f) (f (CachedBlobbed ref f))

cachedBlob :: CachedBlobbed ref f -> Blobbed ref f
cachedBlob (CBUncached r) = r
cachedBlob (CBCached r _) = r

type instance Base (CachedBlobbed ref f) = f

instance (Monad m, BlobStorable m (f (Blobbed BlobRef f)), Functor f) => MRecursive m (CachedBlobbed BlobRef f) where
    -- Projecting the value of a CachedBlobbed involves either projecting the value of the Blobbed field or returning the
    -- cached value.
    mproject (CBUncached r) = fmap CBUncached <$> mproject r
    mproject (CBCached _ c) = pure c

instance (Monad m, BlobStorable m (f (Blobbed BlobRef f)), Functor f) => MCorecursive m (CachedBlobbed BlobRef f) where
    -- Embedding an (f (CachedBlobbed ref f)) value into a CachedBlobbed value requires extracting the Blobbed reference
    -- and copying its embedded version to the Blobbed field of the CachedBlobbed value
    membed r = do
        b <- membed (fmap cachedBlob r)
        return (CBCached b r)

instance (forall a. Serialize (BlobRef a)) => Serialize (CachedBlobbed BlobRef f) where
    put = put . cachedBlob
    get = CBUncached <$> get

instance MonadBlobStore m => BlobStorable m (CachedBlobbed BlobRef f)

-- TODO (MRA) rename
-- | A BufferedBlobbed is a fixed point over the functor `f`
--
-- It can contain either a CachedBlobbed value or both a Blobbed value and the recursive type.
data BufferedBlobbed ref f
    = LBMemory (IORef (Blobbed ref f)) (f (BufferedBlobbed ref f))
    | LBCached (CachedBlobbed ref f)

-- | Create a BufferedBlobbed value that points to the given reference and holds the given value.
makeLBMemory :: MonadIO m => Blobbed ref f -> f (BufferedBlobbed ref f) -> m (BufferedBlobbed ref f)
makeLBMemory r a = liftIO $ do
    ref <- newIORef r
    return $ LBMemory ref a

-- | Create a BufferedBlobbed value that holds no pointer yet.
makeBufferedBlobbed :: (MonadIO m, HasNull (Blobbed ref f)) => f (BufferedBlobbed ref f) -> m (BufferedBlobbed ref f)
makeBufferedBlobbed = makeLBMemory refNull

type instance Base (BufferedBlobbed ref f) = f

instance (Monad m, BlobStorable m (f (Blobbed BlobRef f)), Functor f) => MRecursive m (BufferedBlobbed BlobRef f) where
    -- projecting a BufferedBlobbed value either means projecting the cached reference or returning the in-memory value
    mproject (LBMemory _ r) = pure r
    mproject (LBCached c) = fmap LBCached <$> mproject c
    {-# INLINE mproject #-}

instance (MonadIO m, HasNull (Blobbed ref f)) => MCorecursive m (BufferedBlobbed ref f) where
    -- embedding a value implies creating a buffered blobbed value that still doesn't hold a reference.
    membed = makeBufferedBlobbed
    {-# INLINE membed #-}

-- |Stores in-memory data to disk if it has not been stored yet and returns pointer to saved data
getBBRef :: (BlobStorable m (BufferedBlobbed BlobRef f), BlobStorable m (f (Blobbed BlobRef f)), Traversable f)
               => BufferedBlobbed BlobRef f
               -> m ((Put, BufferedBlobbed BlobRef f), Blobbed BlobRef f)
getBBRef v@(LBCached c) = (, cachedBlob c) . (, v) <$> store c
getBBRef v@(LBMemory ref _) = do
    r <- liftIO $ readIORef ref
    if isNull r
    then do
        (pu, cb) <- storeAndGetCached v
        return ((pu, LBCached cb), cachedBlob cb)
    else
        getBBRef (LBCached (CBUncached r))
    where storeAndGetCached (LBCached c) = storeUpdate c
          storeAndGetCached (LBMemory ref' t) = do
            t' <- mapM (fmap snd . storeAndGetCached) t
            rm <- liftIO $ readIORef ref'
            if (isNull rm)
            then do
                !r <- storeRef (cachedBlob <$> t')
                liftIO $ writeIORef ref' (Blobbed r)
                return (put r, CBCached (Blobbed r) t')
            else storeUpdate (CBCached rm t')

instance (MonadBlobStore m, Traversable f, BlobStorable m (f (Blobbed BlobRef f)), HasNull (Blobbed BlobRef f))
         => BlobStorable m (BufferedBlobbed BlobRef f) where
    store v = fst . fst <$> getBBRef v

    storeUpdate v = fst <$> getBBRef v

    load = return . LBCached <$> get

class FixShowable fix where
    showFix :: Functor f => (f String -> String) -> fix f -> String

instance (forall a. Show (ref a)) => FixShowable (Blobbed ref) where
    showFix _ (Blobbed r) = show r

instance (forall a. Show (ref a)) => FixShowable (CachedBlobbed ref) where
    showFix sh (CBCached r v) = "{" ++ (sh (showFix sh <$> v)) ++ "}" ++ showFix sh r
    showFix sh (CBUncached r) = showFix sh r

instance (forall a. Show (ref a)) => FixShowable (BufferedBlobbed ref) where
    showFix sh (LBMemory _ v) = "{" ++ (sh (showFix sh <$> v)) ++ "}"
    showFix sh (LBCached r) = showFix sh r

-- |Cached a fixed point value (using the 'CachedBlobbed' combinator) given
-- a function for caching the functor.
cacheCachedBlobbed :: (BlobStorable m (f (Blobbed BlobRef f)), Traversable f)
    => (forall a. f a -> m (f a))
    -- ^Function for caching the functor
    -> (CachedBlobbed BlobRef f)
    -- ^Value to cache
    -> m (CachedBlobbed BlobRef f)
cacheCachedBlobbed cacheF (CBUncached blobbed) = do
    unblobbed <- cacheF =<< mapM (cacheCachedBlobbed cacheF . CBUncached) =<< mproject blobbed
    return (CBCached blobbed unblobbed)
cacheCachedBlobbed cacheF (CBCached blobbed unblobbed) = do
    unblobbed' <- cacheF =<< mapM (cacheCachedBlobbed cacheF) unblobbed
    return (CBCached blobbed unblobbed')

-- |Cached a fixed point value (using the 'BufferedBlobbed' combinator) given
-- a function for caching the functor.
cacheBufferedBlobbed :: (BlobStorable m (f (Blobbed BlobRef f)), Traversable f)
    => (forall a. f a -> m (f a))
    -- ^Function for caching the functor
    -> BufferedBlobbed BlobRef f
    -- ^Value to cache
    -> m (BufferedBlobbed BlobRef f)
cacheBufferedBlobbed cacheF (LBMemory ref inner) = do
    inner' <- cacheF =<< mapM (cacheBufferedBlobbed cacheF) inner
    return (LBMemory ref inner')
cacheBufferedBlobbed cacheF (LBCached c) = LBCached <$> cacheCachedBlobbed cacheF c

-- BlobStorable instances
instance MonadBlobStore m => BlobStorable m IPS.IpInfo
instance MonadBlobStore m => BlobStorable m IPS.IdentityProviders
instance MonadBlobStore m => BlobStorable m ARS.ArInfo
instance MonadBlobStore m => BlobStorable m ARS.AnonymityRevokers
instance MonadBlobStore m => BlobStorable m Parameters.CryptographicParameters
-- FIXME: This uses serialization of accounts for storing them.
-- This is potentially quite wasteful when only small changes are made.
instance MonadBlobStore m => BlobStorable m Amount
instance MonadBlobStore m => BlobStorable m BakerId
instance MonadBlobStore m => BlobStorable m BakerInfo
instance MonadBlobStore m => BlobStorable m AccountIndex
instance MonadBlobStore m => BlobStorable m BS.ByteString
instance MonadBlobStore m => BlobStorable m EncryptedAmount
instance MonadBlobStore m => BlobStorable m TransactionHash
instance MonadBlobStore m => BlobStorable m ()

instance MonadBlobStore m => BlobStorable m AccountEncryptedAmount
instance (MonadBlobStore m, Serialize (PersistingAccountData pv)) => BlobStorable m (PersistingAccountData pv)
instance MonadBlobStore m => BlobStorable m Authorizations
instance MonadBlobStore m => BlobStorable m (HigherLevelKeys a)
instance MonadBlobStore m => BlobStorable m ProtocolUpdate
instance MonadBlobStore m => BlobStorable m ExchangeRate
instance MonadBlobStore m => BlobStorable m ElectionDifficulty
instance MonadBlobStore m => BlobStorable m AccountReleaseSchedule
instance MonadBlobStore m => BlobStorable m MintRate
instance MonadBlobStore m => BlobStorable m MintDistribution
instance MonadBlobStore m => BlobStorable m TransactionFeeDistribution
instance MonadBlobStore m => BlobStorable m GASRewards
instance MonadBlobStore m => BlobStorable m (Map AccountAddress Timestamp)
instance MonadBlobStore m => BlobStorable m WasmModule

newtype StoreSerialized a = StoreSerialized { unStoreSerialized :: a }
    deriving newtype (Serialize)
instance (MonadBlobStore m, Serialize a) => BlobStorable m (StoreSerialized a)
deriving newtype instance HashableTo h a => HashableTo h (StoreSerialized a)
deriving newtype instance MHashableTo m h a => MHashableTo m h (StoreSerialized a)

data HashedBufferedRef a
  = HashedBufferedRef
      { bufferedReference :: !(BufferedRef a),
        bufferedHash :: !(Maybe H.Hash)
      }

bufferHashed :: MonadIO m => Hashed a -> m (HashedBufferedRef a)
bufferHashed (Hashed val h) = do
  br <- makeBRMemory refNull val
  return $ HashedBufferedRef br (Just h)

makeHashedBufferedRef :: (MonadIO m, MHashableTo m H.Hash a) => a -> m (HashedBufferedRef a)
makeHashedBufferedRef val = do
  h <- getHashM val
  bufferHashed (Hashed val h)

instance (BlobStorable m a, MHashableTo m H.Hash a) => MHashableTo m H.Hash (HashedBufferedRef a) where
  getHashM ref = maybe (getHashM =<< refLoad ref) return (bufferedHash ref)

instance Show a => Show (HashedBufferedRef a) where
  show ref = show (bufferedReference ref) ++ maybe "" (\x -> " with hash: " ++ show x) (bufferedHash ref)

instance (BlobStorable m a, MHashableTo m H.Hash a) => BlobStorable m (HashedBufferedRef a) where
  store b =
    -- store the value if needed and then serialize the returned reference.
    getBRRef (bufferedReference b) >>= store
  load =
    -- deserialize the reference and keep it as blobbed
    fmap (flip HashedBufferedRef Nothing . BRBlobbed) <$> load
  storeUpdate (HashedBufferedRef brm _) = do
    (pt, br) <- storeUpdate brm
    h <- getHashM . fst =<< cacheBufferedRef br
    return (pt, HashedBufferedRef br (Just h))

instance (Monad m, BlobStorable m a, MHashableTo m H.Hash a) => Reference m HashedBufferedRef a where
  refFlush ref = do
    (br, r) <- flushBufferedRef (bufferedReference ref)
    return (HashedBufferedRef br (bufferedHash ref), r)

  refLoad = loadBufferedRef . bufferedReference

  refMake val = do
    br <- makeBRMemory refNull val
    h <- getHashM val
    return $ HashedBufferedRef br (Just h)

  refCache ref = do
    (val, br) <- cacheBufferedRef (bufferedReference ref)
    case bufferedHash ref of
      Nothing -> do
        h <- getHashM val
        return (val, ref {bufferedReference = br, bufferedHash = Just h})
      theHash@(Just _) -> return (val, ref {bufferedReference = br, bufferedHash = theHash})

  refUncache ref = do
    br <- uncacheBuffered (bufferedReference ref)
    return $ ref {bufferedReference = br}
  {-# INLINE refFlush #-}
  {-# INLINE refLoad #-}
  {-# INLINE refMake #-}
  {-# INLINE refCache #-}
  {-# INLINE refUncache #-}

instance (BlobStorable m a, MHashableTo m H.Hash a) => BlobStorable m (Nullable (HashedBufferedRef a)) where
    store Null = return $ put (refNull :: BlobRef a)
    store (Some v) = store v
    load = do
        (r :: BlobRef a) <- get
        if isNull r then
            return (pure Null)
        else
            return $ pure $ Some $ HashedBufferedRef (BRBlobbed r) Nothing
    storeUpdate n@Null = return (put (refNull :: BlobRef a), n)
    storeUpdate (Some v) = do
        (r, v') <- storeUpdate v
        return (r, Some v')

-- |This class abstracts values that can be cached in some monad.
class Cacheable m a where
    -- |Recursively cache a value of type @a@.
    cache :: a -> m a
    default cache :: (Applicative m) => a -> m a
    cache = pure
instance (Applicative m, Cacheable m a) => Cacheable m (Nullable a) where
    cache Null = pure Null
    cache (Some v) = Some <$> cache v

instance (BlobStorable m a, Cacheable m a) => Cacheable m (BufferedRef a) where
    cache BRBlobbed{..} = do
        brValue <- cache =<< loadRef brRef
        brIORef <- liftIO $ newIORef brRef
        return BRMemory{..}
    cache br@BRMemory{..} = do
        cachedVal <- cache brValue
        return br{brValue = cachedVal}

instance (MHashableTo m H.Hash a, BlobStorable m a, Cacheable m a) => Cacheable m (HashedBufferedRef a) where
  cache (HashedBufferedRef ref Nothing) = do
    ref' <- cache ref
    hsh <- getHashM =<< refLoad ref'
    return (HashedBufferedRef ref' (Just hsh))
  cache (HashedBufferedRef ref hsh) = do
    ref' <- cache ref
    return (HashedBufferedRef ref' hsh)

instance (Applicative m, Cacheable m a, Cacheable m b) => Cacheable m (a, b) where
    cache (x, y) = (,) <$> cache x <*> cache y

instance Applicative m => Cacheable m () where
    cache _ = pure ()

-- Required for caching PersistentAccount
instance (Applicative m) => Cacheable m EncryptedAmount
instance (Applicative m) => Cacheable m AccountReleaseSchedule
instance (Applicative m) => Cacheable m (Map AccountAddress Timestamp)
instance (Applicative m) => Cacheable m WasmModule
instance (Applicative m) => Cacheable m (PersistingAccountData pv)
-- Required for caching AccountIndexes
instance (Applicative m) => Cacheable m AccountIndex
-- Required for caching BlockStatePointers
instance (Applicative m) => Cacheable m IPS.IdentityProviders
instance (Applicative m) => Cacheable m ARS.AnonymityRevokers
instance (Applicative m) => Cacheable m Parameters.CryptographicParameters
-- Required for caching Bakers
instance (Applicative m) => Cacheable m BakerInfo
instance (Applicative m) => Cacheable m Amount
-- Required for caching Updates
instance (Applicative m) => Cacheable m (StoreSerialized a)
