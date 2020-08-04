{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

--  ( -- * Nullable
--    Nullable (..),
--
--    -- * BlobStore
--    BlobStore (..),
--    HasBlobStore (..),
--    createBlobStore,
--    loadBlobStore,
--    flushBlobStore,
--    closeBlobStore,
--    destroyBlobStore,
--
--    -- * MonadBlobStore
--    MonadBlobStore (..),
--
--    -- * BlobStorable
--    BlobStorable (..),
--
--    -- * Basic references
--
--    -- ** BlobRef
--    BlobRef,
--
--    -- ** BufferedRef
--    BufferedRef (..),
--    makeBufferedRef,
--    loadBufferedRef,
--    flushBufferedRef,
--    cacheBufferedRef,
--    uncacheBuffered,
--
--    -- ** HashedBufferedRef
--    HashedBufferedRef (..),
--    makeHashedBufferedRef,
--    makeHashedBufferedRefM,
--    loadHashedBufferedRef,
--    flushHashedBufferedRef,
--    cacheHashedBufferedRef,
--    uncacheHashedBuffered,
--
--    -- * Fixed point references
--
--    -- **  BufferedBlobbed
--    BufferedBlobbed (..),
--    makeBufferedBlobbed,
--
--    -- ** FixShowable
--    FixShowable (..),
--
--    -- * For testing
--    SerializeStorable (..),
--    CachedBlobbed (..),
--    runBlobStoreTemp,
--  )

import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Account
import qualified Concordium.GlobalState.AnonymityRevokers as ARS
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.IdentityProviders as IPS
import qualified Concordium.GlobalState.Parameters as Parameters
import Concordium.GlobalState.Persistent.MonadicRecursive
import Concordium.Types
import Concordium.Types.HashableTo
import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.ByteString as BS
import Data.Functor.Foldable
import Data.IORef
import Data.Proxy
import Data.Serialize
import Data.Word
import GHC.Stack
import System.Directory
import System.IO

{-
-------------------------------------------------------------------------------
                           Nullable values
-------------------------------------------------------------------------------
-}

-- | A type that is an instance of @HasNull@ has a distinguished value that is considered a Null value.
class HasNull ref where
  refNull :: ref
  isNull :: ref -> Bool

{-
-------------------------------------------------------------------------------
                              Nullable
-------------------------------------------------------------------------------
-}

-- | A value that can be empty or contain another value. It is equivalent to `Maybe` but
-- strict on its constructors and its `Serialize` instance depends on the inner type having
-- a special @null@ value.
data Nullable v = Null | Some !v
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Serialization is equivalent to that of the @ref@ as there
-- is a special value for a null reference, i.e. @ref@ is @HasNull@
instance (HasNull (ref a), Serialize (ref a)) => Serialize (Nullable (ref a)) where
  put Null = put (refNull :: ref a)
  put (Some v) = put v
  get = do
    r <- get
    return $! if isNull r then Null else Some r

instance HasNull (Nullable a) where
  refNull = Null
  isNull Null = True
  isNull _ = False

instance (MHashableTo m H.Hash v) => MHashableTo m H.Hash (Nullable v) where
  getHashM Null = return $ H.hash ""
  getHashM (Some v) = getHashM v

{-
-------------------------------------------------------------------------------
                                  Offset
-------------------------------------------------------------------------------
-}

-- | A BlobRef represents an offset on a file
newtype BlobRef a = BlobRef Word64
  deriving (Eq, Ord, Serialize)

instance Show (BlobRef a) where
  show (BlobRef v) = '@' : show v

instance HasNull (BlobRef a) where
  refNull = BlobRef maxBound
  isNull = (== refNull)

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef (BlobRef a)

{-
-------------------------------------------------------------------------------
                              Storage context
-------------------------------------------------------------------------------
-}

-- | The handler for the BlobStore file
data BlobHandle
  = BlobHandle
      { -- | File handle that should be opened in read/write mode.
        bhHandle :: !Handle,
        -- | Whether we are already at the end of the file, to avoid the need to seek on writes.
        bhAtEnd :: !Bool,
        -- | Current size of the file.
        bhSize :: !Int
      }

-- | The storage context
data BlobStore
  = BlobStore
      { blobStoreFile :: !(MVar BlobHandle),
        blobStoreFilePath :: !FilePath
      }

class HasBlobStore a where
  blobStore :: a -> BlobStore

instance HasBlobStore BlobStore where
  blobStore = id

-- | Create a new blob store at a given location.
--  Fails if a file or directory at that location already exists.
createBlobStore :: FilePath -> IO BlobStore
createBlobStore blobStoreFilePath = do
  pathEx <- doesPathExist blobStoreFilePath
  when pathEx $ throwIO (userError $ "Blob store path already exists: " ++ blobStoreFilePath)
  bhHandle <- openBinaryFile blobStoreFilePath ReadWriteMode
  blobStoreFile <- newMVar BlobHandle {bhSize = 0, bhAtEnd = True, ..}
  return BlobStore {..}

-- | Load an existing blob store from a file.
--  The file must be readable and writable, but this is not checked here.
loadBlobStore :: FilePath -> IO BlobStore
loadBlobStore blobStoreFilePath = do
  bhHandle <- openBinaryFile blobStoreFilePath ReadWriteMode
  bhSize <- fromIntegral <$> hFileSize bhHandle
  blobStoreFile <- newMVar BlobHandle {bhAtEnd = bhSize == 0, ..}
  return BlobStore {..}

-- | Flush all buffers associated with the blob store,
--  ensuring all the contents is written out.
flushBlobStore :: BlobStore -> IO ()
flushBlobStore BlobStore {..} =
  withMVar blobStoreFile (hFlush . bhHandle)

-- | Close all references to the blob store, flushing it
--  in the process.
closeBlobStore :: BlobStore -> IO ()
closeBlobStore BlobStore {..} = do
  BlobHandle {..} <- takeMVar blobStoreFile
  hClose bhHandle

-- | Close all references to the blob store and delete the backing file.
destroyBlobStore :: BlobStore -> IO ()
destroyBlobStore bs@BlobStore {..} = do
  closeBlobStore bs
  removeFile blobStoreFilePath

-- | Run a computation with temporary access to the blob store.
--  The given FilePath is a directory where the temporary blob
--  store will be created.
--  The blob store file is deleted afterwards.
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
readBlobBS BlobStore {..} (BlobRef offset) = mask $ \restore -> do
  bh@BlobHandle {..} <- takeMVar blobStoreFile
  eres <- try
    $ restore
    $ do
      hSeek bhHandle AbsoluteSeek (fromIntegral offset)
      esize <- decode <$> BS.hGet bhHandle 8
      case esize :: Either String Word64 of
        Left e -> error e
        Right size -> BS.hGet bhHandle (fromIntegral size)
  putMVar blobStoreFile bh {bhAtEnd = False}
  case eres :: Either SomeException BS.ByteString of
    Left e -> throwIO e
    Right bs -> return bs

-- | Write a bytestring into the blob store and return the offset
writeBlobBS :: BlobStore -> BS.ByteString -> IO (BlobRef a)
writeBlobBS BlobStore {..} bs = mask $ \restore -> do
  bh@BlobHandle {bhHandle = writeHandle, bhAtEnd = atEnd} <- takeMVar blobStoreFile
  eres <- try
    $ restore
    $ do
      unless atEnd (hSeek writeHandle SeekFromEnd 0)
      BS.hPut writeHandle size
      BS.hPut writeHandle bs
  case eres :: Either SomeException () of
    Left e -> do
      -- In case of an exception, query for the size and assume we are not at the end.
      fSize <- hFileSize writeHandle
      putMVar blobStoreFile bh {bhSize = fromInteger fSize, bhAtEnd = False}
      throwIO e
    Right _ -> do
      putMVar blobStoreFile bh {bhSize = bhSize bh + 8 + BS.length bs, bhAtEnd = True}
      return (BlobRef (fromIntegral (bhSize bh)))
  where
    size = encode (fromIntegral (BS.length bs) :: Word64)

{-
-------------------------------------------------------------------------------
                              MonadBlobStore
-------------------------------------------------------------------------------
-}

-- | An instance `MonadBlobStore m ref` specifies that the monad `m` can
-- store values under references of type `ref`.
class (Monad m, forall a. Serialize (ref a)) => MonadBlobStore m ref where
  storeRaw :: BS.ByteString -> m (ref a)
  loadRaw :: ref a -> m BS.ByteString
  storeBlob :: (Serialize a) => a -> m (ref a)
  storeBlob = storeRaw . encode
  loadBlob :: (Serialize a) => ref a -> m a
  loadBlob r = do
    bs <- loadRaw r
    case decode bs of
      Left e -> error e
      Right v -> return v

instance (MonadIO m, MonadReader r m, HasBlobStore r) => MonadBlobStore m BlobRef where
  storeRaw b = do
    bs <- blobStore <$> ask
    liftIO $ writeBlobBS bs b
  loadRaw r = do
    bs <- blobStore <$> ask
    liftIO $ readBlobBS bs r

{-
-------------------------------------------------------------------------------
                            BlobStorable m ref a
-------------------------------------------------------------------------------
-}

-- | The @BlobStorable m ref a@ class defines how a value
--  of type @a@ may be stored as in a reference of type @ref a@
--  in the monad @m@.
--
--  Where @a@ is an instance of 'Serialize', default implementations
--  are provided for 'store' and 'load' that simply (de)serialize
--  the value.  For a complex datatype that uses internal pointers,
--  'store' and 'load' are expected to translate between such pointers
--  and references in the underlying store.
--
--  Note that the functions `store` and `load` are somewhat equivalent to
--  `put` and `get` but working on references so that they can be written
--  to the disk.
class (MonadBlobStore m ref) => BlobStorable m ref a where
  -- | Serialize a value of type @a@ for storage.
  store :: Proxy ref -> a -> m Put
  default store :: (Serialize a) => Proxy ref -> a -> m Put
  store _ = pure . put

  -- | Deserialize a value of type @a@ from storage.
  load :: Proxy ref -> Get (m a)
  default load :: (Serialize a) => Proxy ref -> Get (m a)
  load _ = pure <$> get

  -- | Store a value of type @a@, possibly updating its representation.
  --  This is used when the value's representation includes pointers that
  --  may be replaced or supplemented with blob references.
  storeUpdate :: Proxy ref -> a -> m (Put, a)
  storeUpdate p v = (,v) <$> store p v

  -- | Store a new value of type @a@ returning the generated reference
  storeRef :: a -> m (ref a)
  storeRef v = do
    p <- runPut <$> store (Proxy :: Proxy ref) v
    storeRaw p

  -- | Store the value of type @a@ and return a reference together with
  --  the possibly updated value.
  storeUpdateRef :: a -> m (ref a, a)
  storeUpdateRef v = do
    (p, v') <- storeUpdate (Proxy :: Proxy ref) v
    (,v') <$> storeRaw (runPut p)

  -- | Load the raw bytestring at the referenced location and deserialize
  --  the value.
  loadRef :: HasCallStack => ref a -> m a
  loadRef ref = do
    bs <- loadRaw ref
    case runGet (load (Proxy :: Proxy ref)) bs of
      Left e -> error (e ++ " :: " ++ show bs)
      Right !mv -> mv

instance (MonadIO m, MonadBlobStore m BlobRef, BlobStorable m BlobRef a, BlobStorable m BlobRef b) => BlobStorable m BlobRef (a, b) where
  storeUpdate p (a, b) = do
    (pa, a') <- storeUpdate p a
    (pb, b') <- storeUpdate p b
    let pab = pa >> pb
    return (pab, (a', b'))

  store p v = fst <$> storeUpdate p v

  load p = do
    ma <- load p
    mb <- load p
    return $ do
      a <- ma
      b <- mb
      return (a, b)

-------------------------------------------------
-- BlobStorable instances that are defined by its `Serialize` instance
-------------------------------------------------

-- If a monad can create references of type @ref@ then it can store values of type
-- @Nullable (ref a)@ into @ref@s as the representation is the same than for the @ref@ value.
instance (HasNull (ref a), MonadBlobStore m ref) => BlobStorable m ref (Nullable (ref a))

instance (MonadBlobStore m ref) => BlobStorable m ref IPS.IdentityProviders

instance (MonadBlobStore m ref) => BlobStorable m ref ARS.AnonymityRevokers

instance (MonadBlobStore m ref) => BlobStorable m ref Parameters.CryptographicParameters

-- FIXME: This uses serialization of accounts for storing them.
-- This is potentially quite wasteful when only small changes are made.
instance (MonadBlobStore m ref) => BlobStorable m ref Account

instance (MonadBlobStore m ref) => BlobStorable m ref Amount

instance (MonadBlobStore m ref) => BlobStorable m ref BakerId

instance (MonadBlobStore m ref) => BlobStorable m ref BakerInfo

instance (MonadBlobStore m ref) => BlobStorable m ref Word64

instance (MonadBlobStore m ref) => BlobStorable m ref BS.ByteString

instance (MonadBlobStore m ref) => BlobStorable m ref EncryptedAmount

-- TODO (MRA) this is ad-hoc but it will be removed when we implement a bufferedref list for EncryptedAmount
instance (MonadBlobStore m ref) => BlobStorable m ref [EncryptedAmount]

instance (MonadBlobStore m ref) => BlobStorable m ref PersistingAccountData

{-
-------------------------------------------------------------------------------
                              BufferedRef
-------------------------------------------------------------------------------
-}

-- | A value that may exists purely on disk ('BRBlobbed'), purely in memory ('BRMemory' with @brIORef = refNull@)
--  , or both ('BRMemory' with @brIORef /= refNull@). When the value is cached, the cached value must match
--  the value stored on disk.
data BufferedRef a
  = -- | Value stored on disk
    BRBlobbed {brRef :: !(BlobRef a)}
  | -- | Value stored in memory and possibly on disk.
    --  When a new 'BRMemory' instance is created, we initialize 'brIORef' to 'refNull'.
    --  When we store the instance in persistent storage, we update 'brIORef' with the corresponding pointer.
    --  That way, when we store the same instance again on disk (this could be, e.g., a child block
    --  that inherited its parent's state) we can store the pointer to the 'brValue' data rather than
    --  storing all of the data again.
    BRMemory {brIORef :: !(IORef (BlobRef a)), brValue :: !a}

-- | Create a @BRMemory@ value in a @MonadIO@ context with the provided values
makeBRMemory :: (MonadIO m) => BlobRef a -> a -> m (BufferedRef a)
makeBRMemory r a = do
  ref <- liftIO $ newIORef r
  return $ BRMemory ref a

-- | Create a @BRMemory@ value with a null reference (so the value is just in memory)
makeBufferedRef :: (MonadIO m) => a -> m (BufferedRef a)
makeBufferedRef = makeBRMemory refNull

-- | Load the value from a @BufferedRef@ not caching it.
loadBufferedRef :: (BlobStorable m BlobRef a, MonadIO m) => BufferedRef a -> m a
loadBufferedRef = refLoad

-- | Load a 'BufferedRef' and cache it if it wasn't already in memory.
cacheBufferedRef :: (BlobStorable m BlobRef a, MonadIO m) => BufferedRef a -> m (a, BufferedRef a)
cacheBufferedRef = refCache

-- | If given a Blobbed reference, do nothing. Otherwise if needed store the value.
flushBufferedRef :: (BlobStorable m BlobRef a, MonadIO m) => BufferedRef a -> m (BufferedRef a, BlobRef a)
flushBufferedRef = refFlush

-- | Convert a Cached reference into a Blobbed one storing the data if needed.
uncacheBuffered :: (BlobStorable m BlobRef a, MonadIO m) => BufferedRef a -> m (BufferedRef a)
uncacheBuffered = refUncache

-- | Stores in-memory data to disk if it has not been stored yet and returns pointer to saved data
getBRRef :: (BlobStorable m BlobRef a, MonadIO m) => BufferedRef a -> m (BlobRef a)
getBRRef (BRMemory ref v) = do
  r <- liftIO $ readIORef ref
  if isNull r
    then do
      (r' :: BlobRef a) <- storeRef v
      liftIO . writeIORef ref $! r'
      return r'
    else return r
getBRRef (BRBlobbed r) = return r

instance Show a => Show (BufferedRef a) where
  show (BRBlobbed r) = show r
  show (BRMemory _ v) = "{" ++ show v ++ "}"

-- | When a monad can store values of type @a@ into references of type @BlobRef@ (offsets),
-- it can also store @BufferedRef@s into references of type @BlobRef@.
instance (BlobStorable m BlobRef a, MonadIO m) => BlobStorable m BlobRef (BufferedRef a) where
  store p b =
    -- store the value if needed and then serialize the returned reference.
    getBRRef b >>= store p
  load p =
    -- deserialize the reference and keep it as blobbed
    fmap BRBlobbed <$> load p
  storeUpdate p brm@(BRMemory ref v) = do
    r <- liftIO $ readIORef ref
    if isNull r
      then do
        -- when we hadn't yet stored the reference, store it (possibly modifying other pointers inside)
        -- and return the updated value.
        (r' :: BlobRef a, v') <- storeUpdateRef v
        liftIO . writeIORef ref $! r'
        (,BRMemory ref v') <$> store p r'
      else-- if the value was already stored, we have to store it again updated and return the same
      -- buffered ref (note that store will mutate the IORef value inside it)
        (,brm) <$> store p brm
  storeUpdate p x =
    -- If the value has not yet been read from the disk and is only a BRBlobbed value,
    -- we shall get the serialized reference (TODO js: this will not update the value??)
    (,x) <$> store p x

-- | This instance just wraps the `BlobStorable m BlobRef (BufferedRef a)` instance as
-- @BlobRef@ is an instance of @HasNull@.
instance (BlobStorable m BlobRef a, MonadIO m) => BlobStorable m BlobRef (Nullable (BufferedRef a)) where
  store _ Null = return $ put (refNull :: BlobRef a)
  store p (Some v) = store p v
  load p = do
    (r :: BlobRef a) <- get
    if isNull r
      then return (pure Null)
      else fmap Some <$> load p
  storeUpdate _ n@Null = return (put (refNull :: BlobRef a), n)
  storeUpdate p (Some v) = do
    (r, v') <- storeUpdate p v
    return (r, Some v')

instance (BlobStorable m BlobRef a, MonadIO m) => Reference m BufferedRef a where
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

instance (MonadIO m, BlobStorable m BlobRef a, MHashableTo m H.Hash a) => MHashableTo m H.Hash (BufferedRef a) where
  getHashM ref = getHashM =<< refLoad ref

instance (MonadIO m, Serialize a, Serialize b, BlobStorable m BlobRef a) => MHashableTo m H.Hash (BufferedRef a, b) where
  getHashM (a, b) = do
    val <- encode <$> refLoad a
    return $ H.hash (val <> encode b)

instance (BlobStorable m BlobRef a, BlobStorable m BlobRef b, MonadIO m) => BlobStorable m BlobRef (Nullable (BufferedRef a, b)) where
  store _ Null = return $ put (refNull :: BlobRef a)
  store p (Some v) = store p v
  load p = do
    (r :: BlobRef a) <- get
    if isNull r
      then return (pure Null)
      else fmap Some <$> load p
  storeUpdate _ n@Null = return (put (refNull :: BlobRef a), n)
  storeUpdate p (Some v) = do
    (r, v') <- storeUpdate p v
    return (r, Some v')

{-
-------------------------------------------------------------------------------
                              HashedBufferedRef
-------------------------------------------------------------------------------
-}

data HashedBufferedRef a
  = HashedBufferedRef
      { bufferedReference :: BufferedRef a,
        bufferedHash :: Maybe H.Hash
      }

bufferHashed :: (MonadIO m) => Hashed a -> m (HashedBufferedRef a)
bufferHashed (Hashed val h) = do
  br <- makeBRMemory refNull val
  return $ HashedBufferedRef br (Just h)

instance (MonadIO m, BlobStorable m BlobRef a, MHashableTo m H.Hash a) => MHashableTo m H.Hash (HashedBufferedRef a) where
  getHashM ref = maybe (getHashM =<< refLoad ref) return (bufferedHash ref)

instance Show a => Show (HashedBufferedRef a) where
  show ref = show (bufferedReference ref) ++ maybe "" (\x -> " with hash: " ++ show x) (bufferedHash ref)

instance (MonadIO m, MonadBlobStore m BlobRef, BlobStorable m BlobRef a, MHashableTo m H.Hash a) => BlobStorable m BlobRef (HashedBufferedRef a) where
  store p b =
    -- store the value if needed and then serialize the returned reference.
    getBRRef (bufferedReference b) >>= store p
  load p =
    -- deserialize the reference and keep it as blobbed
    fmap (flip HashedBufferedRef Nothing . BRBlobbed) <$> load p
  storeUpdate p (HashedBufferedRef brm _) = do
    (pt, br) <- storeUpdate p brm
    h <- getHashM . fst =<< cacheBufferedRef br
    return (pt, HashedBufferedRef br (Just h))

class (Monad m) => Reference m ref a where
  refFlush :: ref a -> m (ref a, BlobRef a)
  refCache :: ref a -> m (a, ref a)
  refLoad :: ref a -> m a
  refMake :: a -> m (ref a)
  refUncache :: ref a -> m (ref a)

instance (MHashableTo m H.Hash a, BlobStorable m BlobRef a, MonadIO m) => Reference m HashedBufferedRef a where
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
    h <- getHashM val
    return (val, ref {bufferedReference = br, bufferedHash = bufferedHash ref <|> Just h})

  refUncache ref = do
    br <- uncacheBuffered (bufferedReference ref)
    return $ ref {bufferedReference = br}

{-
-------------------------------------------------------------------------------
                              Blobbed
-------------------------------------------------------------------------------
-}

-- | Blobbed is a fixed point of the functor `f` wrapped in references of type @ref@
newtype Blobbed ref f = Blobbed {unblobbed :: ref (f (Blobbed ref f))}

instance HasNull (Blobbed BlobRef a) where
  refNull = Blobbed refNull
  isNull (Blobbed r) = r == refNull

-- Serialize instances, just wrap the Serialize instances of the underlying reference
deriving instance (forall a. Serialize (ref a)) => Serialize (Blobbed ref f)

instance (forall a. Serialize (Nullable (ref a))) => Serialize (Nullable (Blobbed ref f)) where
  put = put . fmap unblobbed
  get = fmap Blobbed <$> get

-- If a monad can manage references of type @ref@ then it can store values of type
-- @Blobbed ref f@ (just by serializing the inner references) into references of type
-- @ref@
instance (MonadBlobStore m ref) => BlobStorable m ref (Blobbed ref f)

-- If a monad can store references of type @ref@ and a reference is serializable and nullable,
-- then it can store values of type @Nullable (Blobbed ref f)@ into references of type @ref@
instance (MonadBlobStore m ref, forall a. Serialize (Nullable (ref a))) => BlobStorable m ref (Nullable (Blobbed ref f))

type instance Base (Blobbed ref f) = f

instance (Monad m, BlobStorable m ref (f (Blobbed ref f))) => MRecursive m (Blobbed ref f) where
  -- Projecting the blobbed reference boils down to load the value it contains
  mproject (Blobbed r) = loadRef r

instance (Monad m, BlobStorable m ref (f (Blobbed ref f))) => MCorecursive m (Blobbed ref f) where
  -- Embedding a reference into a Blobbed ref boils down to storing the reference
  membed r = Blobbed <$> storeRef r

{-
-------------------------------------------------------------------------------
                              CachedBlobbed
-------------------------------------------------------------------------------
-}

-- | The CachedBlobbed type is equivalent to @BufferedRef@ but defined as a fixed point over `f`
--
-- A value can either be only on disk (`CBUncached`), or cached in memory (`CBCached`).
data CachedBlobbed ref f
  = CBUncached
      { cachedBlob :: Blobbed ref f
      }
  | CBCached
      { cachedBlob :: Blobbed ref f,
        value :: f (CachedBlobbed ref f)
      }
{-# WARNING CachedBlobbed "This is an internal type only exported publicly for testing purposes" #-}

type instance Base (CachedBlobbed ref f) = f

instance (MonadBlobStore m ref, BlobStorable m ref (f (Blobbed ref f)), Functor f) => MRecursive m (CachedBlobbed ref f) where
  -- Projecting the value of a CachedBlobed involves either projecting the value of the Blobbed field or returning the
  -- cached value.
  mproject (CBUncached r) = fmap CBUncached <$> mproject r
  mproject (CBCached _ c) = pure c

instance (MonadBlobStore m ref, BlobStorable m ref (f (Blobbed ref f)), Functor f) => MCorecursive m (CachedBlobbed ref f) where
  -- Embedding an (f (CachedBlobbed ref f)) value into a CachedBlobbed value requires extracting the Blobbed reference
  -- and copying its embeded version to the Blobbed field of the CachedBlobbed value
  membed r = do
    b <- membed (fmap cachedBlob r)
    return (CBCached b r)

instance (forall a. Serialize (ref a)) => Serialize (CachedBlobbed ref f) where
  put = put . cachedBlob
  get = CBUncached <$> get

instance (MonadBlobStore m ref) => BlobStorable m ref (CachedBlobbed ref f)

{-
-------------------------------------------------------------------------------
                              BufferedBlobbed
-------------------------------------------------------------------------------
-}

-- TODO (MRA) rename

-- | A BufferedBlobbed is a fixed point over the functor `f`
--
-- It can contain either a CachedBlobbed value or both a Blobbed value and the recursive type.
data BufferedBlobbed ref f
  = LBMemory (IORef (Blobbed ref f)) (f (BufferedBlobbed ref f))
  | LBCached (CachedBlobbed ref f)

-- | Create a BufferedBlobbed value that points to the given reference and holds the given value.
makeLBMemory :: (MonadIO m) => Blobbed ref f -> f (BufferedBlobbed ref f) -> m (BufferedBlobbed ref f)
makeLBMemory r a = liftIO $ do
  ref <- newIORef r
  return $ LBMemory ref a

-- | Create a BufferedBlobbed value that holds no pointer yet.
makeBufferedBlobbed :: (MonadIO m, HasNull (Blobbed ref f)) => f (BufferedBlobbed ref f) -> m (BufferedBlobbed ref f)
makeBufferedBlobbed = makeLBMemory refNull

type instance Base (BufferedBlobbed ref f) = f

instance (MonadBlobStore m ref, BlobStorable m ref (f (Blobbed ref f)), Functor f) => MRecursive m (BufferedBlobbed ref f) where
  -- projecting a BufferefBlobbed value either means projecting the cached reference or returning the in-memory value
  mproject (LBMemory _ r) = pure r
  mproject (LBCached c) = fmap LBCached <$> mproject c
  {-# INLINE mproject #-}

instance (MonadIO m, HasNull (Blobbed ref f)) => MCorecursive m (BufferedBlobbed ref f) where
  -- embedding a value implies creating a buffered blobbed value that still doesn't hold a reference.
  membed = makeBufferedBlobbed
  {-# INLINE membed #-}

-- | Stores in-memory data to disk if it has not been stored yet and returns pointer to saved data
getBBRef ::
  (BlobStorable m ref (BufferedBlobbed ref f), BlobStorable m ref (f (Blobbed ref f)), MonadIO m, HasNull (Blobbed ref f), Traversable f) =>
  Proxy ref ->
  BufferedBlobbed ref f ->
  m (Put, BufferedBlobbed ref f)
getBBRef p v@(LBCached c) = (,v) <$> store p c
getBBRef p v@(LBMemory ref _) = do
  r <- liftIO $ readIORef ref
  if isNull r
    then do
      (pu, cb) <- storeAndGetCached v
      return (pu, LBCached cb)
    else getBBRef p (LBCached (CBUncached r))
  where
    -- storeAndGetCached :: BufferedBlobbed ref f -> m (Put, CachedBlobbed ref f)
    storeAndGetCached (LBCached c) = storeUpdate p c
    storeAndGetCached (LBMemory ref' t) = do
      t' <- mapM (fmap snd . storeAndGetCached) t
      rm <- liftIO $ readIORef ref'
      if isNull rm
        then do
          !r <- storeRef (cachedBlob <$> t')
          liftIO $ writeIORef ref' (Blobbed r)
          return (put r, CBCached (Blobbed r) t')
        else storeUpdate p (CBCached rm t')

instance
  (MonadIO m, MonadBlobStore m ref, Traversable f, BlobStorable m ref (f (Blobbed ref f)), HasNull (Blobbed ref f)) =>
  BlobStorable m ref (BufferedBlobbed ref f)
  where
  store p v = fst <$> getBBRef p v

  storeUpdate p v = getBBRef p v

  load _ = return . LBCached <$> get

{-
-------------------------------------------------------------------------------
                              FixShowable
-------------------------------------------------------------------------------
-}

-- | FixShowable generalizes `Show` for fixed points over the functor `f`
class FixShowable fix where
  showFix :: Functor f => (f String -> String) -> fix f -> String

instance (forall a. Show (ref a)) => FixShowable (Blobbed ref) where
  showFix _ (Blobbed r) = show r

instance (forall a. Show (ref a)) => FixShowable (CachedBlobbed ref) where
  showFix sh (CBCached r v) = "{" ++ sh (showFix sh <$> v) ++ "}" ++ showFix sh r
  showFix sh (CBUncached r) = showFix sh r

instance (forall a. Show (ref a)) => FixShowable (BufferedBlobbed ref) where
  showFix sh (LBMemory _ v) = "{" ++ sh (showFix sh <$> v) ++ "}"
  showFix sh (LBCached r) = showFix sh r

{-
-------------------------------------------------------------------------------
                          For testing purposes
-------------------------------------------------------------------------------
-}

-- | Newtype providing a @BlobStorable@ reference for every wrapped type
--  that is an instance of @Serialize@
newtype SerializeStorable v = SerStore v
  deriving newtype (Eq, Ord, Show, Serialize)
{-# WARNING SerializeStorable "only intended to use in tests to provide fast testing instances" #-}

-- Every @SerializeStorable@ value will be serialized with the default implementation
instance (Serialize v, MonadBlobStore m ref) => BlobStorable m ref (SerializeStorable v)
