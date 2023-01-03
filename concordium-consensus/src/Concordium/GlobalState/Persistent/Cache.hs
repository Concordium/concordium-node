{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.Cache where

import Control.Concurrent.MVar
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict (WriterT)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.Proxy
import qualified Data.Vector.Primitive.Mutable as Vec

import Concordium.Utils.Serialization.Put (PutT)

import Concordium.GlobalState.Persistent.BlobStore (BlobRef (..), BlobStoreT, HasNull (..))

-- |The 'HasCache' class supports projecting a cache of a particular type.
-- This is used to access a cache given a context.
class HasCache cache r where
    -- |Project the cache.
    projectCache :: r -> cache

-- |The 'MonadCache' combines 'MonadIO' with an operation for getting the cache.
-- This is used as a constraint in the 'Cache' class.
class MonadIO m => MonadCache c m where
    -- |Get the cache.
    getCache :: m c
    default getCache :: (MonadReader r m, HasCache c r) => m c
    getCache = asks projectCache
    {-# INLINE getCache #-}

instance MonadCache c m => MonadCache c (StateT s m) where
    getCache = lift getCache
    {-# INLINE getCache #-}

instance (Monoid w, MonadCache c m) => MonadCache c (WriterT w m) where
    getCache = lift getCache
    {-# INLINE getCache #-}

instance (MonadCache c m) => MonadCache c (ExceptT e m) where
    -- Note that exceptions will not cause rollbacks in the cache.
    getCache = lift getCache
    {-# INLINE getCache #-}

instance (HasCache c r, MonadIO m) => MonadCache c (BlobStoreT r m) where
    getCache = asks projectCache
    {-# INLINE getCache #-}

instance (MonadCache c m) => MonadCache c (PutT m) where
    getCache = lift getCache
    {-# INLINE getCache #-}

instance MonadCache c m => MonadCache c (ReaderT s m) where
    getCache = lift getCache
    {-# INLINE getCache #-}

-- | A cache that stores values identified by (unique) keys.
class Cache c where
    -- |The key used to identify a cache entry.
    type CacheKey c

    -- |A cached value.
    type CacheValue c

    -- |Construct a new cache. The parameter specifies the expected size number of entries in the
    -- cache. Implementations are not required to respect the size.
    newCache :: Int -> IO c

    -- |Resize the cache to a minimally-sized cache. It is not expected that any cached values
    -- should be retained.
    collapseCache :: (MonadCache c m) => Proxy c -> m ()

    -- |Store a value in the cache with the given key.
    -- The cache will not store more than one entry per key.
    -- If an entry is already present for the key, then this should replace the entry.
    putCachedValue :: (MonadCache c m) => Proxy c -> CacheKey c -> CacheValue c -> m (CacheValue c)

    -- |Load a cached value with the given key.
    -- This returns 'Nothing' if the value is not in the cache.
    -- Any value that is returned must have previously been inserted in the cache by
    -- 'putCachedValue'.
    lookupCachedValue :: (MonadCache c m) => Proxy c -> CacheKey c -> m (Maybe (CacheValue c))

    -- |Return the number of entries that can be stored in the cache.
    getCacheSize :: (MonadCache c m) => Proxy c -> m Int

-- | A context that simply wraps a cache, providing an instance @HasCache c (CacheContext c)@.
newtype CacheContext c = CacheContext {theCacheContext :: c}

instance HasCache c (CacheContext c) where
    projectCache = theCacheContext

-- |A null cache that does not store any values.
-- That is, all lookups are cache misses.
data NullCache (v :: Type) = NullCache

instance Cache (NullCache v) where
    type CacheKey (NullCache v) = BlobRef v
    type CacheValue (NullCache v) = v

    newCache = newNullCache
    collapseCache _ = return ()
    putCachedValue _ _ = return
    lookupCachedValue _ _ = return Nothing
    getCacheSize _ = return 0

-- |Construct a new 'NullCache'. The size parameter is ignored.
newNullCache :: Int -> IO (NullCache v)
newNullCache _ = pure NullCache

-- |First-in, first-out cache, with entries keyed by 'BlobRef's.
-- 'refNull' is considered an invalid key, and should not be inserted in the cache.
-- Internally, 'BlobRef' keys are converted to 'Int's so that we can make use of
-- 'IntMap.IntMap'. This relies on lossless conversion from 'Word64' to 'Int', which
-- is the case on 64-bit GHC platforms.
data FIFOCache' v = FIFOCache'
    { -- |Map from keys to values that are stored in the cache.
      -- Each entry in the map should have a corresponding entry in the 'fifoBuffer' vector.
      keyMap :: !(IntMap.IntMap v),
      -- |Vector of cached entries. Each (non-'Empty') entry must correspond to an entry
      -- in the 'keyMap'.
      fifoBuffer :: !(Vec.IOVector Int),
      -- | The next index to use, 0 <= nextIndex < Vec.length fifoBuffer
      nextIndex :: !Int
    }

-- |Convert a 'BlobRef' to an 'Int'.
cacheEntry :: BlobRef a -> Int
cacheEntry = fromIntegral . theBlobRef

-- |A cache entry that is a non-valid 'BlobRef'.
nullCacheEntry :: Int
nullCacheEntry = cacheEntry (refNull :: BlobRef ())

-- |First-in, first-out cache, with entries keyed by 'BlobRefs's.
-- 'refNull' is considered an invalid key, and should not be inserted in the cache.
newtype FIFOCache v = FIFOCache {theFIFOCache :: MVar (FIFOCache' v)}

instance Cache (FIFOCache v) where
    type CacheKey (FIFOCache v) = BlobRef v
    type CacheValue (FIFOCache v) = v

    newCache = newFIFOCache
    collapseCache _ = do
        FIFOCache cacheRef <- getCache
        liftIO $ do
            (cache :: FIFOCache' v) <- emptyFIFOCache' 0
            void $ swapMVar cacheRef $! cache
    putCachedValue _ key val = do
        let intKey = cacheEntry key
        FIFOCache cacheRef <- getCache
        liftIO $! do
            cache <- takeMVar cacheRef
            case IntMap.lookup intKey (keyMap cache) of
                Nothing -> do
                    oldEntry <- Vec.read (fifoBuffer cache) (nextIndex cache)
                    let newKeyMap =
                            IntMap.insert intKey val $!
                                if oldEntry == nullCacheEntry
                                    then keyMap cache
                                    else IntMap.delete oldEntry (keyMap cache)
                    Vec.write (fifoBuffer cache) (nextIndex cache) $! intKey
                    putMVar cacheRef $!
                        cache
                            { keyMap = newKeyMap,
                              nextIndex = (nextIndex cache + 1) `mod` Vec.length (fifoBuffer cache)
                            }
                    return val
                Just _ -> do
                    let newKeyMap = IntMap.insert intKey val (keyMap cache)
                    putMVar cacheRef $! cache{keyMap = newKeyMap}
                    return val

    lookupCachedValue _ key = do
        FIFOCache cacheRef <- getCache
        -- This should be OK as we are just accessing the keyMap, so we can read from a snapshot.
        -- We need to be sure not to retain references after we are done.
        cache <- liftIO $! readMVar cacheRef
        return $! IntMap.lookup (cacheEntry key) (keyMap cache)

    getCacheSize _ = do
        FIFOCache cacheRef :: FIFOCache v <- getCache
        cache <- liftIO $! readMVar cacheRef
        return $! IntMap.size (keyMap cache)

-- |An empty 'FIFOCache'' of at least the specified size.
emptyFIFOCache' :: Int -> IO (FIFOCache' v)
emptyFIFOCache' size' = do
    let size = max 1 size'
    fifoBuffer <- Vec.replicate size nullCacheEntry
    return
        FIFOCache'
            { keyMap = IntMap.empty,
              fifoBuffer = fifoBuffer,
              nextIndex = 0,
              ..
            }

-- |Construct a FIFO cache of at least the specified size.
-- If the size is less than 1, a cache of size 1 will be created instead.
newFIFOCache :: Int -> IO (FIFOCache v)
newFIFOCache size = do
    cache <- emptyFIFOCache' size
    FIFOCache <$> newMVar cache
