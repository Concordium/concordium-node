{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.GlobalState.Persistent.Cache where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Cache.LRU.IO as LRU
import qualified Data.IntMap.Strict as IntMap
import Data.Proxy
import qualified Data.Vector.Mutable as Vec
import Data.Word (Word64)

import Concordium.GlobalState.Persistent.BlobStore (BlobRef (..))
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except (ExceptT)
import Concordium.Utils.Serialization.Put (PutT)

class HasCache cache r where
  projectCache :: r -> cache

class MonadIO m => MonadCache c m where
  getCache :: m c

-- TODO: These instances are sketchy.
instance MonadCache c m => MonadCache c (StateT s m) where
  getCache = lift getCache

instance (Monoid w, MonadCache c m) => MonadCache c (WriterT w m) where
  getCache = lift getCache

-- TODO: Figure out whether this makes sense with respect to rollbacks.
instance (MonadCache c m) => MonadCache c (ExceptT e m) where
  getCache = lift getCache

instance (MonadIO m, HasCache c r) => MonadCache c (ReaderT r m) where
  getCache = asks projectCache

instance (MonadCache c m) => MonadCache c (PutT m) where
  getCache = lift getCache

--| A cache that stores accounts in memory.
class Cache c where
    type CacheKey c
    type CacheValue c

    putCachedValue :: (MonadCache c m) => Proxy c -> CacheKey c -> CacheValue c -> m (CacheValue c)
    lookupCachedValue :: (MonadCache c m) => Proxy c -> CacheKey c -> m (Maybe (CacheValue c))
    getCacheSize :: (MonadCache c m) => Proxy c -> m Int

--| A context that simply wraps a cache, providing an instance @HasCache c (CacheContext c)@.
newtype CacheContext c = CacheContext { theCacheContext :: c }

instance HasCache c (CacheContext c) where
    projectCache = theCacheContext

data DummyCache k v = DummyCache

instance Cache (DummyCache k v) where
    type CacheKey (DummyCache k v) = k
    type CacheValue (DummyCache k v) = v

    putCachedValue _ _ = return
    lookupCachedValue _ _ = return Nothing
    getCacheSize _ = return 0

-- |First-in, first-out cache, with entries keyed by 'Int's.
data FIFOCache' v = FIFOCache'
    { -- |Map from keys to values that are stored in the cache.
      -- Each entry in the map should have a corresponding entry in the 'fifoBuffer' vector.
      keyMap :: !(IntMap.IntMap v),
      -- |Vector of cached entries. Each (non-'Empty') entry must correspond to an entry
      -- in the 'keyMap'.
      fifoBuffer :: !(Vec.IOVector CacheEntry),
      -- | The next index to use, 0 <= nextIndex < Vec.length fifoBuffer
      nextIndex :: !Int
    }

data CacheEntry
    = Empty
    | CacheEntry {key :: !Int}

newtype FIFOCache v = FIFOCache {theFIFOCache :: MVar (FIFOCache' v)}

instance Cache (FIFOCache v) where
    type CacheKey (FIFOCache v) = BlobRef v
    type CacheValue (FIFOCache v) = v

    putCachedValue _ key val = do
        let intKey = fromIntegral (theBlobRef key)
        FIFOCache cacheRef <- getCache
        liftIO $ do
            cache <- takeMVar cacheRef
            case IntMap.lookup intKey (keyMap cache) of
                Nothing -> do
                    oldEntry <- Vec.read (fifoBuffer cache) (nextIndex cache)
                    let newKeyMap = IntMap.insert intKey val $
                            case oldEntry of
                                Empty -> keyMap cache
                                CacheEntry oldKey -> IntMap.delete oldKey (keyMap cache)
                    Vec.write (fifoBuffer cache) (nextIndex cache) $ CacheEntry intKey
                    putMVar cacheRef $
                        cache
                            { keyMap = newKeyMap,
                              nextIndex = (nextIndex cache + 1) `mod` Vec.length (fifoBuffer cache)
                            }
                    return val
                Just _ -> do
                    let newKeyMap = IntMap.insert intKey val (keyMap cache)
                    putMVar cacheRef $ cache { keyMap = newKeyMap }
                    return val

    lookupCachedValue _ key = do
        FIFOCache cacheRef <- getCache
        -- This should be OK as we are just accessing the keyMap, so we can read from a snapshot.
        -- We need to be sure not to retain references after we are done.
        cache <- liftIO $ readMVar cacheRef
        return $! IntMap.lookup (fromIntegral (theBlobRef key)) (keyMap cache)

    getCacheSize _ = do
        FIFOCache cacheRef :: FIFOCache v <- getCache
        cache <- liftIO $ readMVar cacheRef
        return $ IntMap.size (keyMap cache)

newFIFOCache :: Int -> IO (FIFOCache v)
newFIFOCache size = do
    fifoBuffer <- Vec.replicate size Empty
    let cache =
            FIFOCache'
                { keyMap = IntMap.empty,
                  fifoBuffer = fifoBuffer,
                  nextIndex = 0
                }
    FIFOCache <$> newMVar cache

-- | An LRU cache that stores values in memory.
newtype LRUCache v = LRUCache { theLRUCache :: LRU.AtomicLRU Word64 v }

instance Cache (LRUCache v) where
    type CacheKey (LRUCache v) = BlobRef v
    type CacheValue (LRUCache v) = v

    putCachedValue _ k v = do
        lru <- theLRUCache <$> getCache
        liftIO $ LRU.insert (theBlobRef k) v lru
        return v

    lookupCachedValue _ k = do
        lru <- theLRUCache <$> getCache
        liftIO $ LRU.lookup (theBlobRef k) lru

    getCacheSize _ = do
        lru :: LRUCache v <- getCache
        liftIO $ LRU.size $ theLRUCache lru


newLRUCache :: Int -> IO (LRUCache v)
newLRUCache size = LRUCache <$> LRU.newAtomicLRU (Just $ toInteger size)
