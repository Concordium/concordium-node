{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.Cache where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.Vector.Mutable as Vec

class HasCache c r where
  getCache :: r -> c

type MonadCache r c m = (MonadIO m, MonadReader r m, HasCache c r)

class Cache c where
  data CacheKey c
  data CacheValue c

  putCachedValue :: (MonadCache r c m) => CacheKey c -> CacheValue c -> m ()
  lookupCachedValue :: (MonadCache r c m) => CacheKey c -> m (Maybe (CacheValue c))

data DummyCache k v = DummyCache

instance Cache (DummyCache k v) where
  newtype CacheKey (DummyCache k v) = DummyCacheKey k
  newtype CacheValue (DummyCache k v) = DummyCacheValue v

  putCachedValue _ _ = return ()
  lookupCachedValue _ = return Nothing

data FIFOCache' v = FIFOCache'
  { keyMap :: !(IntMap.IntMap v),
    fifoCache :: !(Vec.IOVector (CacheEntry v)),
    -- | The next index to use, 0 <= nextIndex < Vec.length fifoCache
    nextIndex :: !Int
  }

data CacheEntry v
  = Empty
  | CacheEntry {key :: !Int, value :: !v}

newtype FIFOCache v = FIFOCache {theFIFOCache :: IORef (FIFOCache' v)}

instance Cache (FIFOCache v) where
  newtype CacheKey (FIFOCache v) = FIFOCacheKey Int
  newtype CacheValue (FIFOCache v) = FIFOCacheValue v

  putCachedValue (FIFOCacheKey k) (FIFOCacheValue v) = do
    FIFOCache cacheRef <- ask
    cache <- liftIO $ readIORef cacheRef
    oldEntry <- Vec.read (fifoCache cache) (nextIndex cache)
    let newKeyMap = IntMap.insert k v $
          case oldEntry of
            Empty -> keyMap cache
            CacheEntry _ _ -> IntMap.delete (key oldEntry) (keyMap cache)

    Vec.write (fifoCache cache) (nextIndex cache) $ CacheEntry k v
    liftIO $ writeIORef cacheRef $ cache {keyMap = newKeyMap, nextIndex = (nextIndex cache + 1) `mod` Vec.length (fifoCache cache)}

  lookupCachedValue (FIFOCacheKey k) = do
    FIFOCache cacheRef <- ask
    cache <- liftIO $ readIORef cacheRef
    return $ IntMap.lookup k (keyMap cache)
