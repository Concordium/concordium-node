{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.Cache where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.IntMap.Strict as IntMap
import Data.Proxy
import qualified Data.Vector.Mutable as Vec

import Concordium.GlobalState.Persistent.BlobStore (BlobRef (..))
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except (ExceptT)
import Concordium.Utils.Serialization.Put (PutT)

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

instance (MonadCache c m) => MonadCache c (PutT m) where
  getCache = lift getCache

class Cache c where
    type CacheKey c
    type CacheValue c

    putCachedValue :: (MonadCache c m) => Proxy c -> CacheKey c -> CacheValue c -> m (CacheValue c)
    lookupCachedValue :: (MonadCache c m) => Proxy c -> CacheKey c -> m (Maybe (CacheValue c))

data DummyCache k v = DummyCache

instance Cache (DummyCache k v) where
    type CacheKey (DummyCache k v) = k
    type CacheValue (DummyCache k v) = v

    putCachedValue _ _ = return
    lookupCachedValue _ _ = return Nothing

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
                Just val' -> do
                    putMVar cacheRef cache
                    return val'

    lookupCachedValue _ key = do
        FIFOCache cacheRef <- getCache
        -- This should be OK as we are just accessing the keyMap, so we can read from a snapshot.
        -- We need to be sure not to retain references after we are done.
        cache <- liftIO $ readMVar cacheRef
        return $! IntMap.lookup (fromIntegral (theBlobRef key)) (keyMap cache)

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
