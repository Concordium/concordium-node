{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cache
-- import Data.Random

import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Criterion.Main
import Data.List
import Data.Maybe
import Data.Proxy
import System.Random
import System.Random.MWC
import System.Random.MWC.Distributions
import Control.DeepSeq (NFData (..))
import GHC.Generics

data CacheOp k v
  = PutItem k v
  | GetItem k

testCache :: forall v. (Cache v, HasCache v v) => v -> [CacheKey v] -> CacheValue v -> Int -> IO ()
testCache cache keys value txCount = do
  let cacheProxy = Proxy :: Proxy v
  flip runReaderT cache $ do
    res <-
      forM
        (take txCount (reverse keys))
        ( \key -> do
            r <- lookupCachedValue cacheProxy key
            when (isNothing r) $ do
              liftIO $ threadDelay 500
              putCachedValue cacheProxy key value
              return ()
            return r
        )
    -- liftIO $ print $ length $ filter isNothing res
    return ()

  return ()

main :: IO ()
main = do
  g <- getStdGen
  mwc <- create
  let txCount = 500000 -- corresponds to 110K unique accounts
  let mean = fromIntegral txCount
  let deviation = fromIntegral txCount / 10 / 2
  xs <- replicateM txCount (normal mean deviation mwc :: IO Double)
  let keys = map (BlobRef . round) xs
  defaultMain [
    bgroup "cache" [
      bench "test LRUCache" $ perRunEnv (lruCache keys) $ \ lruCache -> testCache lruCache keys "test" 6000,
      bench "test FIFOCache" $ perRunEnv (fifoCache keys) $ \ fifoCache -> testCache fifoCache keys "test" 6000
      ]
      ]
  where
    cacheSize = 30000
    lruCache keys = do
      cache <- newLRUCache cacheSize
      let lruCacheProxy = Proxy :: Proxy (LRUCache String)
      flip runReaderT cache $ do
        mapM_ (\key -> putCachedValue lruCacheProxy key "test") (take (cacheSize*2) keys)
      return cache
    fifoCache keys = do
      cache <- newFIFOCache cacheSize
      let fifoCacheProxy = Proxy :: Proxy (FIFOCache String)
      flip runReaderT cache $ do
        mapM_ (\key -> putCachedValue fifoCacheProxy key "test") (take (cacheSize*2) keys)
      return cache

instance NFData (LRUCache v) where
  rnf bs = bs `seq` ()

instance NFData (BlobRef v) where
  rnf bs = bs `seq` ()

instance NFData (FIFOCache v) where
  rnf bs = bs `seq` ()