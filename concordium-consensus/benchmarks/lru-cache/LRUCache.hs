{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides a benchmark for the comparison of two different
-- | implementations of the cache, namely `FIFOCache` and `LRUCache`.
-- | The benchmark is based on the following assumption:
-- | Most of the transactions in the network are done by a small number of
-- | accounts. To simulate this, we use normal distribution to create the transactions
-- | and the accounts. They are just integer values. Before each run of the benchmark,
-- | we create a new cache and fill it with some transactions. Then, we test the cache
-- | with another set of transactions/accounts.
module Main where

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cache

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

testCache :: forall v. (Cache v) => v -> [CacheKey v] -> CacheValue v -> Int -> IO ()
testCache cache keys value txCount = do
  let cacheProxy = Proxy :: Proxy v
  flip runBlobStoreT (CacheContext cache) $ do
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
  let txs = map (BlobRef . round) xs
  defaultMain [
    bgroup "cache" [
      bench "test LRUCache" $ perRunEnv (lruCache txs) $ \ lruCache -> testCache lruCache txs "test" 6000,
      bench "test FIFOCache" $ perRunEnv (fifoCache txs) $ \ fifoCache -> testCache fifoCache txs "test" 6000
      ]
      ]
  where
    cacheSize = 30000
    lruCache txs = do
      cache <- newLRUCache cacheSize
      let lruCacheProxy = Proxy :: Proxy (LRUCache String)
      flip runBlobStoreT (CacheContext cache) $ do
        mapM_ (\key -> putCachedValue lruCacheProxy key "test") (take (cacheSize*1) txs)
      return cache
    fifoCache txs = do
      cache <- newFIFOCache cacheSize
      let fifoCacheProxy = Proxy :: Proxy (FIFOCache String)
      flip runBlobStoreT (CacheContext cache) $ do
        mapM_ (\key -> putCachedValue fifoCacheProxy key "test") (take (cacheSize*1) txs)
      return cache

instance NFData (LRUCache v) where
  rnf bs = bs `seq` ()

instance NFData (BlobRef v) where
  rnf bs = bs `seq` ()

instance NFData (FIFOCache v) where
  rnf bs = bs `seq` ()