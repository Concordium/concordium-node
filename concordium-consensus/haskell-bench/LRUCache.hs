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

data CacheOp k v
  = PutItem k v
  | GetItem k

testLRUCache :: LRUCache String -> [CacheKey (LRUCache String)] -> IO ()
testLRUCache cache keys = do
  -- cache <- newLRUCache cacheSize :: IO (LRUCache String)
  let cacheProxy = Proxy :: Proxy (LRUCache String)
  flip runReaderT cache $ do
    -- mapM_ (\key -> putCachedValue cacheProxy key "test") keys
    -- res <- mapM (lookupCachedValue cacheProxy) keys
    res <-
      forM
        (take 12000 keys)
        ( \key -> do
            r <- lookupCachedValue cacheProxy key
            when (isNothing r) $ do
              liftIO $ threadDelay 500
              putCachedValue cacheProxy key "test"
              return ()
            return r
        )
    -- liftIO $ print $ length $ filter isNothing res
    return ()

  return ()

testFIFOCache :: FIFOCache String -> [CacheKey (FIFOCache String)] -> IO ()
testFIFOCache cache keys = do
  -- cache <- newFIFOCache cacheSize :: IO (FIFOCache String)
  let cacheProxy = Proxy :: Proxy (FIFOCache String)
  flip runReaderT cache $ do
    -- mapM_ (\key -> putCachedValue cacheProxy key "test") keys
    res <-
      forM
        (take 12000 keys)
        ( \key -> do
            r <- lookupCachedValue cacheProxy key
            when (isNothing r) $ do
              liftIO $ threadDelay 500
              putCachedValue cacheProxy key "test"
              return ()
            return r
        )
    -- liftIO $ print $ length $ filter isNothing res
    return ()

  return ()

setupEnv :: IO (IO (LRUCache String), IO (FIFOCache String), [BlobRef String])
setupEnv = do
  g <- getStdGen
  mwc <- create
  let transactions = 500000 -- corresponds to 110K unique accounts
  let mean = fromIntegral transactions
  let deviation = fromIntegral transactions / 10 / 2
  putStrLn $ show mean ++ "-" ++ show deviation
  xs <- replicateM transactions (normal mean deviation mwc :: IO Double)
  let keys = map (BlobRef . round) xs
  return (lruCache keys, fifoCache keys, keys)
  where
    cacheSize = 30000
    lruCache keys = do
      cache <- newLRUCache cacheSize :: IO (LRUCache String)
      let lruCacheProxy = Proxy :: Proxy (LRUCache String)
      flip runReaderT cache $ do
        mapM_ (\key -> putCachedValue lruCacheProxy key "test") keys
      return cache
    fifoCache keys = do
      cache <- newFIFOCache cacheSize :: IO (FIFOCache String)
      let fifoCacheProxy = Proxy :: Proxy (FIFOCache String)
      flip runReaderT cache $ do
        mapM_ (\key -> putCachedValue fifoCacheProxy key "test") keys
      return cache

main :: IO ()
main = do
  -- print $ length $ nub keys
  defaultMain [
    env setupEnv $ \ ~(lruCache, fifoCache, keys) -> bgroup "cache" [
      bench "test LRUCache" $ nfIO $ lruCache >>= flip testLRUCache keys,
      bench "test FIFOCache" $ nfIO $ fifoCache >>= flip testFIFOCache keys
      ]
      ]

