{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module GlobalStateTests.Cache where

import Concordium.GlobalState.Persistent.BlobStore (BlobRef (BlobRef))
import Concordium.GlobalState.Persistent.Cache
import Control.Monad.Reader
import Data.Proxy
import Test.Hspec

-- This file contains tests relating to the caching.

tests :: Spec
tests = do
  describe "Testing LRUCache: " $ do
    parallel $ do
      specify "Item should be retrieved from LRUCache" $ do
        cache :: LRUCache String <- newLRUCache 1
        let cacheProxy = Proxy :: Proxy (LRUCache String)
        flip runReaderT cache $ do
          -- Put an item in the cache
          _ <- putCachedValue cacheProxy (BlobRef 1) "test"
          val <- lookupCachedValue cacheProxy (BlobRef 1)
          lift $ val `shouldBe` Just "test"
          -- Check that the cache size is correct
          s <- getCacheSize cacheProxy
          lift $ s `shouldBe` 1

      specify "Item should be overwritten in the LRUCache" $ do
        cache :: LRUCache String <- newLRUCache 3
        let cacheProxy = Proxy :: Proxy (LRUCache String)
        let key = BlobRef 1
        flip runReaderT cache $ do
          -- Put an item in the cache
          _ <- putCachedValue cacheProxy key "foo"
          val <- lookupCachedValue cacheProxy key
          -- Check that the item is in the cache
          lift $ val `shouldBe` Just "foo"
          -- Overwrite the item in the cache
          _ <- putCachedValue cacheProxy key "bar"
          val2 <- lookupCachedValue cacheProxy key
          -- Check that the new item is in the cache
          lift $ val2 `shouldBe` Just "bar"
          -- Check that the cache size did not change
          s <- getCacheSize cacheProxy
          lift $ s `shouldBe` 1

      specify "Least recently used item should be removed from the LRUCache" $ do
        cache :: LRUCache String <- newLRUCache 3
        let cacheProxy = Proxy :: Proxy (LRUCache String)
        let key1 = BlobRef 1
        let key2 = BlobRef 2
        let key3 = BlobRef 3
        let key4 = BlobRef 4
        -- Fill the cache
        flip runReaderT cache $ do
          _ <- putCachedValue cacheProxy key1 "val1"
          _ <- putCachedValue cacheProxy key2 "val2"
          _ <- putCachedValue cacheProxy key3 "val3"
          -- Access 2 of the items and leave the other as least recently used
          _ <- lookupCachedValue cacheProxy key1
          _ <- lookupCachedValue cacheProxy key2
          -- Add a new item
          _ <- putCachedValue cacheProxy key4 "val4"
          -- Check that the least recently used item was removed
          val3 <- lookupCachedValue cacheProxy key3
          lift $ val3 `shouldBe` Nothing
          -- Check that the other items are still there
          val1 <- lookupCachedValue cacheProxy key1
          lift $ val1 `shouldBe` Just "val1"
          val2 <- lookupCachedValue cacheProxy key2
          lift $ val2 `shouldBe` Just "val2"
          val4 <- lookupCachedValue cacheProxy key4
          lift $ val4 `shouldBe` Just "val4"
          -- Check that the cache size did not change
          s <- getCacheSize cacheProxy
          lift $ s `shouldBe` 3

  describe "Testing FIFOCache: " $ do
    parallel $ do
      specify "Item should be retrieved from the FIFOCache" $ do
        cache :: FIFOCache String <- newFIFOCache 1
        let cacheProxy = Proxy :: Proxy (FIFOCache String)
        flip runReaderT cache $ do
          -- Put an item in the cache
          let key = BlobRef 1
          _ <- putCachedValue cacheProxy key "test"
          val <- lookupCachedValue cacheProxy key
          lift $ val `shouldBe` Just "test"
          -- Check that the cache size is correct
          s <- getCacheSize cacheProxy
          lift $ s `shouldBe` 1

      specify "Item should be overwritten in the FIFOCache" $ do
        cache :: FIFOCache String <- newFIFOCache 3
        let cacheProxy = Proxy :: Proxy (FIFOCache String)
        let key = BlobRef 1
        flip runReaderT cache $ do
          -- Put an item in the cache
          _ <- putCachedValue cacheProxy key "foo"
          val <- lookupCachedValue cacheProxy key
          -- Check that the item is in the cache
          lift $ val `shouldBe` Just "foo"
          -- Overwrite the item in the cache
          _ <- putCachedValue cacheProxy key "bar"
          val2 <- lookupCachedValue cacheProxy key
          -- Check that the new item is in the cache
          lift $ val2 `shouldBe` Just "bar"
          -- Check that the cache size did not change
          s <- getCacheSize cacheProxy
          lift $ s `shouldBe` 1

      specify "The oldest item should be removed from the FIFOCache" $ do
        cache :: FIFOCache String <- newFIFOCache 3
        let cacheProxy = Proxy :: Proxy (FIFOCache String)
        let key1 = BlobRef 1
        let key2 = BlobRef 2
        let key3 = BlobRef 3
        let key4 = BlobRef 4
        -- Fill the cache
        flip runReaderT cache $ do
          -- Fill the cache
          _ <- putCachedValue cacheProxy key1 "val1"
          _ <- putCachedValue cacheProxy key2 "val2"
          _ <- putCachedValue cacheProxy key3 "val3"
          -- Add a new item
          _ <- putCachedValue cacheProxy key4 "val4"
          -- Check that the oldest item was removed
          val3 <- lookupCachedValue cacheProxy key1
          lift $ val3 `shouldBe` Nothing
          -- Check that the other items are still there
          val2 <- lookupCachedValue cacheProxy key2
          lift $ val2 `shouldBe` Just "val2"
          val1 <- lookupCachedValue cacheProxy key3
          lift $ val1 `shouldBe` Just "val3"
          val4 <- lookupCachedValue cacheProxy key4
          lift $ val4 `shouldBe` Just "val4"
          -- Check that the cache size did not change
          s <- getCacheSize cacheProxy
          lift $ s `shouldBe` 3
