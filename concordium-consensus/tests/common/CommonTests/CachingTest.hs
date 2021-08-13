{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE OverloadedStrings #-}
module CommonTests.CachingTest where

import qualified Concordium.Caching as C

import Test.Hspec

tests :: Spec
tests = do
  describe "Testing Caching" $ do
    parallel $ do
      specify "Can fetch an inserted entry" $ do
        let cache = C.empty :: C.Cache String String
            cache' = C.insert "key" "value" cache
            lookupInFirstCache = C.lookup "key" cache
            lookupInSecondCache = C.lookup "key" cache'    
        lookupInFirstCache `shouldBe` Nothing
        lookupInSecondCache `shouldBe` Just "value"

      specify "Duplicate insertion overrides entries" $ do
        let cache = C.empty :: C.Cache String String
            cache' = C.insert "key" "value" cache
            cache'' = C.insert "key" "overriddenValue" cache'
            overriddenValue = C.lookup "key" cache''
        overriddenValue `shouldBe` Just "overriddenValue"

      specify "Can delete an inserted entry" $ do
        let cache = C.empty :: C.Cache String String
            cache' = C.insert "key" "value" cache
            cache'' = C.delete "key" cache'
            lookupOnDeletedEntry = C.lookup "key" cache''
        lookupOnDeletedEntry `shouldBe` Nothing
        
  describe "Testing Capped Caching" $ do
    parallel $ do
      specify "Size of capped cache does not exceed capacity" $ do
        let cache = C.emptyCapped 1 :: C.Cache String String
            cache' = C.insert "key0" "value0" cache
            cache'' = C.insert "key1" "value1" cache'            
        C.size cache'' `shouldBe` 1
