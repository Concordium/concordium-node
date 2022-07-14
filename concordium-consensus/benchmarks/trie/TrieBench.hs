{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A simple benchmark for the Trie implementation.
-- This benchmark tests the performance of lookups in the AccountMap, which uses the Trie.
-- It constructs an AccountMap with many addresses and benchmarks the time to look up specific
-- addresses in the map.
module Main where

import Control.DeepSeq
import Control.Monad.Reader
import Criterion
import Criterion.Main
import Data.Maybe
import System.IO.Temp
import System.Random

import Concordium.ID.Types
import Concordium.Types

import qualified Concordium.GlobalState.AccountMap as AM
import Concordium.GlobalState.Persistent.BlobStore

testAccountAddress :: Int -> AccountAddress
testAccountAddress = fst . randomAccountAddress . mkStdGen

accounts :: Int -> [(AccountAddress, AccountIndex)]
accounts n = [(testAccountAddress i, fromIntegral i) | i <- [0 .. n - 1]]

testAccountMap :: Int -> AM.PureAccountMap 'P4
testAccountMap n = foldr (uncurry AM.insertPure) AM.empty (accounts n)

instance NFData (AM.AccountMap pv fix) where
    rnf a = seq a ()

instance NFData AccountAddress where
    rnf a = seq a ()

instance NFData BlobStore where
    rnf a = seq a ()

testPersistentAccountMap :: Int -> IO (BlobStore, AM.PersistentAccountMap 'P4)
testPersistentAccountMap n = do
    tempBlobStoreFile <- emptySystemTempFile "blb.dat"
    bs <- loadBlobStore tempBlobStoreFile
    !pam <- flip runReaderT bs $ do
        pam0 <- foldM (flip $ uncurry AM.insert) AM.empty (accounts n)
        snd <$> storeUpdate pam0
    return (bs, pam)

cleanupPersistent :: (BlobStore, AM.PersistentAccountMap 'P4) -> IO ()
cleanupPersistent = destroyBlobStore . fst

main :: IO ()
main =
    defaultMain
        [ benchPassive 100000,
          benchPersistent 100000,
          benchPersistent 200000,
          benchPersistent 400000,
          benchPersistent 800000,
          benchPersistent 1600000
        ]

benchPassive :: Int -> Benchmark
benchPassive n = env (pure (testAccountMap n)) $ \am0 ->
    let testAccount x xres = env (pure (testAccountAddress x)) $ \addr0 ->
            bench ("Account" ++ show x) $
                whnf (\(am, addr) -> AM.lookupPure addr am == xres) (am0, addr0)
     in bgroup
            "lookup"
            [ testAccount 0 (Just 0),
              testAccount 1234 (Just 1234),
              testAccount 7 (Just 7),
              testAccount 8 (Just 8),
              testAccount 8 (Just 9),
              testAccount 10 (Just 10),
              testAccount (-2) Nothing
            ]

benchPersistent :: Int -> Benchmark
benchPersistent n = envWithCleanup (testPersistentAccountMap n) cleanupPersistent $ \ ~(bs, pam) ->
    let testAccount x xres = env (pure (testAccountAddress x)) $ \addr ->
            bench ("Account" ++ show x) $
                whnfIO $ do
                    res <- runReaderT (AM.lookup addr pam) bs
                    return $! res == xres
     in bgroup
            ("lookupPersistent" ++ show n)
            [ testAccount 0 (Just 0),
              testAccount 10 (Just 10),
              testAccount (-2) Nothing
            ]
