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

testAccountMap :: AM.PureAccountMap 'P4
testAccountMap = foldr (uncurry AM.insertPure) AM.empty (accounts 100000)

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
        [ env (pure testAccountMap) $ \am0 ->
            bgroup
                "lookup"
                [ env (pure (testAccountAddress 0)) $ \addr0 ->
                    bench "Account0" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 0) (am0, addr0),
                  env (pure (testAccountAddress 1234)) $ \addr0 ->
                    bench "Account1234" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 1234) (am0, addr0),
                  env (pure (testAccountAddress 7)) $ \addr0 ->
                    bench "Account7" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 7) (am0, addr0),
                  env (pure (testAccountAddress 8)) $ \addr0 ->
                    bench "Account8" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 8) (am0, addr0),
                  env (pure (testAccountAddress 9)) $ \addr0 ->
                    bench "Account9" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 9) (am0, addr0),
                  env (pure (testAccountAddress 10)) $ \addr0 ->
                    bench "Account10" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 10) (am0, addr0),
                  env (pure (testAccountAddress (-2))) $ \addr0 ->
                    bench "Account-2" $
                        whnf (\(am, addr) -> isNothing (AM.lookupPure addr am)) (am0, addr0)
                ],
          benchPersistent 100000,
          benchPersistent 200000,
          benchPersistent 400000,
          benchPersistent 800000,
          benchPersistent 1600000
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
