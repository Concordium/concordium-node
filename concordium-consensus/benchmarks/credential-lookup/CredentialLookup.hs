{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |A benchmark for credential lookup. This compares three approaches:
--
-- 1. A map stored in memory.
--
-- 2. A trie that is cached in the memory.
--
-- 3. A trie that is not cached in the memory and always accessed from the disk.
module Main where

import Control.DeepSeq
import Control.Monad
import Criterion
import Criterion.Main
import qualified Data.Map.Strict as Map
import Data.Maybe
import System.IO.Temp
import System.Random

import Concordium.ID.Types
import Concordium.Types
import qualified Data.FixedByteString as FBS

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie

-- |The starting state from which the benchmarks are run.
-- This consists of the blob store, and indexes in the three forms required for testing.
data Setup = Setup
    { setupBlobStore :: !BlobStore,
      setupMemMap :: !(Map.Map RawCredentialRegistrationID AccountIndex),
      setupBufferedTrie :: !(Trie.TrieN BufferedFix RawCredentialRegistrationID AccountIndex),
      setupUnbufferedTrie :: !(Trie.TrieN UnbufferedFix RawCredentialRegistrationID AccountIndex)
    }

instance NFData Setup where
    rnf Setup{..} = rnf setupMemMap

instance NFData RawCredentialRegistrationID where
    rnf a = a `seq` ()

instance NFData AccountIndex where
    rnf (AccountIndex x) = rnf x

-- |Generate a pseudo-randoming registration ID given a seed.
genRegID :: Int -> RawCredentialRegistrationID
genRegID seed = RawCredentialRegistrationID $! FBS.pack (randoms (mkStdGen seed))

-- |Create a benchmark environment by populating the map and tries with the given number of
-- generated credentials. The trie is written out after each insertion. This decreases the locality
-- of the trie in the blob store and gives a more realistic scenario than if the trie is only
-- written a single time.
setup :: Int -> IO Setup
setup size = do
    tempBlobStoreFile <- emptySystemTempFile "blb.dat"
    setupBlobStore <- loadBlobStore tempBlobStoreFile
    flip runBlobStoreM setupBlobStore $ do
        (setupMemMap, bt) <- foldM add (Map.empty, Trie.empty) toInsert
        setupBufferedTrie <- snd <$> storeUpdate bt
        setupUnbufferedTrie <- case setupBufferedTrie of
            Trie.EmptyTrieN -> return Trie.EmptyTrieN
            Trie.TrieN sz (BufferedFix br) -> do
                (_, blobRef) <- refFlush br
                return $! Trie.TrieN sz (UnbufferedFix (blobRefToUnbufferedRef (BlobRef (theBlobRef blobRef))))
        return $! Setup{..}
  where
    toInsert = [(genRegID x, fromIntegral x) | x <- [1 .. size]]
    add (m, t) (regid, acctix) = do
        (_, t') <- storeUpdate =<< Trie.insert regid acctix t
        return (Map.insert regid acctix m, t')

cleanup :: Setup -> IO ()
cleanup = destroyBlobStore . setupBlobStore

-- |Benchmark looking up specific registration IDs.
benches :: Int -> Benchmark
benches size = envWithCleanup (setup size) cleanup $ \cnf ->
    let testRegID x = env (pure (genRegID x)) $ \regid ->
            bgroup
                ("Cred" ++ show x)
                [ bench "map" $
                    whnf
                        (\(c, r) -> Map.lookup r (setupMemMap c) == xres)
                        (cnf, regid),
                  bench "mem-trie" $
                    whnfIO
                        ( do
                            res <- runBlobStoreM (Trie.lookup regid (setupBufferedTrie cnf)) (setupBlobStore cnf)
                            return (res == xres)
                        ),
                  bench "disk-trie" $
                    whnfIO
                        ( do
                            res <- runBlobStoreM (Trie.lookup regid (setupUnbufferedTrie cnf)) (setupBlobStore cnf)
                            return (res == xres)
                        )
                ]
          where
            !xres = if 1 <= x && x <= size then Just $! fromIntegral x else Nothing
    in  bgroup
            ("Map size " ++ show size)
            [ testRegID 1,
              testRegID size,
              testRegID (-1)
            ]

-- |Benchmark looking up random registration IDs. There should be a 50% chance of each occurring
-- in the map.
benchesRandom :: Int -> Benchmark
benchesRandom size = envWithCleanup (setup size) cleanup $ \cnf ->
    bgroup
        ("Map size " ++ show size)
        [ bench "map" $
            perRunEnv
                (genRegID <$> randomRIO (1, 2 * size))
                ( \regid -> do
                    let res = Map.lookup regid (setupMemMap cnf)
                    return $ isJust res
                ),
          bench "mem-trie" $
            perRunEnv
                (genRegID <$> randomRIO (1, 2 * size))
                ( \regid -> do
                    res <- runBlobStoreM (Trie.lookup regid (setupBufferedTrie cnf)) (setupBlobStore cnf)
                    return $ isJust res
                ),
          bench "disk-trie" $
            perRunEnv
                (genRegID <$> randomRIO (1, 2 * size))
                ( \regid -> do
                    res <- runBlobStoreM (Trie.lookup regid (setupUnbufferedTrie cnf)) (setupBlobStore cnf)
                    return $ isJust res
                )
        ]

main :: IO ()
main = defaultMain [benchesRandom 100000, benchesRandom 1000000]
