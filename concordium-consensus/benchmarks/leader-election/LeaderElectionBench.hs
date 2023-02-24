{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A simple benchmark for the leader election implementation.
-- This measures the time it takes to run 'getLeader' for 1000 different round numbers for
-- different numbers of bakers. (It is important to run for different round numbers as the time
-- varies based on the inputs.)
-- The purpose is to determine if the performance of the implementation is acceptable, given that
-- it is linear in the number of bakers.
module Main where

import Control.DeepSeq
import Criterion
import Criterion.Main

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types

import Concordium.KonsensusV1.LeaderElection

deriving instance NFData Amount
instance NFData Hash.Hash where
    rnf a = a `seq` ()

main :: IO ()
main = defaultMain [bgroup "leadership election" (benchLE <$> [1, 10, 100, 1000, 10000, 100000])]

benchLE :: Int -> Benchmark
benchLE n =
    bench (show n ++ " bakers, 1000 iterations") $
        bakers `deepseq`
            nonce `deepseq`
                -- We do 1000 iterations with different numbers of bakers to get a broader average measurement.
                whnf (sum . fmap (getLeader bakers nonce)) [1 .. 1000]
  where
    bakers = [(i, 1000000000000) | i <- [1 .. n]]
    nonce = Hash.hash "blah"
